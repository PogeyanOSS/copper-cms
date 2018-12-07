/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.impl.utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.DateTimeHelper;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyBooleanImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDateTimeImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDecimalImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyHtmlImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIntegerImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyUriImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.TracingMessage;
import com.pogeyan.cmis.impl.services.CmisTypeServices;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class CmisPropertyConverter {
	private static final Logger LOG = LoggerFactory.getLogger(CmisPropertyConverter.class);

	public static class Impl {

		public static Properties createNewProperties(Map<String, List<String>> propertiesdata, String repositoryId,
				IUserObject userObject, String tracingId, ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisPropertyConverter::createNewProperties", null);
			List<TypeDefinition> secondaryTypes = new ArrayList<>();
			List<TypeDefinition> objectType = new ArrayList<>();
			Map<String, List<String>> properties = propertiesdata;
			if (properties == null) {
				return null;
			}

			// load primary type
			List<String> objectTypeIdsValues = properties.get(PropertyIds.OBJECT_TYPE_ID);
			if (isNotEmpty(objectTypeIdsValues)) {
				TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId,
						objectTypeIdsValues.get(0), null, userObject, tracingId, span);
				objectType.add(typeDef);
				if (typeDef == null) {
					TracingApiServiceFactory.getApiService()
							.updateSpan(span,
									TracingMessage.message(
											String.format(objectTypeIdsValues.get(0),
													ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()),
											ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
					throw new CmisInvalidArgumentException(String.format(objectTypeIdsValues.get(0),
							ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()));
				}
			}

			// load secondary types
			List<String> secondaryObjectTypeIdsValues = properties.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (isNotEmpty(secondaryObjectTypeIdsValues)) {
				for (String secTypeId : secondaryObjectTypeIdsValues) {
					TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, secTypeId, null,
							userObject, tracingId, span);
					secondaryTypes.add(typeDef);
					if (typeDef == null) {
						TracingApiServiceFactory.getApiService().updateSpan(span,
								TracingMessage.message(
										String.format(objectTypeIdsValues.get(0), ErrorMessages.INVALID_TYPE_DEFINITION,
												span.getTraceId()),
										ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
						TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
						throw new CmisInvalidArgumentException(String.format(objectTypeIdsValues.get(0),
								ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()));
					}
				}
			}

			// create properties
			PropertiesImpl result = new PropertiesImpl();
			for (Map.Entry<String, List<String>> property : properties.entrySet()) {
				PropertyDefinition<?> propDef = getPropertyDefinition(objectType, property.getKey());
				if (propDef == null) {
					propDef = getPropertyDefinition(secondaryTypes, property.getKey());
				}

				if (propDef == null) {
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingMessage.message(String.format(ErrorMessages.UNKNOWN_PROPERTY, span.getTraceId()),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
					throw new CmisInvalidArgumentException(
							String.format(ErrorMessages.UNKNOWN_PROPERTY, span.getTraceId()));
				}

				result.addProperty(createPropertyData(propDef, property.getValue()));
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			return result;
		}

		public static Properties createUpdateProperties(Map<String, List<String>> propertiesdata, String typeId,
				List<String> secondaryTypeIds, List<String> objectIds, String repositoryId, IBaseObject data,
				IUserObject userObject, String tracingId, ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisPropertyConverter::createUpdateProperties", null);
			List<TypeDefinition> secondaryTypes = new ArrayList<>();
			List<TypeDefinition> objectType = new ArrayList<>();
			List<TypeDefinition> innerObjectType = new ArrayList<>();
			if (LOG.isDebugEnabled()) {
				LOG.debug("createUpdateProperties on objectIds: {} getting type definition using this id : {}",
						objectIds, typeId);
			}
			Map<String, List<String>> properties = propertiesdata;
			if (properties == null) {
				return null;
			}

			// load primary type
			if (typeId != null) {
				TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject, tracingId, span);
				objectType.add(typeDef);
				if (typeDef == null) {
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingMessage.message(
									String.format(typeId, ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
					throw new CmisInvalidArgumentException(
							String.format(typeId, ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()));
				}
			}

			// load secondary types
			List<String> secondaryObjectTypeIdsValues = properties.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (isNotEmpty(secondaryObjectTypeIdsValues)) {
				for (String secTypeId : secondaryObjectTypeIdsValues) {
					TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, secTypeId, null,
							userObject, tracingId, span);
					secondaryTypes.add(typeDef);
					if (typeDef == null) {
						TracingApiServiceFactory.getApiService().updateSpan(span,
								TracingMessage.message(
										String.format(secTypeId, ErrorMessages.INVALID_TYPE_DEFINITION,
												span.getTraceId()),
										ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
						TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
						throw new CmisInvalidArgumentException(
								String.format(secTypeId, ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()));
					}
				}
			}

			if (secondaryTypeIds != null) {
				for (String secTypeId : secondaryTypeIds) {
					TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, secTypeId, null,
							userObject, tracingId, span);
					secondaryTypes.add(typeDef);
					if (typeDef == null) {
						TracingApiServiceFactory.getApiService().updateSpan(span,
								TracingMessage.message(
										String.format(secTypeId, ErrorMessages.INVALID_TYPE_DEFINITION,
												span.getTraceId()),
										ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
						TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
						throw new CmisInvalidArgumentException(
								String.format(secTypeId, ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()));
					}
				}
			}

			// create properties
			PropertiesImpl result = new PropertiesImpl();
			for (Map.Entry<String, List<String>> property : properties.entrySet()) {
				PropertyDefinition<?> propDef = getPropertyDefinition(objectType, property.getKey());
				if (propDef == null && objectIds != null) {
					for (String objectId : objectIds) {
						IBaseObject object = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, typeId);
						TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId,
								object.getTypeId(), null, userObject, tracingId, span);
						innerObjectType.add(typeDef);
						propDef = getPropertyDefinition(innerObjectType, property.getKey());
						if (propDef != null) {
							break;
						}
					}
				}
				if (propDef == null) {
					if (secondaryTypes.isEmpty()) {
						for (String secTypeId : data.getSecondaryTypeIds()) {
							TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, secTypeId,
									null, userObject, tracingId, span);
							secondaryTypes.add(typeDef);
							if (typeDef == null) {
								TracingApiServiceFactory.getApiService().updateSpan(span,
										TracingMessage.message(
												String.format(secTypeId, ErrorMessages.INVALID_TYPE_DEFINITION,
														span.getTraceId()),
												ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
								TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
								throw new CmisInvalidArgumentException(String.format(secTypeId,
										ErrorMessages.INVALID_TYPE_DEFINITION, span.getTraceId()));
							}
						}
					}
					propDef = getPropertyDefinition(secondaryTypes, property.getKey());
				}
				if (propDef == null) {
					throw new CmisInvalidArgumentException(property.getKey() + " is unknown!");
				}

				result.addProperty(createPropertyData(propDef, property.getValue()));
			}

			LOG.debug("createUpdateProperties on objectIds: {} are : resultProperties{}", objectIds,
					result != null ? result.getPropertyList() : null);

			return result;
		}

		@SuppressWarnings("unchecked")
		private static PropertyData<?> createPropertyData(PropertyDefinition<?> propDef, Object value) {

			List<String> strValues;
			if (value == null) {
				strValues = Collections.emptyList();
			} else if (value instanceof String) {
				strValues = new ArrayList<String>();
				strValues.add((String) value);
			} else {
				strValues = (List<String>) value;
			}

			PropertyData<?> propertyData = null;
			switch (propDef.getPropertyType()) {
			case STRING:
				propertyData = new PropertyStringImpl(propDef.getId(), strValues);
				break;
			case ID:
				propertyData = new PropertyIdImpl(propDef.getId(), strValues);
				break;
			case BOOLEAN:
				List<Boolean> boolValues = new ArrayList<Boolean>(strValues.size());
				for (String s : strValues) {
					boolValues.add(Boolean.valueOf(s));
				}
				propertyData = new PropertyBooleanImpl(propDef.getId(), boolValues);
				break;
			case INTEGER:
				List<BigInteger> intValues = new ArrayList<BigInteger>(strValues.size());
				try {
					for (String s : strValues) {
						intValues.add(new BigInteger(s));
					}
				} catch (NumberFormatException e) {
					throw new CmisInvalidArgumentException(propDef.getId() + " value is not an integer value!", e);
				}
				propertyData = new PropertyIntegerImpl(propDef.getId(), intValues);
				break;
			case DECIMAL:
				List<BigDecimal> decValues = new ArrayList<BigDecimal>(strValues.size());
				try {
					for (String s : strValues) {
						decValues.add(new BigDecimal(s));
					}
				} catch (NumberFormatException e) {
					throw new CmisInvalidArgumentException(propDef.getId() + " value is not an integer value!", e);
				}
				propertyData = new PropertyDecimalImpl(propDef.getId(), decValues);
				break;
			case DATETIME:
				List<GregorianCalendar> calValues = new ArrayList<GregorianCalendar>(strValues.size());
				for (String s : strValues) {
					GregorianCalendar cal;
					try {
						long timestamp = Long.parseLong(s);
						cal = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
						cal.setTimeInMillis(timestamp);
					} catch (NumberFormatException e) {
						cal = DateTimeHelper.parseXmlDateTime(s);
					}

					if (cal == null) {
						throw new CmisInvalidArgumentException(propDef.getId() + " value is not an datetime value!");
					}

					calValues.add(cal);
				}

				propertyData = new PropertyDateTimeImpl(propDef.getId(), calValues);
				break;
			case HTML:
				propertyData = new PropertyHtmlImpl(propDef.getId(), strValues);
				break;
			case URI:
				propertyData = new PropertyUriImpl(propDef.getId(), strValues);
				break;
			default:
				assert false;
			}

			return propertyData;
		}

		public static boolean isNotEmpty(Collection<?> col) {
			return col != null && !col.isEmpty();
		}

		public static boolean isNullOrEmpty(Collection<?> col) {
			return col == null || col.isEmpty();
		}

		private static PropertyDefinition<?> getPropertyDefinition(List<TypeDefinition> types, String propertyName) {
			for (TypeDefinition type : types) {
				Map<String, PropertyDefinition<?>> property = type.getPropertyDefinitions();
				for (PropertyDefinition<?> pro : property.values()) {
					if (pro.getId().equals(propertyName)) {

						return pro;
					}
				}
			}

			return null;
		}

		public static String getTypeIdForObject(String repositoryId, String objectId, String typeId) {
			if (objectId == null) {
				throw new CmisInvalidArgumentException("Object Id must be set.");
			}
			IBaseObject data = null;
			try {
				data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, typeId);
			} catch (Exception e) {
				throw new MongoException(e.toString());
			}
			if (data == null) {
				LOG.error("getTypeIdForObject Object id: {}, null in : {} repository!", objectId, repositoryId);
				throw new CmisObjectNotFoundException("Object must not be null!");
			}
			return data.getTypeId();
		}
	}
}
