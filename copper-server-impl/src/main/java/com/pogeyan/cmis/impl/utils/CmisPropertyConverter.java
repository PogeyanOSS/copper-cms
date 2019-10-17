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
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyIntegerDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyStringDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConstraintException;
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
import com.pogeyan.cmis.api.data.IObjectEncryptService;
import com.pogeyan.cmis.api.data.common.EncryptType;
import com.pogeyan.cmis.impl.factory.EncryptionFactory;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.impl.services.CmisTypeServices;

public class CmisPropertyConverter {
	private static final Logger LOG = LoggerFactory.getLogger(CmisPropertyConverter.class);

	public static class Impl {

		public static Properties createNewProperties(Map<String, List<String>> propertiesdata, String repositoryId,
				IUserObject userObject) {
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
						objectTypeIdsValues.get(0), null, userObject, null, null);
				objectType.add(typeDef);
				if (typeDef == null) {
					throw new CmisInvalidArgumentException("Invalid type: " + objectTypeIdsValues.get(0));
				}
			}

			// load secondary types
			List<String> secondaryObjectTypeIdsValues = properties.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (isNotEmpty(secondaryObjectTypeIdsValues)) {
				for (String secTypeId : secondaryObjectTypeIdsValues) {
					TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, secTypeId, null,
							userObject, null, null);
					secondaryTypes.add(typeDef);
					if (typeDef == null) {
						throw new CmisInvalidArgumentException("Invalid type: " + secTypeId);
					}
				}
			}

			// create properties
			PropertiesImpl result = new PropertiesImpl();
			IObjectEncryptService encryptService = EncryptionFactory.createEncryptionService(repositoryId);
			for (Map.Entry<String, List<String>> property : properties.entrySet()) {
				PropertyDefinition<?> propDef = getPropertyDefinition(objectType, property.getKey());
				if (propDef == null) {
					propDef = getPropertyDefinition(secondaryTypes, property.getKey());
				}

				if (propDef == null) {
					throw new CmisInvalidArgumentException(property.getKey() + " is unknown!");
				}

				result.addProperty(createPropertyData(objectTypeIdsValues.get(0), propDef, property.getValue(),
						repositoryId, encryptService, secondaryObjectTypeIdsValues, propertiesdata));
			}

			return result;
		}

		public static Properties createUpdateProperties(Map<String, List<String>> propertiesdata, String typeId,
				List<String> secondaryTypeIds, List<String> objectIds, String repositoryId, IBaseObject data,
				IUserObject userObject) {
			String[] principalIds = Helpers.getPrincipalIds(userObject);
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
				TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
						null, null);
				objectType.add(typeDef);
				if (typeDef == null) {
					throw new CmisInvalidArgumentException("Invalid type: " + typeId);
				}
			}

			// load secondary types
			List<String> secondaryObjectTypeIdsValues = properties.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (isNotEmpty(secondaryObjectTypeIdsValues)) {
				for (String secTypeId : secondaryObjectTypeIdsValues) {
					TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, secTypeId, null,
							userObject, null, null);
					secondaryTypes.add(typeDef);
					if (typeDef == null) {
						throw new CmisInvalidArgumentException("Invalid type: " + secTypeId);
					}
				}
			}

			if (secondaryTypeIds != null) {
				for (String secTypeId : secondaryTypeIds) {
					TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, secTypeId, null,
							userObject, null, null);
					secondaryTypes.add(typeDef);
					if (typeDef == null) {
						throw new CmisInvalidArgumentException("Invalid secondary type: " + secTypeId);
					}
				}
			}
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			// create properties
			PropertiesImpl result = new PropertiesImpl();
			IObjectEncryptService encryptService = EncryptionFactory.createEncryptionService(repositoryId);
			for (Map.Entry<String, List<String>> property : properties.entrySet()) {
				PropertyDefinition<?> propDef = getPropertyDefinition(objectType, property.getKey());
				if (propDef == null && objectIds != null) {
					for (String objectId : objectIds) {
						IBaseObject object = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, objectId,
								null, typeId);
						TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId,
								object.getTypeId(), null, userObject, null, null);
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
									null, userObject, null, null);
							secondaryTypes.add(typeDef);
							if (typeDef == null) {
								throw new CmisInvalidArgumentException("Invalid secondary type: " + secTypeId);
							}
						}
					}
					propDef = getPropertyDefinition(secondaryTypes, property.getKey());
				}
				if (propDef == null) {
					throw new CmisInvalidArgumentException(property.getKey() + " is unknown!");
				}

				result.addProperty(createPropertyData(typeId, propDef, property.getValue(), repositoryId,
						encryptService, secondaryObjectTypeIdsValues, propertiesdata));
			}

			LOG.debug("createUpdateProperties on objectIds: {} are : resultProperties{}", objectIds,
					result != null ? result.getPropertyList() : null);

			return result;
		}

		@SuppressWarnings("unchecked")
		private static PropertyData<?> createPropertyData(String typeId, PropertyDefinition<?> propDef, Object value,
				String repositoryId, IObjectEncryptService encryptService, List<String> secondaryObjectTypeIdsValues,
				Map<String, List<String>> propertyDataMap) {
			List<String> strValues;
			if (value == null) {
				strValues = Collections.emptyList();
			} else if (value instanceof String) {
				strValues = new ArrayList<String>();
				strValues.add((String) value);
			} else {
				strValues = (List<String>) value;
				strValues = new ArrayList<>(strValues);
			}

			PropertyData<?> propertyData = null;
			boolean result = invokeEncryptBeforeCreate(encryptService, repositoryId, EncryptType.ENCRYPT, typeId,
					propDef.getId(), strValues, secondaryObjectTypeIdsValues, propertyDataMap);
			if (result) {
				propertyData = new PropertyStringImpl(propDef.getId(), strValues);
			} else {
				switch (propDef.getPropertyType()) {
				case STRING:
					PropertyStringDefinition strPropDef = (PropertyStringDefinition) propDef;
					for (String strVal : strValues) {
						if (strPropDef.getMaxLength() != null
								&& strVal.length() > strPropDef.getMaxLength().intValueExact()) {
							throw new CmisConstraintException(propDef.getId() + " value size is greater than "
									+ strPropDef.getMaxLength().toString());
						}
					}
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
					PropertyIntegerDefinition intPropDef = (PropertyIntegerDefinition) propDef;
					List<BigInteger> intValues = new ArrayList<BigInteger>(strValues.size());
					try {
						for (String s : strValues) {
							BigInteger bigint = new BigInteger(s);
							if ((intPropDef.getMinValue() != null && bigint.compareTo(intPropDef.getMinValue()) < 0)
									|| (intPropDef.getMaxValue() != null
											&& bigint.compareTo(intPropDef.getMaxValue()) > 0)) {
								throw new CmisConstraintException(propDef.getId() + " value is not in range ["
										+ intPropDef.getMinValue() + "-" + intPropDef.getMaxValue() + "]");
							}
							intValues.add(bigint);
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
							throw new CmisInvalidArgumentException(
									propDef.getId() + " value is not an datetime value!");
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

		public static String getTypeIdForObject(String repositoryId, String[] principalIds, String objectId,
				String typeId) {
			if (objectId == null) {
				throw new CmisInvalidArgumentException("Object Id must be set.");
			}
			IBaseObject data = null;
			try {
				data = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, false, objectId, null, typeId);
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

	private static boolean invokeEncryptBeforeCreate(IObjectEncryptService objectFlowService, String repositoryId,
			EncryptType invokeMethod, String typeId, String propId, List<String> strValues,
			List<String> secondaryObjectTypeIdsValues, Map<String, List<String>> propertyDataMap) {
		boolean resultFlow = false;
		if (objectFlowService != null) {
			try {
				LOG.info("invokeEncryptBeforeCreate, InvokeMethod: {}", invokeMethod);
				if (EncryptType.ENCRYPT.equals(invokeMethod)) {
					resultFlow = objectFlowService.encrypt(repositoryId, typeId, propId, strValues,
							secondaryObjectTypeIdsValues, propertyDataMap);
				}
			} catch (Exception ex) {
				LOG.error("Operation failed with ObjectFlowService for InvokeMethod: {}, with exception: {}",
						invokeMethod, ex.getMessage());
				throw new IllegalArgumentException(ex.getMessage());
			}
		}
		return resultFlow;
	}
}
