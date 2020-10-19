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
package com.pogeyan.cmis.impl.services;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.AllowableActions;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.data.RenditionData;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.Action;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.RelationshipDirection;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AllowableActionsImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChangeEventInfoDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PolicyIdListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyBooleanImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDateTimeImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDecimalImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyHtmlImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIntegerImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyUriImpl;
import org.apache.chemistry.opencmis.commons.impl.server.ObjectInfoImpl;
import org.apache.chemistry.opencmis.commons.impl.server.RenditionInfoImpl;
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.apache.chemistry.opencmis.commons.server.RenditionInfo;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.IObjectEncryptService;
import com.pogeyan.cmis.api.data.IObjectFlowFactory;
import com.pogeyan.cmis.api.data.IObjectFlowService;
import com.pogeyan.cmis.api.data.IRelationObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.common.EncryptType;
import com.pogeyan.cmis.api.data.common.ObjectFlowType;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.api.utils.TracingWriter;
import com.pogeyan.cmis.impl.factory.EncryptionFactory;
import com.pogeyan.cmis.impl.factory.ObjectFlowFactory;
import com.pogeyan.cmis.impl.factory.TypeServiceFactory;
import com.pogeyan.cmis.impl.utils.CmisUtils;
import com.pogeyan.cmis.impl.utils.DBUtils;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class CmisRelationshipService {
	private static final Logger LOG = LoggerFactory.getLogger(CmisNavigationService.class);
	public static final String FV_ENCRYPT_PROPS = "fv:encryptProperties";
	
	public static class Impl {
		/**
		 * CMIS getObjectRelationships.
		 */
		public static ObjectList getObjectRelationships(String repositoryId, String objectId,
				Boolean includeSubRelationshipTypes, RelationshipDirection relationshipDirection, String typeId,
				String filter, Boolean includeAllowableActions, BigInteger maxItems, BigInteger skipCount,
				ObjectInfoHandler objectInfos, IUserObject userObject, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException, MongoException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisRelationshipService::getobjectRelationships", null);
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			IBaseObject so = null;
			try {
				so = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, objectId, null, typeId);
			} catch (Exception e) {
				LOG.error("Method name: {}, getObjectRelationships Exception: {}, repositoryid: {}, TraceId: {}",
						"getObjectRelationships", ExceptionUtils.getStackTrace(e), repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span),
								ErrorMessages.MONGO_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new MongoException(TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span));
			}

			if (so == null) {
				LOG.error("Method name: {}, getObjectRelationships Exception: {}, {}, repositoryid: {}, traceId: {}",
						"getObjectRelationships", "Unknown object id", objectId, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
			}
			int maxItemsInt = maxItems == null ? -1 : maxItems.intValue();
			int skipCountInt = skipCount == null ? 0 : skipCount.intValue();

			String[] filterArray = null;
			// split filter
			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			if (filter != null && filterCollection != null && filterCollection.size() > 0) {
				filterArray = Helpers.getFilterArray(filterCollection, so.getBaseId() != BaseTypeId.CMIS_DOCUMENT);
			}
			ObjectListImpl result = new ObjectListImpl();
			List<ObjectData> odList = null;
			List<? extends IRelationObject> totalSize = null;
			if (relationshipDirection == RelationshipDirection.SOURCE) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipBySourceId(repositoryId, so.getId().toString(), true,
						-1, -1, null, typeId);
				odList = getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.SOURCE, so, userObject, maxItemsInt, skipCountInt, filterArray);
			} else if (relationshipDirection == RelationshipDirection.TARGET) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipByTargetId(repositoryId, so.getId().toString(), true,
						-1, -1, null, typeId);
				odList = getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.TARGET, so, userObject, maxItemsInt, skipCountInt, filterArray);
			} else if (relationshipDirection == RelationshipDirection.EITHER) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipBySourceId(repositoryId, so.getId().toString(), true,
						-1, -1, null, typeId);
				odList = getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.BOTH, so, userObject, maxItemsInt, skipCountInt, filterArray);
			}

			result.setObjects(odList);
			result.setNumItems(BigInteger.valueOf(totalSize.size()));
			result.setHasMoreItems(totalSize.size() > maxItemsInt - 1);

			if (result != null) {
				LOG.debug("ObjectRelationships result count: {}", result.getNumItems());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;

		}

	public static ObjectData getRelationshipSimpleObject(String repositoryId, String objectId, IUserObject userObject,
			BaseTypeId baseTypeId, String typeId) {
		return getRelationshipObject(repositoryId, objectId, null, false, IncludeRelationships.NONE, "cmis:none", false, true,
				null, userObject, baseTypeId, typeId, null, null);
	}

	public static ObjectData getRelationshipObject(String repositoryId, String objectId, String filter,
			Boolean includeAllowableActions, IncludeRelationships includeRelationships, String renditionFilter,
			Boolean includePolicyIds, Boolean includeAcl, ObjectInfoHandler objectInfos, IUserObject userObject,
			BaseTypeId baseTypeId, String typeId, String tracingId, ISpan parentSpan)
			throws CmisInvalidArgumentException, IllegalArgumentException, CmisObjectNotFoundException {
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"CmisObjectService::getObject", null);
		if (objectId == null && filter == null) {
			LOG.error("getObject objectId and filter is null in repository: {}, TraceId: {}", repositoryId,
					span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.OBJECT_ID_NULL), span),
							ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisObjectNotFoundException(
					TracingWriter.log(String.format(ErrorMessages.OBJECT_ID_NULL), span));
		} else if (objectId == null) {
			LOG.error("getObject objectId is null in repository: {}, TraceId: {}", repositoryId,
					span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.OBJECT_ID_NULL), span),
							ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisInvalidArgumentException(
					TracingWriter.log(String.format(ErrorMessages.OBJECT_ID_NULL), span));
		}
		String[] principalIds = Helpers.getPrincipalIds(userObject);
		String[] filterArray = null;
		// split filter
		Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
		if (filter != null && filterCollection != null && filterCollection.size() > 0) {
			filterArray = Helpers.getFilterArray(filterCollection, baseTypeId != BaseTypeId.CMIS_DOCUMENT);
		}
		String systemAdmin = System.getenv("SYSTEM_ADMIN");
		boolean aclPropagation = Stream.of(userObject.getGroups())
				.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
		IRelationObject data = null;
		try {
			if(baseTypeId == BaseTypeId.CMIS_RELATIONSHIP) {
				data = DBUtils.RelationshipDAO.getRelationshipByObjectId(repositoryId, principalIds, false, objectId,
						filterArray, typeId);
			}
		} catch (Exception e) {
			LOG.error("getObject Exception: {}, repository: {}, TraceId: {}", ExceptionUtils.getStackTrace(e),
					repositoryId, span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span),
							ErrorMessages.MONGO_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new MongoException(TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span));
		}
		if (data == null) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span),
							ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisObjectNotFoundException(
					TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span));
		}

		// set defaults if values not set
		boolean iaa = getBooleanParameter(includeAllowableActions, false);
		boolean iacl = getBooleanParameter(includeAcl, false);

		// gather properties
		ObjectData od = compileObjectData(repositoryId, data, filterCollection, iaa, iacl, true, objectInfos,
				renditionFilter, includeRelationships, userObject, tracingId, span);
		if (od != null) {
			LOG.debug("getobject result data: {}", od.getProperties());
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return od;
	}
	
	private static boolean getBooleanParameter(Boolean value, boolean def) {
		if (value == null) {
			return def;
		}

		return value.booleanValue();
	}
	
	public static ObjectData compileObjectData(String repositoryId, IRelationObject data, Set<String> filter,
			boolean includeAllowableActions, boolean includeAcl, boolean userReadOnly,
			ObjectInfoHandler objectInfos, String renditionFilter, IncludeRelationships includeRelationships,
			IUserObject userObject, String tracingId, ISpan parentSpan) throws IllegalArgumentException {
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"CmisObjectService::compileObjectData", null);
		ObjectDataImpl result = new ObjectDataImpl();
		ObjectInfoImpl objectInfo = new ObjectInfoImpl();
		result.setProperties(
				compileProperties(repositoryId, data, filter, objectInfo, userObject, tracingId, span));

		if (includeAllowableActions) {
			AllowableActions action = getAllowableActions(repositoryId, data, null,
					userObject == null ? null : userObject, data.getTypeId(), tracingId, span);
			result.setAllowableActions(action);
		}

		if (includeAcl) {
			Acl acl = (Acl) data.getAcl();
			result.setAcl(acl);
		}
//		List<RenditionData> renditions = getRenditions(repositoryId, data, null, renditionFilter,
//				BigInteger.valueOf(1), BigInteger.valueOf(0), userObject == null ? null : userObject.getUserDN(),
//				data.getTypeId(), tracingId, span);
//		result.setRenditions(renditions);

//		if (null != includeRelationships && includeRelationships != IncludeRelationships.NONE) {
//			result.setRelationships(fillRelationships(repositoryId, includeAllowableActions, includeRelationships,
//					data, userObject, null));
//		}

		if (data.getChangeToken() != null) {
			GregorianCalendar changeEventDate = new GregorianCalendar();
			changeEventDate.setTimeInMillis(data.getChangeToken().getTime());

			ChangeEventInfoDataImpl changeEvent = new ChangeEventInfoDataImpl(
					TokenChangeType.fromValue(data.getChangeToken().getChangeType()), changeEventDate);
			result.setChangeEventInfo(changeEvent);
		}

//		if (data.getPolicies() != null) {
//			PolicyIdListImpl polIds = new PolicyIdListImpl();
//			List<String> pols = data.getPolicies();
//			polIds.setPolicyIds(pols);
//			result.setPolicyIds(polIds);
//		}

		// if (context.isObjectInfoRequired()) {
		// objectInfo.setObject(result);
		// objectInfos.addObjectInfo(objectInfo);
		//
		// }
		if (result != null) {
			LOG.debug(
					"Compiled ObjectDataInfo for this objectId: {}, repository:{}, policies : {}, acl : {}, changeEvent: {}",
					data != null ? data.getId() : null, repositoryId, result.getPolicyIds(), result.getAcl(),
					result.getChangeEventInfo());
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return result;
	}

	public static Properties compileProperties(String repositoryId, IRelationObject data, Set<String> orgfilter,
			ObjectInfoImpl objectInfo, IUserObject userObject, String tracingId, ISpan parentSpan)
			throws IllegalArgumentException {
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"CmisObjectService::compileProperties", null);
		if (data == null) {
			LOG.error("compileProperties object is null in: {} repository!, TraceId: {}", repositoryId,
					span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span),
							ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new IllegalArgumentException(TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span));
		}

		if (objectInfo == null) {
			LOG.error("compileProperties ObjectInfoImpl is null in: {} repository!, TraceId: {}", repositoryId,
					span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.OBJECT_INFOIMPL_NULL), span),
							ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new IllegalArgumentException(
					TracingWriter.log(String.format(ErrorMessages.OBJECT_INFOIMPL_NULL), span));
		}

		// copy filter
		Set<String> filter = (orgfilter == null ? null : new HashSet<String>(orgfilter));

		// find base type
		String typeId = null;

		if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_RELATIONSHIP.value())
				|| data.getBaseId() == BaseTypeId.CMIS_RELATIONSHIP) {
			if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_RELATIONSHIP.value())) {
				typeId = BaseTypeId.CMIS_RELATIONSHIP.value();
			} else {
				typeId = data.getTypeId();
			}
			if (objectInfo != null) {
				objectInfo.setBaseType(BaseTypeId.CMIS_RELATIONSHIP);
				objectInfo.setTypeId(typeId);
				objectInfo.setContentType(null);
				objectInfo.setFileName(null);
				objectInfo.setHasAcl(true);
				objectInfo.setHasContent(false);
				objectInfo.setVersionSeriesId(null);
				objectInfo.setIsCurrentVersion(true);
				objectInfo.setSupportsDescendants(true);
				objectInfo.setSupportsFolderTree(true);
				objectInfo.setSupportsPolicies(false);
				objectInfo.setSupportsRelationships(false);
				objectInfo.setWorkingCopyId(null);
				objectInfo.setWorkingCopyOriginalId(null);
			}
		} 
		// let's do it
		try {
			PropertiesImpl result = new PropertiesImpl();
			TypeDefinition type = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
					tracingId, span);
			// id
			String id = data.getId().toString();
			addPropertyId(repositoryId, result, type, filter, PropertyIds.OBJECT_ID, id, userObject);
			objectInfo.setId(id);

			// // parent id
			// if (data.getParentId() != null) {
			// addPropertyId(repositoryId, result, type, filter,
			// PropertyIds.PARENT_ID, data.getParentId());
			// }

			// name
			String name = data.getName();
			if (name != null) {
				addPropertyString(repositoryId, result, type, filter, PropertyIds.NAME, name, userObject);
				objectInfo.setName(name);
			}

			// created and modified by
			if (data.getCreatedBy() != null) {
				addPropertyString(repositoryId, result, type, filter, PropertyIds.CREATED_BY, data.getCreatedBy(),
						userObject);
				objectInfo.setCreatedBy(data.getCreatedBy());
			}

			// last modified by
			if (data.getModifiedBy() != null) {
				addPropertyString(repositoryId, result, type, filter, PropertyIds.LAST_MODIFIED_BY,
						data.getModifiedBy(), userObject);
			}

			if (data.getCreatedAt() != null) {
				GregorianCalendar creationDateCalender = new GregorianCalendar();
				creationDateCalender.setTimeInMillis(data.getCreatedAt());

				addPropertyDateTime(repositoryId, result, type, filter, PropertyIds.CREATION_DATE,
						creationDateCalender, userObject);
				objectInfo.setCreationDate(creationDateCalender);
			}
			GregorianCalendar lastModifiedCalender = new GregorianCalendar();
			if (data.getModifiedAt() != null) {
				lastModifiedCalender.setTimeInMillis(data.getModifiedAt());
				addPropertyDateTime(repositoryId, result, type, filter, PropertyIds.LAST_MODIFICATION_DATE,
						lastModifiedCalender, userObject);
				objectInfo.setLastModificationDate(lastModifiedCalender);
			}

			// change token - always null
			addPropertyString(repositoryId, result, type, filter, PropertyIds.CHANGE_TOKEN,
					String.valueOf(data.getChangeToken() != null ? data.getChangeToken().getTime() : "token-24"),
					userObject);

			addPropertyString(repositoryId, result, type, filter, PropertyIds.DESCRIPTION,
					data.getDescription() == null ? "" : data.getDescription(), userObject);

//			addPropertyId(repositoryId, result, type, filter, PropertyIds.SECONDARY_OBJECT_TYPE_IDS,
//					data.getSecondaryTypeIds() == null ? null : data.getSecondaryTypeIds(), userObject);

			// directory or file
			if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_FOLDER.value())
					|| data.getBaseId() == BaseTypeId.CMIS_FOLDER) {

				// base type and type name
				addPropertyId(repositoryId, result, type, filter, PropertyIds.BASE_TYPE_ID,
						BaseTypeId.CMIS_FOLDER.value(), userObject);
				if (typeId != null) {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.OBJECT_TYPE_ID, typeId,
							userObject);
				}
				if (data.getParentId() == null) {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.PARENT_ID, null, userObject);

				} else {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.PARENT_ID, data.getParentId(),
							userObject);
					objectInfo.setHasParent(true);
				}
//				addPropertyString(repositoryId, result, type, filter, PropertyIds.PATH,
//						data.getPath() == null ? "" : data.getPath(), userObject);
				// String path = getRepositoryPath(file);

				// folder properties
				addPropertyId(repositoryId, result, type, filter, PropertyIds.ALLOWED_CHILD_OBJECT_TYPE_IDS, null,
						userObject);
			} else if ((data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_DOCUMENT.value())
					|| data.getBaseId() == BaseTypeId.CMIS_DOCUMENT)) {
				// base type and type name
				// base type and type name
				addPropertyId(repositoryId, result, type, filter, PropertyIds.BASE_TYPE_ID,
						BaseTypeId.CMIS_DOCUMENT.value(), userObject);

				if (typeId != null) {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.OBJECT_TYPE_ID, typeId,
							userObject);
				}

				if (data instanceof IDocumentObject) {
					IDocumentObject documentObject = (IDocumentObject) data;

					addPropertyBoolean(repositoryId, result, type, filter, PropertyIds.IS_LATEST_VERSION,
							documentObject.getIsLatestVersion() == null ? false
									: documentObject.getIsLatestVersion(),
							userObject);

					addPropertyBoolean(repositoryId, result, type, filter, PropertyIds.IS_IMMUTABLE,
							documentObject.getIsImmutable() == null ? false : documentObject.getIsImmutable(),
							userObject);

					addPropertyBoolean(repositoryId, result, type, filter, PropertyIds.IS_MAJOR_VERSION,
							documentObject.getIsMajorVersion() == null ? false : documentObject.getIsMajorVersion(),
							userObject);

					addPropertyBoolean(repositoryId, result, type, filter, PropertyIds.IS_LATEST_MAJOR_VERSION,
							documentObject.getIsLatestMajorVersion() == null ? false
									: documentObject.getIsLatestMajorVersion(),
							userObject);

					addPropertyString(repositoryId, result, type, filter, PropertyIds.VERSION_LABEL,
							documentObject.getVersionLabel() == null ? "versionLabel"
									: documentObject.getVersionLabel(),
							userObject);

					addPropertyId(repositoryId, result, type, filter, PropertyIds.VERSION_SERIES_ID,
							documentObject.getVersionSeriesId(), userObject);

					addPropertyBoolean(repositoryId, result, type, filter,
							PropertyIds.IS_VERSION_SERIES_CHECKED_OUT,
							documentObject.getIsVersionSeriesCheckedOut() == null ? false
									: documentObject.getIsVersionSeriesCheckedOut(),
							userObject);

					addPropertyString(repositoryId, result, type, filter, PropertyIds.VERSION_SERIES_CHECKED_OUT_BY,
							documentObject.getVersionSeriesCheckedOutBy(), userObject);

					addPropertyString(repositoryId, result, type, filter, PropertyIds.VERSION_SERIES_CHECKED_OUT_ID,
							documentObject.getVersionSeriesCheckedOutId(), userObject);

					addPropertyString(repositoryId, result, type, filter, PropertyIds.CHECKIN_COMMENT,
							documentObject.getCheckinComment(), userObject);

					addPropertyId(repositoryId, result, type, filter, "cmis:previousVersionObjectId",
							documentObject.getPreviousVersionObjectId() == null ? "defaultID"
									: documentObject.getPreviousVersionObjectId(),
							userObject);

					addPropertyBoolean(repositoryId, result, type, filter, PropertyIds.IS_PRIVATE_WORKING_COPY,
							documentObject.getIsPrivateWorkingCopy() == null ? false
									: documentObject.getIsPrivateWorkingCopy(),
							userObject);
//					addPropertyString(repositoryId, result, type, filter, PropertyIds.PATH,
//							data.getPath() == null ? "" : data.getPath(), userObject);
					// if (context.getCmisVersion() != CmisVersion.CMIS_1_0)
					// {
					// addPropertyBoolean(repositoryId, result, type,
					// filter,
					// PropertyIds.IS_PRIVATE_WORKING_COPY,
					// false);
					// }
					addPropertyString(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_ID,
							documentObject.getContentStreamId(), userObject);
					if (documentObject.getContentStreamLength() == null
							&& documentObject.getContentStreamMimeType() == null) {
						addPropertyBigInteger(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_LENGTH,
								null, userObject);
						addPropertyString(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_MIME_TYPE,
								documentObject.getContentStreamMimeType(), userObject);
						addPropertyString(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_FILE_NAME,
								documentObject.getContentStreamFileName(), userObject);
						objectInfo.setHasContent(false);

					} else {
						// file content
						addPropertyBigInteger(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_LENGTH,
								BigInteger.valueOf(documentObject.getContentStreamLength()), userObject);
						addPropertyString(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_MIME_TYPE,
								documentObject.getContentStreamMimeType(), userObject);
						addPropertyString(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_FILE_NAME,
								documentObject.getContentStreamFileName(), userObject);
						objectInfo.setHasContent(true);
					}

					objectInfo.setFileName(documentObject.getContentStreamFileName());
					objectInfo.setContentType(documentObject.getContentStreamMimeType());
					addPropertyId(repositoryId, result, type, filter, PropertyIds.CONTENT_STREAM_ID,
							documentObject.getContentStreamId(), userObject);

				}
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_ITEM.value())
					|| data.getBaseId() == BaseTypeId.CMIS_ITEM) {
				addPropertyId(repositoryId, result, type, filter, PropertyIds.BASE_TYPE_ID,
						BaseTypeId.CMIS_ITEM.value(), userObject);
				if (typeId != null) {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.OBJECT_TYPE_ID, typeId,
							userObject);
				}
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_POLICY.value())
					|| data.getBaseId() == BaseTypeId.CMIS_POLICY) {
				addPropertyId(repositoryId, result, type, filter, PropertyIds.BASE_TYPE_ID,
						BaseTypeId.CMIS_POLICY.value(), userObject);
				if (typeId != null) {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.OBJECT_TYPE_ID, typeId,
							userObject);
				}
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_SECONDARY.value())
					|| data.getBaseId() == BaseTypeId.CMIS_SECONDARY) {
				addPropertyId(repositoryId, result, type, filter, PropertyIds.BASE_TYPE_ID,
						BaseTypeId.CMIS_SECONDARY.value(), userObject);
				if (typeId != null) {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.OBJECT_TYPE_ID, typeId,
							userObject);
				}
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_RELATIONSHIP.value())
					|| data.getBaseId() == BaseTypeId.CMIS_RELATIONSHIP) {
				addPropertyId(repositoryId, result, type, filter, PropertyIds.BASE_TYPE_ID,
						BaseTypeId.CMIS_RELATIONSHIP.value(), userObject);
				if (typeId != null) {
					addPropertyId(repositoryId, result, type, filter, PropertyIds.OBJECT_TYPE_ID, typeId,
							userObject);
				}
				objectInfo.setSupportsRelationships(true);
				if (data.getProperties() != null) {
					Map<String, Object> propDefs = data.getProperties();
					List<String> srcIds = new ArrayList<String>();
					List<String> targetIds = new ArrayList<String>();
					Map<String, IBaseObject> rels = getRelationshipObjects(repositoryId, propDefs, typeId);
					Set<Map.Entry<String, IBaseObject>> datas = rels.entrySet();
					for (Map.Entry<String, IBaseObject> objectValues : datas) {
						String ids = objectValues.getKey();

						if (ids.equalsIgnoreCase(PropertyIds.SOURCE_ID)) {
							srcIds.add(ids.toString());

						} else if (ids.equalsIgnoreCase(PropertyIds.TARGET_ID)) {
							targetIds.add(ids.toString());
						}
					}
					objectInfo.setRelationshipSourceIds(srcIds);
					objectInfo.setRelationshipTargetIds(targetIds);
				}
			}
			if (data.getProperties() != null) {
				ITypePermissionService typePermissionService = TypeServiceFactory
						.createTypePermissionFlowService(repositoryId);
				readCustomProperties(repositoryId, data, result, type, filter, typePermissionService, userObject);
			}
			// if (filter != null) {
			// if (!filter.isEmpty()) {
			// LOG.warn("Unknown filter properties: {} ",
			// filter.toString());
			// }
			// }
			if (result != null) {
				LOG.debug(
						"Compiled Properties for this objectId: {}, repository: {}, with filter properties: {}, result properties: {}",
						data != null ? data.getId() : null, repositoryId,
						filter != null && !filter.isEmpty() ? filter.toString() : null,
						result != null ? null : result.getProperties());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		} catch (Exception e) {
			LOG.error("Method name: {}, error: {}, repository: {}, TraceId: {}", "compileProperties", e,
					repositoryId, span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span),
							ErrorMessages.RUNTIME_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span));
		}
	}
	
	public static AllowableActions getAllowableActions(String repositoryId, IRelationObject data, String objectId,
			IUserObject userObject, String typeId, String tracingId, ISpan parentSpan) {
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"CmisObjectService::getAllowableActions", null);
		String systemAdmin = System.getenv("SYSTEM_ADMIN");
		boolean aclPropagation = Stream.of(userObject.getGroups())
				.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;

		if (data == null && objectId != null) {
			try {
				String[] principalIds = Helpers.getPrincipalIds(userObject);
				data = DBUtils.RelationshipDAO.getRelationshipByObjectId(repositoryId, principalIds, aclPropagation, objectId, null,
						typeId);
			} catch (Exception e) {
				LOG.error("getAllowableActions Exception: {}, repository: {}, TraceId: {}",
						ExceptionUtils.getStackTrace(e), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span),
								ErrorMessages.MONGO_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new MongoException(
						TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span));
			}
		}
		if (data == null && objectId == null) {
			LOG.error("getAllowableActions unknown objectId: {}, repository: {}, TraceId: {}", objectId,
					repositoryId, span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.OBJECTID_MUST_BE_SET), span),
							ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisInvalidArgumentException(
					TracingWriter.log(String.format(ErrorMessages.OBJECTID_MUST_BE_SET), span));
		}

		if (data == null) {
			LOG.error("getAllowableActions for this objectId: {}, is null in: {} repository!, TraceId: {}",
					objectId, repositoryId, span != null ? span.getTraceId() : null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span),
							ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisObjectNotFoundException(
					TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span));
		}

		AllowableActions allowableActions = fillAllowableActions(repositoryId, data, userObject.getUserDN());
		if (allowableActions != null) {
			LOG.debug("Allowable actions on objectId: {}, are : {}", objectId,
					allowableActions.getAllowableActions());
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return allowableActions;
	}
	
	private static AllowableActions fillAllowableActions(String repositoryId, IRelationObject so, String user) {
		boolean isFolder = false;
		boolean isDocument = false;
		boolean isItem = false;
		boolean isPolicy = false;
		boolean hasContent = false;
		boolean isRootFolder = false;
		IDocumentObject documentData = null;
		if (so.getBaseId().value().equalsIgnoreCase(BaseTypeId.CMIS_FOLDER.value())) {
			isFolder = true;
		}
		if (so.getBaseId().value().equalsIgnoreCase(BaseTypeId.CMIS_DOCUMENT.value())) {
			isDocument = true;
		}
		if (so.getBaseId().value().equalsIgnoreCase(BaseTypeId.CMIS_ITEM.value())) {
			isItem = true;
		}
		if (so.getBaseId().value().equalsIgnoreCase(BaseTypeId.CMIS_POLICY.value())) {
			isPolicy = true;
		}
		boolean isFileable = isFolder || isDocument || isItem;
		@SuppressWarnings("unused")
		boolean isCheckedOut = false;
		boolean canCheckOut = false;
		boolean canCheckIn = false;
		boolean isVersioned = false;
		if (isFolder == true) {
			if (so.getName().equalsIgnoreCase("@ROOT@")) {
				isRootFolder = true;
			}
		} else if (isDocument == true) {
			if (so instanceof IDocumentObject) {
				documentData = (IDocumentObject) so;
			} else {
				documentData = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, so.getId(), null);
			}

			if (documentData.getContentStreamMimeType() != null) {
				hasContent = true;
			}
			if (documentData.getVersionLabel() != null) {
				isVersioned = true;
			}
			if (documentData.getIsVersionSeriesCheckedOut() != null
					&& documentData.getIsVersionSeriesCheckedOut() == true) {
				isCheckedOut = true;
			}
			if (documentData.getIsVersionSeriesCheckedOut() != null
					&& documentData.getIsVersionSeriesCheckedOut() == false) {
				canCheckOut = true;
			}
			if (documentData.getIsVersionSeriesCheckedOut() != null
					&& documentData.getIsVersionSeriesCheckedOut() == true
					&& documentData.getIsPrivateWorkingCopy() != null
					&& documentData.getIsPrivateWorkingCopy() == true) {
				canCheckIn = true;
			}
		}
		boolean hasRendition = hasRendition(user);
		boolean canGetAcl = user != null && (isDocument || isFolder || isItem);
		boolean canSetAcl = canGetAcl;
		// boolean cmis11 = callContext.getCmisVersion() !=
		// CmisVersion.CMIS_1_0;

		AllowableActionsImpl allowableActions = new AllowableActionsImpl();
		Set<Action> set = allowableActions.getAllowableActions();

		if (!isRootFolder) {
			set.add(Action.CAN_DELETE_OBJECT);
			set.add(Action.CAN_UPDATE_PROPERTIES);
			set.add(Action.CAN_APPLY_POLICY);
			set.add(Action.CAN_GET_PROPERTIES);
			set.add(Action.CAN_GET_APPLIED_POLICIES);
		}

		if (isRootFolder) {
			set.add(Action.CAN_GET_PROPERTIES);
		}

		if (isFolder || isDocument || isItem || isPolicy) {
			if (!isRootFolder && isFileable) {
				set.add(Action.CAN_GET_OBJECT_PARENTS);
				set.add(Action.CAN_MOVE_OBJECT);
			}
		}

		if (isFolder) {
			if (!isRootFolder) {
				set.add(Action.CAN_GET_FOLDER_PARENT);
				set.add(Action.CAN_DELETE_TREE);
			}
			set.add(Action.CAN_GET_FOLDER_TREE);
			set.add(Action.CAN_GET_DESCENDANTS);

			set.add(Action.CAN_CREATE_DOCUMENT);
			set.add(Action.CAN_CREATE_FOLDER);
			// if (cmis11) {
			// set.add(Action.CAN_CREATE_ITEM);
			// }
			set.add(Action.CAN_GET_CHILDREN);
		}

		if (hasContent) {
			set.add(Action.CAN_DELETE_CONTENT_STREAM);
			set.add(Action.CAN_GET_CONTENT_STREAM);
		}

		if (isVersioned) {
			if (canCheckOut) {
				set.add(Action.CAN_CHECK_OUT);
			}
			if (canCheckIn) {
				set.add(Action.CAN_CANCEL_CHECK_OUT);
				set.add(Action.CAN_CHECK_IN);
			}
			set.add(Action.CAN_GET_ALL_VERSIONS);
		}

		if (hasRendition) {
			set.add(Action.CAN_GET_RENDITIONS);
		}

		if (canSetAcl) {
			set.add(Action.CAN_APPLY_ACL);
		}
		if (canGetAcl) {
			set.add(Action.CAN_GET_ACL);
		}

		allowableActions.setAllowableActions(set);

		return allowableActions;
	}

	private static boolean hasRendition(String user) {
		return false;
	}

	private static List<ObjectData> fillRelationships(String repositoryId, boolean includeAllowableActions,
			IncludeRelationships includeRelationships, IBaseObject so, IUserObject userObject,
			String[] mappedColumns) {
		return getRelationships(repositoryId, includeAllowableActions, includeRelationships, so, userObject, -1, -1,
				mappedColumns);
	}

	public static List<ObjectData> getRelationships(String repositoryId, boolean includeAllowableActions,
			IncludeRelationships includeRelationships, IBaseObject spo, IUserObject userObject, int maxItems,
			int skipCount, String[] mappedColumns) {
		LOG.info("getRelationships on objectId: {}, repository: {}, includeRelationships for: {}",
				spo != null ? spo.getId() : null, repositoryId,
				includeRelationships != null ? includeRelationships.value() : null);
		String systemAdmin = System.getenv("SYSTEM_ADMIN");
		boolean aclPropagation = Stream.of(userObject.getGroups())
				.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
		if (includeRelationships == IncludeRelationships.SOURCE) {
			List<? extends IRelationObject> source = DBUtils.RelationshipDAO.getRelationshipBySourceId(repositoryId,
					spo.getId().toString(), aclPropagation, maxItems, skipCount, mappedColumns, spo.getTypeId());
			return getSourceTargetRelationship(repositoryId, includeAllowableActions, userObject, source);
		} else if (includeRelationships == IncludeRelationships.TARGET) {
			List<? extends IRelationObject> target = DBUtils.RelationshipDAO.getRelationshipByTargetId(repositoryId,
					spo.getId().toString(), aclPropagation, maxItems, skipCount, mappedColumns, spo.getTypeId());
			return getSourceTargetRelationship(repositoryId, includeAllowableActions, userObject, target);
		} else if (includeRelationships == IncludeRelationships.BOTH) {
			List<ObjectData> sourceTarget = new ArrayList<>();
			List<? extends IRelationObject> sourceObject = DBUtils.RelationshipDAO.getRelationshipBySourceId(
					repositoryId, spo.getId().toString(), aclPropagation, maxItems, skipCount, mappedColumns,
					spo.getTypeId());
			List<? extends IRelationObject> targetObject = DBUtils.RelationshipDAO.getRelationshipByTargetId(
					repositoryId, spo.getId().toString(), aclPropagation, maxItems, skipCount, mappedColumns,
					spo.getTypeId());
			List<ObjectData> source = getSourceTargetRelationship(repositoryId, includeAllowableActions, userObject,
					sourceObject);
			List<ObjectData> taregt = getSourceTargetRelationship(repositoryId, includeAllowableActions, userObject,
					targetObject);
			getSourceTargetList(sourceTarget, source);
			getSourceTargetList(sourceTarget, taregt);
			return sourceTarget;
		}
		return null;
	}

	private static List<ObjectData> getSourceTargetList(List<ObjectData> sourceTarget, List<ObjectData> data) {
		for (ObjectData dataST : data) {
			sourceTarget.add(dataST);
		}
		return sourceTarget;

	}

	public static List<ObjectData> getSourceTargetRelationship(String repositoryId,
			boolean includeAllowableActions, IUserObject userObject, List<? extends IRelationObject> relationships) {
		List<ObjectData> res = new ArrayList<ObjectData>();
		for (IRelationObject so : relationships) {
			ObjectData od = compileObjectData(repositoryId, so, null, includeAllowableActions, false, false, null,
					null, null, userObject, null, null);
			res.add(od);
		}
		if (res != null) {
			LOG.debug("SourceTargetRelationship result data count: {}", res.size());
		}
		return res;
	}
}

	public static Map<String, IBaseObject> getRelationshipObjects(String repositoryId,
			Map<String, Object> relationshipIds, String typeId) {
		Map<String, IBaseObject> result = new HashMap<>();
		Map<String, Object> custom = relationshipIds;
		custom = custom.entrySet().stream()
				.filter(map -> (!(map.getKey().equalsIgnoreCase(PropertyIds.NAME)
						|| map.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFIED_BY)
						|| map.getKey().equalsIgnoreCase(PropertyIds.OBJECT_TYPE_ID)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CREATED_BY)
						|| map.getKey().equalsIgnoreCase(PropertyIds.PATH)
						|| map.getKey().equalsIgnoreCase(PropertyIds.DESCRIPTION)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CHANGE_TOKEN)
						|| map.getKey().equalsIgnoreCase(PropertyIds.ALLOWED_CHILD_OBJECT_TYPE_IDS)
						|| map.getKey().equalsIgnoreCase(PropertyIds.PARENT_ID)
						|| map.getKey().equalsIgnoreCase(PropertyIds.BASE_TYPE_ID)
						|| map.getKey().equalsIgnoreCase(PropertyIds.OBJECT_ID)
						|| map.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFICATION_DATE)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CREATION_DATE)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_LENGTH)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_FILE_NAME)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_MIME_TYPE)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CHECKIN_COMMENT)
						|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_BY)
						|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_LABEL)
						|| map.getKey().equalsIgnoreCase(PropertyIds.IS_MAJOR_VERSION)
						|| map.getKey().equalsIgnoreCase(PropertyIds.IS_LATEST_VERSION)
						|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_ID)
						|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_ID)
						|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_ID)
						|| map.getKey().equalsIgnoreCase(PropertyIds.IS_IMMUTABLE))))
				.collect(Collectors.toMap(p -> p.getKey(), p -> p.getValue()));
		Set<Map.Entry<String, Object>> data = custom.entrySet();
		for (Map.Entry<String, Object> propertiesValues : data) {
			String id = propertiesValues.getKey();
			Object valueName = propertiesValues.getValue();
			if (id.equalsIgnoreCase(PropertyIds.SOURCE_ID)) {
				IBaseObject object = getObjectById(repositoryId, valueName.toString(), typeId);
				result.put(id, object);
			} else if (id.equalsIgnoreCase(PropertyIds.TARGET_ID)) {
				IBaseObject object = getObjectById(repositoryId, valueName.toString(), typeId);
				result.put(id, object);
			}
		}
		return result;
	}

	public static IBaseObject getObjectById(String repositoryId, String objectId, String typeId) {
		// check id
		if (objectId == null) {
			LOG.error("Method name: {}, unknown object id: {}, repositoryId: {}", "getObjectById", objectId,
					repositoryId);
			throw new CmisInvalidArgumentException("Object Id must be set.");
		}
		IBaseObject data = null;
		try {
			data = DBUtils.BaseDAO.getByObjectId(repositoryId, null, false, objectId, null, typeId);
		} catch (Exception e) {
			LOG.error("getObjectById Exception: {}, repositoryId: {}", ExceptionUtils.getStackTrace(e),
					repositoryId);
			throw new MongoException(e.toString());
		}

		if (data == null) {
			LOG.error("Method name:{}, object must not be null: {}, repositoryId: {}", "getObjectById", objectId,
					repositoryId);
			throw new CmisObjectNotFoundException("Object must not be null!");
		}

		if (data != null) {
			LOG.debug("getObjectById result object: {}, repositoryId: {}", data, repositoryId);
		}
		return data;
	}

	@SuppressWarnings("unchecked")
	private static void readCustomProperties(String repositoryId, IRelationObject data, PropertiesImpl props,
			TypeDefinition typeId, Set<String> filter, ITypePermissionService typePermissionFlow,
			IUserObject userObject) {
		HashMap<String, Object> customProps = new HashMap<String, Object>();
		data.getProperties().entrySet().forEach(map -> {
			if (!(map.getKey().equalsIgnoreCase(PropertyIds.NAME)
					|| map.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFIED_BY)
					|| map.getKey().equalsIgnoreCase(PropertyIds.OBJECT_TYPE_ID)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CREATED_BY)
					|| map.getKey().equalsIgnoreCase(PropertyIds.PATH)
					|| map.getKey().equalsIgnoreCase(PropertyIds.DESCRIPTION)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CHANGE_TOKEN)
					|| map.getKey().equalsIgnoreCase(PropertyIds.ALLOWED_CHILD_OBJECT_TYPE_IDS)
					|| map.getKey().equalsIgnoreCase(PropertyIds.PARENT_ID)
					|| map.getKey().equalsIgnoreCase(PropertyIds.BASE_TYPE_ID)
					|| map.getKey().equalsIgnoreCase(PropertyIds.OBJECT_ID)
					|| map.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFICATION_DATE)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CREATION_DATE)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_LENGTH)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_FILE_NAME)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_MIME_TYPE)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CHECKIN_COMMENT)
					|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_BY)
					|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_LABEL)
					|| map.getKey().equalsIgnoreCase(PropertyIds.IS_MAJOR_VERSION)
					|| map.getKey().equalsIgnoreCase(PropertyIds.IS_LATEST_VERSION)
					|| map.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_ID)
					|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_ID)
					|| map.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_ID)
					|| map.getKey().equalsIgnoreCase(PropertyIds.IS_IMMUTABLE))) {

				if (typeId.getPropertyDefinitions().get(map.getKey()) == null) {
					LOG.error("Method name: {}, unknown propertiesTypes: {}, repositoryId: {}", "readCustomProperties",
							map.getKey(), repositoryId);
					throw new IllegalArgumentException("Property '" + map.getKey() + "' is unknown!");
				} else {
					customProps.put(map.getKey(), typeId.getPropertyDefinitions().get(map.getKey()).getPropertyType());
				}
			}
		});

		IObjectEncryptService encryptService = EncryptionFactory.createEncryptionService(repositoryId);
		if (customProps.size() > 0) {
			Set<Map.Entry<String, Object>> customData = customProps.entrySet();
			for (Map.Entry<String, Object> customValues : customData) {
				String id = customValues.getKey();
				if (!(customValues.getKey().equals(PropertyIds.SECONDARY_OBJECT_TYPE_IDS))) {
					Object valueOfType = data.getProperties().get(id);
					PropertyType propertyType = (PropertyType) customValues.getValue();
					valueOfType = invokeDecryptAfterCreate(encryptService, repositoryId, EncryptType.DECRYPT,
							typeId.getId(), id, valueOfType, propertyType, null,
							data.getProperties());
					if (propertyType == PropertyType.INTEGER) {
						if (valueOfType instanceof Integer) {
							Integer valueBigInteger = convertInstanceOfObject(valueOfType, Integer.class);
							addPropertyBigInteger(repositoryId, props, typeId, filter, id,
									BigInteger.valueOf(valueBigInteger), userObject);
						} else if (valueOfType instanceof Long) {
							Long valueBigInteger = convertInstanceOfObject(valueOfType, Long.class);
							addPropertyBigInteger(repositoryId, props, typeId, filter, id,
									BigInteger.valueOf(valueBigInteger), userObject);
						} else if (valueOfType instanceof List<?>) {
							List<BigInteger> value = convertInstanceOfObject(valueOfType, List.class);
							addPropertyBigInteger(repositoryId, props, typeId, filter, id, value, userObject);
						}
					} else if (propertyType == PropertyType.BOOLEAN) {
						if (valueOfType instanceof Boolean) {
							Boolean booleanValue = convertInstanceOfObject(valueOfType, Boolean.class);
							addPropertyBoolean(repositoryId, props, typeId, filter, id, booleanValue, userObject);
						} else if (valueOfType instanceof List<?>) {
							List<Boolean> booleanValue = convertInstanceOfObject(valueOfType, List.class);
							addPropertyBoolean(repositoryId, props, typeId, filter, id, booleanValue, userObject);
						}
					} else if (propertyType == PropertyType.ID) {
						if (valueOfType instanceof String) {
							String value = convertInstanceOfObject(valueOfType, String.class);
							addPropertyId(repositoryId, props, typeId, filter, id, value, userObject);
						} else if (valueOfType instanceof List<?>) {
							List<String> value = convertInstanceOfObject(valueOfType, List.class);
							addPropertyId(repositoryId, props, typeId, filter, id, value, userObject);
						}

					} else if (propertyType == PropertyType.DATETIME) {
						if (valueOfType instanceof GregorianCalendar) {
							Long value = convertInstanceOfObject(valueOfType, Long.class);
							GregorianCalendar lastModifiedCalender = new GregorianCalendar();
							lastModifiedCalender.setTimeInMillis(value);
							addPropertyDateTime(repositoryId, props, typeId, filter, id, lastModifiedCalender,
									userObject);
						} else if (valueOfType instanceof List<?>) {
							List<Long> value = convertInstanceOfObject(valueOfType, List.class);
							List<GregorianCalendar> calenderList = new ArrayList<>();
							value.forEach(v -> {
								GregorianCalendar lastModifiedCalender = new GregorianCalendar();
								lastModifiedCalender.setTimeInMillis(v);
								calenderList.add(lastModifiedCalender);
							});
							addPropertyDateTime(repositoryId, props, typeId, filter, id, calenderList, userObject);
						} else {
							Long value = convertInstanceOfObject(valueOfType, Long.class);
							if (value != null) {
								GregorianCalendar lastModifiedCalender = new GregorianCalendar();
								lastModifiedCalender.setTimeInMillis(value);
								addPropertyDateTime(repositoryId, props, typeId, filter, id, lastModifiedCalender,
										userObject);
							}
						}

					} else if (propertyType == PropertyType.DECIMAL) {
						if (valueOfType instanceof Double) {
							Double value = convertInstanceOfObject(valueOfType, Double.class);
							addPropertyBigDecimal(repositoryId, props, typeId, filter, id,
									BigDecimal.valueOf(value), userObject);
						} else if (valueOfType instanceof List<?>) {
							List<BigDecimal> value = convertInstanceOfObject(valueOfType, List.class);
							addPropertyBigDecimal(repositoryId, props, typeId, filter, id, value, userObject);
						}

					} else if (propertyType == PropertyType.HTML) {
						if (valueOfType instanceof String) {
							String value = convertInstanceOfObject(valueOfType, String.class);
							String decodedValue = StringEscapeUtils.unescapeHtml4(value);
							addPropertyHtml(repositoryId, props, typeId, filter, id, decodedValue, userObject);
						} else if (valueOfType instanceof List<?>) {
							List<String> value = convertInstanceOfObject(valueOfType, List.class);
							List<String> decodedValue = new ArrayList<>();
							value.forEach(v -> {
								decodedValue.add(StringEscapeUtils.unescapeHtml4(v));
							});
							addPropertyHtml(repositoryId, props, typeId, filter, id, decodedValue, userObject);
						}

					} else if (propertyType == PropertyType.STRING) {
						if (valueOfType instanceof String) {
							String value = convertInstanceOfObject(valueOfType, String.class);
							addPropertyString(repositoryId, props, typeId, filter, id, value, userObject);
						} else if (valueOfType instanceof List<?>) {
							List<String> value = convertInstanceOfObject(valueOfType, List.class);
							addPropertyString(repositoryId, props, typeId, filter, id, value, userObject);
						}
					} else if (propertyType == PropertyType.URI) {
						if (valueOfType instanceof String) {
							String value = convertInstanceOfObject(valueOfType, String.class);
							addPropertyUri(repositoryId, props, typeId, filter, id, value, userObject);
						} else if (valueOfType instanceof List<?>) {
							List<String> value = convertInstanceOfObject(valueOfType, List.class);
							addPropertyUri(repositoryId, props, typeId, filter, id, value, userObject);
						}

					}
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private static Object convertDecryptProperties(Object valueOfType, PropertyType propertyType) {

		if (propertyType == PropertyType.INTEGER) {
			if (valueOfType instanceof String) {
				valueOfType = Integer.valueOf((String) valueOfType);
			} else if (valueOfType instanceof List<?>) {
				List<Integer> values = ((ArrayList<String>) valueOfType).stream()
						.map(v -> Integer.valueOf((String) v)).collect(Collectors.toList());
				valueOfType = values;
			}
		} else if (propertyType == PropertyType.BOOLEAN) {
			if (valueOfType instanceof String) {
				valueOfType = Boolean.valueOf((String) valueOfType);
			} else if (valueOfType instanceof List<?>) {
				List<Boolean> values = ((ArrayList<String>) valueOfType).stream()
						.map(v -> Boolean.valueOf((String) v)).collect(Collectors.toList());
				valueOfType = values;
			}
		} else if (propertyType == PropertyType.DATETIME) {
			if (valueOfType instanceof String) {
				GregorianCalendar gc = new GregorianCalendar();
				gc.setTimeInMillis(Long.valueOf((String) valueOfType));
				valueOfType = gc;
			} else if (valueOfType instanceof List<?>) {
				List<GregorianCalendar> calenderList = new ArrayList<>();
				((ArrayList<String>) valueOfType).forEach(v -> {
					GregorianCalendar lastModifiedCalender = new GregorianCalendar();
					lastModifiedCalender.setTimeInMillis(Long.valueOf((String) v));
					calenderList.add(lastModifiedCalender);
				});
			}
		} else if (propertyType == PropertyType.DECIMAL) {
			if (valueOfType instanceof Integer) {
				int intValue = ((Integer) valueOfType).intValue();
				valueOfType = new Double(intValue);
			} else if (valueOfType instanceof String) {
				valueOfType = Double.valueOf((String) valueOfType);
			} else if (valueOfType instanceof List<?>) {
				List<Double> values = ((ArrayList<String>) valueOfType).stream()
						.map(v -> Double.valueOf((String) v)).collect(Collectors.toList());
				valueOfType = values;
			}
		}
		return valueOfType;
	}

	@SuppressWarnings("unchecked")
	private static void addPropertyId(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}

		PropertyIdImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyIdImpl(id, (List<String>) value);
		} else {
			pd = new PropertyIdImpl(id, (String) value);
		}
		pd.setDisplayName(id);
		pd.setQueryName(id);

		props.addProperty(pd);
		if (id != null) {
			LOG.debug("Added propertyId: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * Adding the propertyString properties
	 */
	@SuppressWarnings("unchecked")
	private static void addPropertyString(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}
		// props.addProperty(new PropertyStringImpl(id, value));
		PropertyStringImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyStringImpl(id, (List<String>) value);
		} else {
			pd = new PropertyStringImpl(id, (String) value);
		}
		pd.setDisplayName(id);
		pd.setQueryName(id);
		props.addProperty(pd);

		if (id != null) {
			LOG.debug("Added propertyString: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * Adding the propertyURI properties
	 */
	@SuppressWarnings("unchecked")
	private static void addPropertyUri(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}
		PropertyUriImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyUriImpl(id, (List<String>) value);
		} else {
			pd = new PropertyUriImpl(id, (String) value);
		}
		pd.setDisplayName(id);
		pd.setQueryName(id);
		props.addProperty(pd);

		if (id != null) {
			LOG.debug("Added propertyUri: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * Adding the propertyHTML properties
	 */
	@SuppressWarnings("unchecked")
	private static void addPropertyHtml(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}
		PropertyHtmlImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyHtmlImpl(id, (List<String>) value);
		} else {
			pd = new PropertyHtmlImpl(id, (String) value);
		}
		pd.setDisplayName(id);
		pd.setQueryName(id);
		props.addProperty(pd);

		if (id != null) {
			LOG.debug("Added propertyHtml: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * Adding the propertyBigInteger properties
	 */
	@SuppressWarnings("unchecked")
	private static void addPropertyBigInteger(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}

		// props.addProperty(new PropertyIntegerImpl(id, value));
		PropertyIntegerImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyIntegerImpl(id, (List<BigInteger>) value);
		} else {
			pd = new PropertyIntegerImpl(id, (BigInteger) value);
		}

		pd.setDisplayName(id);
		pd.setQueryName(id);

		props.addProperty(pd);

		if (id != null) {
			LOG.debug("Added propertyBigInteger: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * Adding the propertyBigDecimal properties
	 */
	@SuppressWarnings("unchecked")
	private static void addPropertyBigDecimal(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}

		// props.addProperty(new PropertyIntegerImpl(id, value));
		PropertyDecimalImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyDecimalImpl(id, (List<BigDecimal>) value);
		} else {
			pd = new PropertyDecimalImpl(id, (BigDecimal) value);
		}

		pd.setDisplayName(id);
		pd.setQueryName(id);

		props.addProperty(pd);

		if (id != null) {
			LOG.debug("Added propertyBigDecimal: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * Adding the propertyBoolean properties
	 */
	@SuppressWarnings("unchecked")
	private static void addPropertyBoolean(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}

		// props.addProperty(new PropertyBooleanImpl(id, value));
		PropertyBooleanImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyBooleanImpl(id, (List<Boolean>) value);
		} else {
			pd = new PropertyBooleanImpl(id, (Boolean) value);
		}

		pd.setDisplayName(id);
		pd.setQueryName(id);

		props.addProperty(pd);

		if (id != null) {
			LOG.debug("Added propertyBoolean: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * Adding the propertyDateTime properties
	 */
	@SuppressWarnings("unchecked")
	private static void addPropertyDateTime(String repositoryId, PropertiesImpl props, TypeDefinition typeId,
			Set<String> filter, String id, Object value, IUserObject userObject) {
		if (!checkAddProperty(repositoryId, props, typeId, filter, id, userObject)) {
			return;
		}

		// props.addProperty(new PropertyDateTimeImpl(id, value));
		PropertyDateTimeImpl pd = null;
		if (value instanceof List) {
			pd = new PropertyDateTimeImpl(id, (List<GregorianCalendar>) value);
		} else {
			pd = new PropertyDateTimeImpl(id, (GregorianCalendar) value);
		}

		pd.setDisplayName(id);
		pd.setQueryName(id);

		props.addProperty(pd);

		if (id != null) {
			LOG.debug("Added propertyDateTime: {}, repository: {}", id, repositoryId);
		}
	}

	/**
	 * CheckingProperty
	 */
	@SuppressWarnings({ "unchecked", "rawtypes", "null" })
	private static boolean checkAddProperty(String repositoryId, Properties properties, TypeDefinition type,
			Set<String> filter, String id, IUserObject userObject) {
		if (properties == null || properties.getProperties() == null) {
			LOG.error("Method name: {}, unknown properties: {}, repositoryId: {}", "checkAddProperty", properties,
					repositoryId);
			throw new IllegalArgumentException("Properties must not be null!");
		}

		if (id == null) {
			LOG.error("Unknown object id: {}, repositoryId: {}", id, repositoryId);
			throw new IllegalArgumentException("Id must not be null!");
		}

		if (type == null) {
			LOG.error("Unknown typeId: {}, repositoryId: {}", type, repositoryId);
			throw new IllegalArgumentException("Unknown type: " + type.getId());
		}

		List<String> secondaryObjectTypeIds = null;
		PropertyData<?> secondaryObjectType = properties.getProperties().get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
		if (secondaryObjectType != null) {
			secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
		}

		String queryName = null;
		if (id.equalsIgnoreCase(PropertyIds.PATH)) {
			queryName = PropertyIds.PATH;
		} else {
			if (type.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = type.getPropertyDefinitions();
				for (PropertyDefinition<?> pro : property.values()) {
					if (pro.getId().equals(id)) {
						queryName = pro.getQueryName();
						break;
					}
				}
				if (secondaryObjectTypeIds != null) {
					for (String typeId : secondaryObjectTypeIds) {
						TypeDefinition types = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null,
								userObject, null, null);
						Map<String, PropertyDefinition<?>> secondaryProperty = types.getPropertyDefinitions();
						for (PropertyDefinition pros : secondaryProperty.values()) {
							if (pros.getId().equals(id)) {
								queryName = pros.getQueryName();
								break;
							}
						}
					}
				}
			}
		}

		if (queryName == null) {
			if (secondaryObjectTypeIds == null) {
				return false;
			} else {
				LOG.error("Unknown property: {}, repositoryId: {}", id, repositoryId);
				throw new IllegalArgumentException("Unknown property: " + id);
			}
		}

		if (queryName != null && filter != null) {
			if (!filter.contains(queryName)) {
				return false;
			} else {
				filter.remove(queryName);
			}
		}

		return true;
	}

	/**
	 * fill the corresponding information Links in Object data
	 */
	public static void fillInformationForAtomLinks(String repositoryId, ObjectData od, IBaseObject so,
			ObjectInfoImpl objectInfo, IUserObject userobject) {

		fillInformationForAtomLinks(repositoryId, so, od, objectInfo, userobject);
	}

	/**
	 * fill the corresponding information Links in Object data
	 */
	public static void fillInformationForAtomLinks(String repositoryId, IBaseObject so, ObjectData od,
			ObjectInfoImpl objInfo, IUserObject userobject) {
		if (null == objInfo || null == so) {
			return;
		}
		// boolean cmis11 = callContext.getCmisVersion() !=
		// CmisVersion.CMIS_1_0;
		TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, so.getTypeId(), null,
				userobject, null, null);

		// Fill all setters:
		objInfo.setId(so.getId().toString());
		objInfo.setName(so.getName());
		objInfo.setCreatedBy(so.getCreatedBy()); // !
		GregorianCalendar lastCreationDate = new GregorianCalendar();
		lastCreationDate.setTimeInMillis(so.getCreatedAt());
		objInfo.setCreationDate(lastCreationDate); // !
		GregorianCalendar lastModifiedCalender = new GregorianCalendar();
		lastModifiedCalender.setTimeInMillis(so.getModifiedAt());
		objInfo.setLastModificationDate(lastModifiedCalender);
		objInfo.setTypeId(so.getTypeId());
		objInfo.setBaseType(typeDef.getBaseTypeId());
		objInfo.setObject(od);

//		List<RenditionData> renditions = getRenditions(repositoryId, so, null, null, BigInteger.ZERO,
//				BigInteger.ZERO, null, so.getTypeId(), null, null);
//		if (renditions == null || renditions.size() == 0) {
//			objInfo.setRenditionInfos(null);
//		} else {
//			List<RenditionInfo> infos = new ArrayList<RenditionInfo>();
//			for (RenditionData rendition : renditions) {
//				RenditionInfoImpl info = new RenditionInfoImpl();
//				info.setKind(rendition.getKind());
//				info.setId(rendition.getStreamId());
//				info.setContentType(rendition.getMimeType());
//				info.setLength(rendition.getBigLength());
//				info.setTitle(rendition.getTitle());
//				infos.add(info);
//			}
//			objInfo.setRenditionInfos(infos);
//		}

		// Relationships
		objInfo.setSupportsRelationships(true);
		Map<String, Object> propDefs = so.getProperties();
		List<String> srcIds = new ArrayList<String>();
		List<String> targetIds = new ArrayList<String>();
		Map<String, IBaseObject> rels = getRelationshipObjects(repositoryId, propDefs, so.getTypeId());
		Set<Map.Entry<String, IBaseObject>> data = rels.entrySet();
		for (Map.Entry<String, IBaseObject> objectValues : data) {
			String id = objectValues.getKey();

			if (id.equalsIgnoreCase(PropertyIds.SOURCE_ID)) {
				srcIds.add(id.toString());

			} else if (id.equalsIgnoreCase(PropertyIds.TARGET_ID)) {
				targetIds.add(id.toString());
			}
		}
		objInfo.setRelationshipSourceIds(srcIds);
		objInfo.setRelationshipTargetIds(targetIds);

		objInfo.setSupportsPolicies(true);

		objInfo.setHasAcl(true);

		objInfo.setSupportsDescendants(true);
		objInfo.setSupportsFolderTree(true);
	}

	/**
	 * Adds the default value of property if defined.
	 */
	@SuppressWarnings({ "unchecked", "unused" })
	private static boolean addPropertyDefault(PropertiesImpl props, PropertyDefinition<?> propDef) {
		if (props == null || props.getProperties() == null) {
			LOG.error("Unknown properties: {}", props);
			throw new IllegalArgumentException("Props must not be null!");
		}

		if (propDef == null) {
			return false;
		}
		LOG.info("Adding the default value of properties: {}", props);
		List<?> defaultValue = propDef.getDefaultValue();
		if (defaultValue != null && !defaultValue.isEmpty()) {
			switch (propDef.getPropertyType()) {
			case BOOLEAN:
				props.addProperty(new PropertyBooleanImpl(propDef.getId(), (List<Boolean>) defaultValue));
				break;
			case DATETIME:
				props.addProperty(
						new PropertyDateTimeImpl(propDef.getId(), (List<GregorianCalendar>) defaultValue));
				break;
			case DECIMAL:
				props.addProperty(new PropertyDecimalImpl(propDef.getId(), (List<BigDecimal>) defaultValue));
				break;
			case HTML:
				props.addProperty(new PropertyHtmlImpl(propDef.getId(), (List<String>) defaultValue));
				break;
			case ID:
				props.addProperty(new PropertyIdImpl(propDef.getId(), (List<String>) defaultValue));
				break;
			case INTEGER:
				props.addProperty(new PropertyIntegerImpl(propDef.getId(), (List<BigInteger>) defaultValue));
				break;
			case STRING:
				props.addProperty(new PropertyStringImpl(propDef.getId(), (List<String>) defaultValue));
				break;
			case URI:
				props.addProperty(new PropertyUriImpl(propDef.getId(), (List<String>) defaultValue));
				break;
			default:
				assert false;
			}

			return true;
		}

		return false;
	}
	
	private static Object invokeDecryptAfterCreate(IObjectEncryptService objectFlowService, String repositoryId,
			EncryptType invokeMethod, String typeId, String propId, Object propValue, PropertyType propertyType,
			List<String> secondaryObjectTypeIdsValues, Map<String, Object> customProps) {
		if (objectFlowService != null) {
			try {
				if (EncryptType.DECRYPT.equals(invokeMethod)) {
					LOG.info("invokeEncryptBeforeCreate, InvokeMethod: {}", invokeMethod);
					if (objectFlowService
							.shouldEncrypt(repositoryId, typeId, propId, secondaryObjectTypeIdsValues,
									customProps.get(FV_ENCRYPT_PROPS) != null
											? customProps.get(FV_ENCRYPT_PROPS) instanceof String
													? Arrays.asList(customProps.get(FV_ENCRYPT_PROPS).toString())
													: (List<String>) customProps.get(FV_ENCRYPT_PROPS)
											: null)) {
						propValue = objectFlowService
								.decrypt(repositoryId, typeId, propId, propValue, secondaryObjectTypeIdsValues,
										customProps.get(FV_ENCRYPT_PROPS) != null
												? customProps.get(FV_ENCRYPT_PROPS) instanceof String
														? Arrays.asList(
																customProps.get(FV_ENCRYPT_PROPS).toString())
														: (List<String>) customProps.get(FV_ENCRYPT_PROPS)
												: null);
						propValue = convertDecryptProperties(propValue, propertyType);
					}
				}
			} catch (Exception ex) {
				LOG.error("Operation failed with ObjectFlowService for InvokeMethod: {}, with exception: {}",
						invokeMethod, ex.getMessage());
				throw new IllegalArgumentException(ex.getMessage());
			}
		}
		return propValue;
	}
	
	private static <T> T convertInstanceOfObject(Object o, Class<T> clazz) {
		try {
			return clazz.cast(o);
		} catch (ClassCastException e) {
			return null;
		}
	}



}
