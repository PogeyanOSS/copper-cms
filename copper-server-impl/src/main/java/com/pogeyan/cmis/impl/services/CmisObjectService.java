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

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.AllowableActions;
import org.apache.chemistry.opencmis.commons.data.BulkUpdateObjectIdAndChangeToken;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.FailedToDeleteData;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.data.PropertyId;
import org.apache.chemistry.opencmis.commons.data.RenditionData;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.Action;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.UnfileObject;
import org.apache.chemistry.opencmis.commons.enums.VersioningState;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConstraintException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisStorageException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUpdateConflictException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AllowableActionsImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.BulkUpdateObjectIdAndChangeTokenImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChangeEventInfoDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.FailedToDeleteDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectDataImpl;
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
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.apache.chemistry.opencmis.server.support.TypeValidator;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.CustomTypeId;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.IObjectEncryptService;
import com.pogeyan.cmis.api.data.IObjectFlowFactory;
import com.pogeyan.cmis.api.data.IObjectFlowService;
import com.pogeyan.cmis.api.data.IRelationObject;
import com.pogeyan.cmis.api.data.ISettableBaseObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.EncryptType;
import com.pogeyan.cmis.api.data.common.ObjectFlowType;
import com.pogeyan.cmis.api.data.common.PermissionType;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.data.services.MNavigationDocServiceDAO;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.api.data.services.MRelationObjectDAO;
import com.pogeyan.cmis.api.repo.CopperCmsRepository;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.api.storage.IStorageService;
import com.pogeyan.cmis.api.uri.exception.CmisRoleValidationException;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.api.utils.TracingWriter;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.impl.factory.ObjectFlowFactory;
import com.pogeyan.cmis.impl.factory.StorageServiceFactory;
import com.pogeyan.cmis.impl.factory.TypeServiceFactory;
import com.pogeyan.cmis.impl.utils.CmisPropertyConverter;
import com.pogeyan.cmis.impl.utils.CmisUtils;
import com.pogeyan.cmis.impl.utils.DBUtils;
import com.pogeyan.cmis.impl.utils.NameValidator;
import com.pogeyan.cmis.impl.utils.TypeValidators;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

import scala.Tuple2;

public class CmisObjectService {
	private static final Logger LOG = LoggerFactory.getLogger(CmisObjectService.class);
	public static String RELATION_NAME = "relation_name";
	public static final String FV_ENCRYPT_PROPS = "fv:encryptProperties";

	public static class Impl {

		/**
		 * Adding RootFolder into MongoDB
		 */
		public static String addRootFolder(String repositoryId, String userName, String typeId, String tracingId,
				ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::addRootFolder", null);
			try {
				MBaseObjectDAO objectDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MBaseObjectDAO.class);
				IBaseObject rootData = DBUtils.BaseDAO.getRootFolder(repositoryId, typeId, false);
				if (rootData != null) {
					LOG.info("Root folderId: {}, already created for repository: {}", rootData.getId(), repositoryId);
					return rootData.getId();
				} else {
					TokenImpl token = new TokenImpl(TokenChangeType.CREATED, System.currentTimeMillis());
					AccessControlListImplExt aclImp = new AccessControlListImplExt(new ArrayList<>(),
							AclPropagation.REPOSITORYDETERMINED.toString(), true);
					IBaseObject folderObject = objectDAO.createObjectFacade(CopperCmsRepository.ROOT_ID,
							BaseTypeId.CMIS_FOLDER, "cmis:folder", repositoryId, null,
							"Pogeyan MongoDB CMIS Repository", userName, userName, token, ",", null, null, aclImp, "/",
							null);
					objectDAO.commit(folderObject, typeId);
					LOG.info("Root folder created in Database: {} , repository: {} ",
							folderObject != null ? folderObject.getId() : null, repositoryId);
					addRootFolder(repositoryId);
					return folderObject.getId();
				}
			} catch (MongoException e) {

				LOG.error("addRootFolder exception: {}, repositoryId: {},  TraceId: {}",
						ExceptionUtils.getStackTrace(e), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span),
								ErrorMessages.BASE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			}
			
			return null;
		}

		public static ObjectData getSimpleObject(String repositoryId, String objectId, IUserObject userObject,
				BaseTypeId baseTypeId, String typeId) {
			return getObject(repositoryId, objectId, null, false, IncludeRelationships.NONE, "cmis:none", false, true,
					null, userObject, baseTypeId, typeId, null, null);
		}

		public static ObjectData getObject(String repositoryId, String objectId, String filter,
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
			Set<String> filterCollection = splitFilter(filter);
			if (filter != null && filterCollection != null && filterCollection.size() > 0) {
				filterArray = Helpers.getFilterArray(filterCollection, baseTypeId != BaseTypeId.CMIS_DOCUMENT);
			}
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			IBaseObject data = null;
			try {
				if (baseTypeId == null || baseTypeId != BaseTypeId.CMIS_DOCUMENT) {
					data = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, objectId,
							filterArray, typeId);
				} else {
					data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId, null);
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

		public static ObjectData getObjectForRestAPI(String repositoryId, String typeId, String primaryKeyField,
				String primaryKeyValue, String filter, Boolean includeAllowableActions,
				IncludeRelationships includeRelationships, String renditionFilter, Boolean includePolicyIds,
				Boolean includeAcl, ObjectInfoHandler objectInfos, IUserObject userObject)
				throws CmisInvalidArgumentException, IllegalArgumentException, CmisObjectNotFoundException {

			LOG.info("Method name: {}, ObjectInfo- typeId: {}, repository: {}, primaryKey: {}", "getObjectForRestAPI",
					typeId, repositoryId, primaryKeyField);

			if (typeId == null && filter == null) {
				LOG.error("getObjectForRestAPI objectId and filter is null in repository: {}", repositoryId);
				throw new CmisInvalidArgumentException("Object Id should not be null");
			} else if (typeId == null) {
				LOG.error("getObjectForRestAPI objectId is null in repositoryId: {}", repositoryId);
				throw new CmisInvalidArgumentException("Object Id should not be null");
			}

			String[] filterArray = null;
			// split filter
			Set<String> filterCollection = splitFilter(filter);
			if (filter != null && filterCollection != null && filterCollection.size() > 0) {
				filterArray = Helpers.getFilterArray(filterCollection, true);
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			TypeDefinition type = CmisTypeServices.Impl
					.getTypeDefinitionWithTypePermission(typePermissionFlow, repositoryId, userObject, typeId).get(0);
			IBaseObject data = null;
			try {
				if (primaryKeyField.equals(PropertyIds.OBJECT_ID)) {
					data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, primaryKeyValue, filterArray);
				} else {
					data = DBUtils.DocumentDAO.getByDocumentByPropertiesField(repositoryId, typeId, primaryKeyField,
							checkPrimaryKeyType(primaryKeyValue, type.getPropertyDefinitions().get(primaryKeyField)),
							filterArray);
				}
			} catch (Exception e) {
				LOG.error("getObjectForRestAPI Exception: {}, repository: {}", ExceptionUtils.getStackTrace(e),
						repositoryId);
				throw new MongoException(e.toString());
			}
			if (data == null) {
				LOG.error("getObjectForRestAPI for primary key field: {}, is null in {} repository!", primaryKeyField,
						repositoryId);
				throw new CmisObjectNotFoundException("Object must not be null!");
			}

			// set defaults if values not set
			boolean iaa = getBooleanParameter(includeAllowableActions, false);
			boolean iacl = getBooleanParameter(includeAcl, false);

			// gather properties
			ObjectData od = compileObjectData(repositoryId, data, filterCollection, iaa, iacl, true, objectInfos,
					renditionFilter, includeRelationships, userObject, null, null);
			if (od != null) {
				LOG.debug("getObjectForRestAPI result data: {}", od.getProperties());
			}
			return od;
		}

		/**
		 * Returns the ObjectData for particular path
		 */
		public static ObjectData getObjectByPath(String repositoryId, String path, String filter,
				Boolean includeAllowableActions, IncludeRelationships includeRelationships, String renditionFilter,
				Boolean includePolicyIds, Boolean includeAcl, ObjectInfoHandler objectInfos, IUserObject userObject,
				String typeId, String tracingId, ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::getObjectByPath", null);
			// check id
			LOG.info("getObjectByPath compileObjectData for: {}, repositoryid: {}", path, repositoryId);
			if (path == null && filter == null) {
				LOG.error("getObjectByPath unknown object id: {}, repository: {}, TraceId: {}", path, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.OBJECTID_MUST_BE_SET), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.OBJECTID_MUST_BE_SET), span));
			}

			IBaseObject data = null;
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			try {
				String[] principalIds = Helpers.getPrincipalIds(userObject);
				data = DBUtils.BaseDAO.getByPath(repositoryId, principalIds, aclPropagation, path, typeId);
			} catch (Exception e) {
				LOG.error("getObjectByPath Exception: {}, repository: {}, TraceId: {}", ExceptionUtils.getStackTrace(e),
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span),
								ErrorMessages.MONGO_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new MongoException(TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span));
			}

			if (data == null) {
				LOG.error("getObjectByPath: {}, is null in {} repository!, TraceId: {}", path, repositoryId,
						span != null ? span.getTraceId() : null);
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

			// split filter
			Set<String> filterCollection = splitFilter(filter);

			// gather properties

			ObjectData od = compileObjectData(repositoryId, data, filterCollection, iaa, iacl, true, objectInfos,
					renditionFilter, includeRelationships, userObject, tracingId, span);

			// ObjectInfoImpl objectInfo = new ObjectInfoImpl();
			// fillInformationForAtomLinks(repositoryId, od, data, objectInfo);
			// objectInfos.addObjectInfo(objectInfo);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return od;
		}

		public static ObjectData compileObjectData(String repositoryId, IBaseObject data, Set<String> filter,
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
			List<RenditionData> renditions = getRenditions(repositoryId, data, null, renditionFilter,
					BigInteger.valueOf(1), BigInteger.valueOf(0), userObject == null ? null : userObject.getUserDN(),
					data.getTypeId(), tracingId, span);
			result.setRenditions(renditions);

			if (null != includeRelationships && includeRelationships != IncludeRelationships.NONE) {
				result.setRelationships(fillRelationships(repositoryId, includeAllowableActions, includeRelationships,
						data, userObject, null));
			}

			if (data.getChangeToken() != null) {
				GregorianCalendar changeEventDate = new GregorianCalendar();
				changeEventDate.setTimeInMillis(data.getChangeToken().getTime());

				ChangeEventInfoDataImpl changeEvent = new ChangeEventInfoDataImpl(
						TokenChangeType.fromValue(data.getChangeToken().getChangeType()), changeEventDate);
				result.setChangeEventInfo(changeEvent);
			}

			if (data.getPolicies() != null) {
				PolicyIdListImpl polIds = new PolicyIdListImpl();
				List<String> pols = data.getPolicies();
				polIds.setPolicyIds(pols);
				result.setPolicyIds(polIds);
			}

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

		public static Properties compileProperties(String repositoryId, IBaseObject data, Set<String> orgfilter,
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

			if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_FOLDER.value())
					|| data.getBaseId() == BaseTypeId.CMIS_FOLDER) {
				if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_FOLDER.value())) {
					typeId = BaseTypeId.CMIS_FOLDER.value();
				} else {
					typeId = data.getTypeId();
				}

				if (objectInfo != null) {
					objectInfo.setBaseType(BaseTypeId.CMIS_FOLDER);
					objectInfo.setTypeId(typeId);
					objectInfo.setContentType(null);
					objectInfo.setFileName(null);
					objectInfo.setHasAcl(true);
					objectInfo.setHasContent(false);
					objectInfo.setVersionSeriesId(null);
					objectInfo.setIsCurrentVersion(true);
					objectInfo.setRelationshipSourceIds(null);
					objectInfo.setRelationshipTargetIds(null);
					objectInfo.setRenditionInfos(null);
					objectInfo.setSupportsDescendants(true);
					objectInfo.setSupportsFolderTree(true);
					objectInfo.setSupportsPolicies(false);
					objectInfo.setSupportsRelationships(false);
					objectInfo.setWorkingCopyId(null);
					objectInfo.setWorkingCopyOriginalId(null);
				}

			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_DOCUMENT.value())
					|| data.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
				if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_DOCUMENT.value())) {
					typeId = BaseTypeId.CMIS_DOCUMENT.value();
				} else {
					typeId = data.getTypeId();
				}
				if (objectInfo != null) {
					objectInfo.setBaseType(BaseTypeId.CMIS_DOCUMENT);
					objectInfo.setTypeId(typeId);
					objectInfo.setHasAcl(true);
					objectInfo.setHasContent(true);
					objectInfo.setHasParent(true);
					objectInfo.setVersionSeriesId(null);
					objectInfo.setIsCurrentVersion(true);
					objectInfo.setRelationshipSourceIds(null);
					objectInfo.setRelationshipTargetIds(null);
					objectInfo.setRenditionInfos(null);
					objectInfo.setSupportsDescendants(false);
					objectInfo.setSupportsFolderTree(false);
					objectInfo.setSupportsPolicies(false);
					objectInfo.setSupportsRelationships(false);
					objectInfo.setWorkingCopyId(null);
					objectInfo.setWorkingCopyOriginalId(null);
				}
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_ITEM.value())
					|| data.getBaseId() == BaseTypeId.CMIS_ITEM) {
				if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_ITEM.value())) {
					typeId = BaseTypeId.CMIS_ITEM.value();
				} else {
					typeId = data.getTypeId();
				}
				if (objectInfo != null) {
					objectInfo.setBaseType(BaseTypeId.CMIS_ITEM);
					objectInfo.setTypeId(typeId);
					objectInfo.setContentType(null);
					objectInfo.setFileName(null);
					objectInfo.setHasAcl(true);
					objectInfo.setHasContent(false);
					objectInfo.setVersionSeriesId(null);
					objectInfo.setIsCurrentVersion(true);
					objectInfo.setRelationshipSourceIds(null);
					objectInfo.setRelationshipTargetIds(null);
					objectInfo.setRenditionInfos(null);
					objectInfo.setSupportsDescendants(true);
					objectInfo.setSupportsFolderTree(true);
					objectInfo.setSupportsPolicies(false);
					objectInfo.setSupportsRelationships(false);
					objectInfo.setWorkingCopyId(null);
					objectInfo.setWorkingCopyOriginalId(null);
				}
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_RELATIONSHIP.value())
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
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_POLICY.value())
					|| data.getBaseId() == BaseTypeId.CMIS_POLICY) {
				if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_POLICY.value())) {
					typeId = BaseTypeId.CMIS_POLICY.value();
				} else {
					typeId = data.getTypeId();
				}
				if (objectInfo != null) {
					objectInfo.setBaseType(BaseTypeId.CMIS_POLICY);
					objectInfo.setTypeId(typeId);
					objectInfo.setContentType(null);
					objectInfo.setFileName(null);
					objectInfo.setHasAcl(true);
					objectInfo.setHasContent(false);
					objectInfo.setVersionSeriesId(null);
					objectInfo.setIsCurrentVersion(true);
					objectInfo.setSupportsDescendants(true);
					objectInfo.setSupportsFolderTree(true);
					objectInfo.setSupportsPolicies(true);
					objectInfo.setSupportsRelationships(false);
					objectInfo.setWorkingCopyId(null);
					objectInfo.setWorkingCopyOriginalId(null);
				}
			} else if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_SECONDARY.value())
					|| data.getBaseId() == BaseTypeId.CMIS_SECONDARY) {
				if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_SECONDARY.value())) {
					typeId = BaseTypeId.CMIS_SECONDARY.value();
				} else {
					typeId = data.getTypeId();
				}
				if (objectInfo != null) {
					objectInfo.setBaseType(BaseTypeId.CMIS_SECONDARY);
					objectInfo.setTypeId(typeId);
					objectInfo.setContentType(null);
					objectInfo.setFileName(null);
					objectInfo.setHasAcl(false);
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

				addPropertyId(repositoryId, result, type, filter, PropertyIds.SECONDARY_OBJECT_TYPE_IDS,
						data.getSecondaryTypeIds() == null ? null : data.getSecondaryTypeIds(), userObject);

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
					addPropertyString(repositoryId, result, type, filter, PropertyIds.PATH,
							data.getPath() == null ? "" : data.getPath(), userObject);
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
						addPropertyString(repositoryId, result, type, filter, PropertyIds.PATH,
								data.getPath() == null ? "" : data.getPath(), userObject);
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

		public static AllowableActions getAllowableActions(String repositoryId, IBaseObject data, String objectId,
				IUserObject userObject, String typeId, String tracingId, ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::getAllowableActions", null);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;

			if (data == null && objectId != null) {
				try {
					String[] principalIds = Helpers.getPrincipalIds(userObject);
					data = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, objectId, null,
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

		private static AllowableActions fillAllowableActions(String repositoryId, IBaseObject so, String user) {
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

			if (isDocument || isItem) {
				if (so.getInternalPath() != null) {
					set.add(Action.CAN_ADD_OBJECT_TO_FOLDER);
					set.add(Action.CAN_REMOVE_OBJECT_FROM_FOLDER);
					set.add(Action.CAN_GET_PROPERTIES);
				}
				if (isDocument) {
					set.add(Action.CAN_SET_CONTENT_STREAM);
				}
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

		private static boolean getBooleanParameter(Boolean value, boolean def) {
			if (value == null) {
				return def;
			}

			return value.booleanValue();
		}

		public static List<RenditionData> getRenditions(String repositoryId, IBaseObject data, String objectId,
				String renditionFilter, BigInteger maxItems, BigInteger skipCount, String userName, String typeId,
				String tracingId, ISpan parentSpan) throws CmisInvalidArgumentException {
			// LOG.info("Method name: {}, checking renditions using this
			// objectId: {}",
			// "getRenditions", objectId);
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::getRenditions", null);
			if (data == null && objectId != null) {
				try {
					data = DBUtils.BaseDAO.getByObjectId(repositoryId, null, true, objectId, null, typeId);
				} catch (Exception e) {
					LOG.error("getRenditions Exception: {}, repositoryId: {}, TraceId: {}",
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
				LOG.error("getRenditions unknown objectId: {}, repositoryId: {}, TraceId: {}", objectId, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.OBJECTID_MUST_BE_SET), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.OBJECTID_MUST_BE_SET), span));
			}

			if (data == null) {
				LOG.error("getRenditions for this objectId: {}, is null in: {} repository!, TraceId: {}", objectId,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span));
			}

			List<RenditionData> renditions = null;
			if (data.getTypeId().equalsIgnoreCase(BaseTypeId.CMIS_DOCUMENT.value())) {
				renditions = checkRenditions(repositoryId, data, renditionFilter,
						maxItems == null ? 0 : maxItems.longValue(), skipCount == null ? 0 : skipCount.longValue(),
						userName);
			}
			if (renditions != null) {
				LOG.debug("Renditions for this objectId: {}, are: {}", objectId, renditions);
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return renditions;
		}

		public static List<RenditionData> checkRenditions(String repositoryId, IBaseObject so, String renditionFilter,
				long maxItems, long skipCount, String user) {

			return CmisUtils.Rendition.getRenditions(repositoryId, so, renditionFilter, maxItems, skipCount, user);
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
				return CmisRelationshipService.Impl.getSourceTargetRelationship(repositoryId, includeAllowableActions,
						userObject, source);
			} else if (includeRelationships == IncludeRelationships.TARGET) {
				List<? extends IRelationObject> target = DBUtils.RelationshipDAO.getRelationshipByTargetId(repositoryId,
						spo.getId().toString(), aclPropagation, maxItems, skipCount, mappedColumns, spo.getTypeId());
				return CmisRelationshipService.Impl.getSourceTargetRelationship(repositoryId, includeAllowableActions,
						userObject, target);
			} else if (includeRelationships == IncludeRelationships.BOTH) {
				List<ObjectData> sourceTarget = new ArrayList<>();
				List<? extends IRelationObject> sourceObject = DBUtils.RelationshipDAO.getRelationshipBySourceId(
						repositoryId, spo.getId().toString(), aclPropagation, maxItems, skipCount, mappedColumns,
						spo.getTypeId());
				List<? extends IRelationObject> targetObject = DBUtils.RelationshipDAO.getRelationshipByTargetId(
						repositoryId, spo.getId().toString(), aclPropagation, maxItems, skipCount, mappedColumns,
						spo.getTypeId());
				List<ObjectData> source = CmisRelationshipService.Impl.getSourceTargetRelationship(repositoryId,
						includeAllowableActions, userObject, sourceObject);
				List<ObjectData> taregt = CmisRelationshipService.Impl.getSourceTargetRelationship(repositoryId,
						includeAllowableActions, userObject, targetObject);
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
		private static void readCustomProperties(String repositoryId, IBaseObject data, PropertiesImpl props,
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
						if (data.getSecondaryTypeIds() != null) {
							List<? extends TypeDefinition> secondaryObject = CmisTypeServices.Impl
									.checkTypePermissionList(typePermissionFlow, repositoryId, userObject,
											data.getSecondaryTypeIds());
							secondaryObject.stream().collect(Collectors.toList()).forEach(e -> {
								Map<String, PropertyDefinition<?>> secondaryProperty = e.getPropertyDefinitions();
								secondaryProperty.entrySet().stream().collect(Collectors.toList()).forEach(t -> {
									if (t.getValue().getId().equals(map.getKey())) {
										customProps.put(t.getKey(), t.getValue().getPropertyType());
									}
								});
							});
						} else {
							LOG.error("Method name: {}, unknown propertiesTypes: {}, repositoryId: {}",
									"readCustomProperties", map.getKey(), repositoryId);
							throw new IllegalArgumentException("Property '" + map.getKey() + "' is unknown!");
						}
					} else {
						customProps.put(map.getKey(),
								typeId.getPropertyDefinitions().get(map.getKey()).getPropertyType());
					}
				}
			});

//			IObjectEncryptService encryptService = EncryptionFactory.createEncryptionService(repositoryId);
			if (customProps.size() > 0) {
				Set<Map.Entry<String, Object>> customData = customProps.entrySet();
				for (Map.Entry<String, Object> customValues : customData) {
					String id = customValues.getKey();
					if (!(customValues.getKey().equals(PropertyIds.SECONDARY_OBJECT_TYPE_IDS))) {
						Object valueOfType = data.getProperties().get(id);
						PropertyType propertyType = (PropertyType) customValues.getValue();
//						valueOfType = invokeDecryptAfterCreate(encryptService, repositoryId, EncryptType.DECRYPT,
//								typeId.getId(), id, valueOfType, propertyType, data.getSecondaryTypeIds(),
//								data.getProperties());
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

		/**
		 * create a folder for particular folderId
		 */
		public static String createFolder(String repositoryId, String folderId, Properties properties,
				List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject, String tracingId,
				ISpan parentSpan, Map<String, String> headers)
				throws CmisObjectNotFoundException, IllegalArgumentException, CmisInvalidArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createFolder", null);

			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.CREATE), false);
			if (permission) {
				IBaseObject folder = createFolderIntern(repositoryId, folderId, properties, typeId, policies, addAces,
						removeAces, userObject, tracingId, span, headers);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return folder.getId();
			} else {
				LOG.error("Create type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * returns an folderOject for particular folderId
		 */
		@SuppressWarnings("unchecked")
		private static IBaseObject createFolderIntern(String repositoryId, String folderId, Properties properties,
				String typeId, List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject,
				String tracingId, ISpan parentSpan, Map<String, String> headers)
				throws CmisObjectNotFoundException, IllegalArgumentException, CmisInvalidArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createFolderIntern", null);
			if (addAces != null && removeAces != null) {
				LOG.debug("Adding aces: {}, removing aces: {}", addAces.getAces(), removeAces.getAces());
			}
			invokeObjectFlowServiceBeforeCreate(repositoryId, folderId, properties, policies, addAces, removeAces,
					userObject, null, ObjectFlowType.CREATED);
			AccessControlListImplExt aclAdd = TypeValidators.impl
					.expandAclMakros(userObject == null ? null : userObject.getUserDN(), addAces);
			Acl aclRemove = TypeValidators.impl.expandAclMakros(userObject == null ? null : userObject.getUserDN(),
					removeAces);
			IBaseObject parent = null;
			List<String> secondaryObjectTypeIds = null;

			// get required properties
			PropertyData<?> pd = properties.getProperties().get(PropertyIds.NAME);
			String folderName = (String) pd.getFirstValue();
			if (folderName == null || folderName.length() == 0) {
				LOG.error("createFolderIntern unknown folder name: {}, repositoryId: {}, TraceId: {}", folderName,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_FOLDER), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						(TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_FOLDER), span)));
			}

			// check name syntax
			if (!NameValidator.impl.isValidName(folderName)) {
				LOG.error("createFolderIntern error: {}, : {}, repositoryId: {}, TraceId: {}",
						NameValidator.ERROR_ILLEGAL_NAME, folderName, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter
								.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span));
			}

			PropertyData<?> secondaryObjectType = properties.getProperties().get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (secondaryObjectType != null) {
				secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
			}

			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
					tracingId, span);

			if (typeDef == null) {
				LOG.error("Method name: {}, unknown typeId: {}, repositoryId: {}, TraceId: {}", "createFolder", typeDef,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span));
			}

			if (!typeDef.getBaseTypeId().equals(BaseTypeId.CMIS_FOLDER)) {
				LOG.error(
						"createFolderIntern cannot create a folder with a non-folder type: {}, repositoryId: {}, TraceId: {}",
						typeDef.getId(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService()
						.updateSpan(span,
								TracingErrorMessage.message(TracingWriter.log(String.format(
										ErrorMessages.CANNOT_CREATE_FOLDER_WITH_NON_FOLDER, typeDef.getId()), span),
										ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(String.format(TracingWriter.log(
						String.format(ErrorMessages.CANNOT_CREATE_FOLDER_WITH_NON_FOLDER, typeDef.getId()), span)));
			}
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			PropertiesImpl props = compileWriteProperties(repositoryId, typeDef, userObject, properties, null,
					tracingId, span);
			if (folderId != null) {
				parent = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, folderId, null,
						typeId);
			} else {
				parent = DBUtils.BaseDAO.getByName(repositoryId, "@ROOT@", false, null, typeId);
			}

			if (parent == null) {
				LOG.error("createFolderIntern parent is unknown: {}, repositoryId: {}, TraceId: {}", parent,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span));
			}

			LOG.debug("parentFolderData id: {}, path: {}, getInternalPath: {}", parent.getId(), parent.getPath(),
					parent.getInternalPath());

			PropertyData<?> objectIdProperty = properties.getProperties().get(PropertyIds.OBJECT_ID);
			String objectId = objectIdProperty == null ? null : (String) objectIdProperty.getFirstValue();
			PropertyData<?> virtual = properties.getProperties().get("cmis_ext:isVirtual");
			boolean isVirtual = virtual != null ? Boolean.parseBoolean(virtual.getFirstValue().toString()) : false;
			LOG.info("className: {}, methodName: {}, repositoryId: {}, isVirtual: {}", "cmisObjectService",
					"createFolderIntern", repositoryId, isVirtual);

			IBaseObject result = createFolderObject(repositoryId, parent, objectId, folderName, userObject,
					secondaryObjectTypeIds, typeId, props.getProperties(), objectMorphiaDAO, policies, aclAdd,
					aclRemove, tracingId, span);

			if (!isVirtual) {
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);
				try {
					if (localService != null) {
						LOG.debug("localService calling createFolder: {}, {}",
								localService.getClass() != null ? localService.getClass().getName() : null,
								result.getId());
					}
					if (localService != null) {
						localService.createFolder(result.getId().toString(), result.getName(), result.getPath());
						LOG.info("Folder: {} created ", result != null ? result.getName() : null);
					}
				} catch (IOException e) {
					objectMorphiaDAO.delete(repositoryId, principalIds, folderId, true, aclPropagation, null, typeId);
					LOG.error(
							"className: {}, methodName: {}, repositoryId: {}, createFolderIntern folder creation exception: {}, TraceId: {}",
							"cmisObjectService", "createFolderIntern", repositoryId, e,
							span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span),
									ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new IllegalArgumentException(
							TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span));
				}
			}
			invokeObjectFlowServiceAfterCreate(result, ObjectFlowType.CREATED, null, userObject, headers, repositoryId);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		public static void createTypeFolder(String repositoryId, Properties properties, IUserObject userObject,
				BaseTypeId baseType, String tracingId, ISpan parentSpan, Map<String, String> headers) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createTypeFolder", null);
			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			LOG.debug("createTypeFolder for custom type: {}", typeId);
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
					tracingId, span);
			PropertiesImpl props = compileWriteProperties(repositoryId, typeDef, userObject, properties, null,
					tracingId, span);
			IBaseObject parent = DBUtils.BaseDAO.getByName(repositoryId, "@ROOT@", false, null, typeId);
			PropertyData<?> pd = properties.getProperties().get(PropertyIds.NAME);
			String folderName = (String) pd.getFirstValue();
			AccessControlListImplExt aclImp = (AccessControlListImplExt) CmisUtils.Object
					.getAclFor(userObject == null ? null : userObject.getUserDN(), "cmis:all");
			aclImp.setAclPropagation(AclPropagation.REPOSITORYDETERMINED.toString());
			PropertyData<?> objectIdProperty = properties.getProperties().get(PropertyIds.OBJECT_ID);
			String objectId = objectIdProperty == null ? null : (String) objectIdProperty.getFirstValue();
			if (baseType == BaseTypeId.CMIS_DOCUMENT) {
				String folderObjectId = CmisObjectService.Impl.createFolder(repositoryId, parent.getId(), properties,
						null, aclImp, null, userObject, tracingId, span, headers);
				CmisAclServices.Impl.applyAcl(repositoryId, folderObjectId, null, null,
						AclPropagation.REPOSITORYDETERMINED, null, null, null, userObject, typeId, tracingId,
						parentSpan);
			} else {
				createFolderObject(repositoryId, parent, objectId, folderName, userObject, null, typeId,
						props.getProperties(), objectMorphiaDAO, null, aclImp, null, tracingId, span);
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		}

		/**
		 * inserting FolderObject into MongoDB
		 */
		private static IBaseObject createFolderObject(String repositoryId, IBaseObject parentData, String objectId,
				String folderName, IUserObject userObject, List<String> secondaryObjectTypeId, String typeId,
				Map<String, PropertyData<?>> properties, MBaseObjectDAO objectMorphiaDAO, List<String> policies,
				AccessControlListImplExt addAces, Acl removeAces, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createFolderObject", null);
			Tuple2<String, String> p = resolvePathForObject(parentData, folderName);

			Map<String, Object> custom = readCustomPropetiesData(properties, secondaryObjectTypeId, repositoryId,
					typeId, userObject);
			IBaseObject folderObject = DBUtils.BaseDAO.getByName(repositoryId, folderName, false,
					parentData.getId().toString(), typeId);
			if (folderObject != null) {
				LOG.error("createFolderObject already present FolderObject:{}, repositoryId: {}, TraceId: {}",
						folderObject.getId(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.FOLDER_PRESENT, folderName), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.FOLDER_PRESENT, folderName), span));
			}

			MBaseObjectDAO objectDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			TokenImpl token = new TokenImpl(TokenChangeType.CREATED, System.currentTimeMillis());
			IBaseObject result = objectDAO.createObjectFacade(folderName, BaseTypeId.CMIS_FOLDER, typeId, repositoryId,
					secondaryObjectTypeId,
					properties.get(PropertyIds.DESCRIPTION) == null ? ""
							: properties.get(PropertyIds.DESCRIPTION).getFirstValue().toString(),
					userObject == null ? null : userObject.getUserDN(),
					userObject == null ? null : userObject.getUserDN(), token, p._1(), custom, policies, addAces,
					p._2(), parentData.getId().toString());

			if (result instanceof ISettableBaseObject) {
				ISettableBaseObject settableBaseObject = (ISettableBaseObject) result;
				if (objectId != null && !objectId.isEmpty()) {
					settableBaseObject.setId(objectId);
				}
			}

			objectMorphiaDAO.commit(result, typeId);
			if (result != null) {
				LOG.debug("createFolderObject successful using objectMorphiaDAO: {}, {}", result.getId(),
						result.getName());
			}
			if (removeAces != null)
				validateAcl(repositoryId, removeAces, result.getId(), result);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		private static Map<String, Object> readCustomPropetiesData(Map<String, PropertyData<?>> properties,
				List<String> secondaryObjectTypeIds, String repositoryId, String typeId, IUserObject userObject) {
			Map<String, Object> custom = new HashMap<String, Object>();
			TypeDefinition type = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject, null,
					null);
			Set<Map.Entry<String, PropertyData<?>>> customData = properties.entrySet();
			for (Map.Entry<String, PropertyData<?>> customValues : customData) {
				PropertyData<?> valueName = customValues.getValue();
				if (valueName.getValues().size() == 0) {
					continue;
				}
				Boolean isOpenChoice = null;
				if (type.getPropertyDefinitions().get(valueName.getId()) == null) {
					// check for secondaryTypeIds's
					if (secondaryObjectTypeIds != null) {
						isOpenChoice = secondaryObjectTypeIds.stream()
								.map(id -> CmisTypeServices.Impl.getTypeDefinition(repositoryId, id, null, userObject,
										null, null))
								.filter(secType -> secType.getPropertyDefinitions().get(valueName.getId()) != null
										&& secType.getPropertyDefinitions().get(valueName.getId())
												.isOpenChoice() != null)
								.map(secType -> secType.getPropertyDefinitions().get(valueName.getId()).isOpenChoice())
								.findFirst().orElse(null);
					}
				} else {
					isOpenChoice = type.getPropertyDefinitions().get(valueName.getId()).isOpenChoice();
				}

				if (valueName.getFirstValue().getClass().getSimpleName().equalsIgnoreCase("GregorianCalendar")) {
					if (valueName.getValues().size() == 1) {
						GregorianCalendar value = convertInstanceOfObject(valueName.getFirstValue(),
								GregorianCalendar.class);
						Long time = value.getTimeInMillis();
						custom.put(valueName.getId(), time.longValue());
					} else {
						List<Long> valueList = new ArrayList<>();
						valueName.getValues().forEach(v -> {
							GregorianCalendar value = convertInstanceOfObject(v, GregorianCalendar.class);
							Long time = value.getTimeInMillis();
							valueList.add(time.longValue());
						});
						custom.put(valueName.getId(), valueList);
					}

				} else if (valueName.getFirstValue().getClass().getSimpleName().equalsIgnoreCase("BigInteger")) {
					if (isOpenChoice != null && isOpenChoice == false) {
						List<?> choiceValues = type.getPropertyDefinitions().get(valueName.getId()).getChoices() != null
								? type.getPropertyDefinitions().get(valueName.getId()).getChoices().stream()
										.flatMap(a -> a.getValue().stream()).filter(b -> b != null)
										.map(c -> new BigInteger(c.toString())).collect(Collectors.toList())
								: null;

						valueName.getValues().stream()
								.filter(value -> choiceValues != null && choiceValues.size() > 0
										&& !choiceValues.contains(convertInstanceOfObject(value, BigInteger.class)))
								.findFirst().ifPresent(a -> {
									throw new IllegalArgumentException(
											"wrong choice for propertyType:" + valueName.getId());
								});
					}
					BigInteger maxInt = BigInteger.valueOf(Integer.MAX_VALUE);
					if (valueName.getValues().size() == 1) {
						BigInteger valueBigInteger = convertInstanceOfObject(valueName.getFirstValue(),
								BigInteger.class);
						if (valueBigInteger.compareTo(maxInt) > 0) {
							custom.put(valueName.getId(), valueBigInteger.longValue());
						} else {
							custom.put(valueName.getId(), valueBigInteger.intValue());
						}
					} else {
						List<Long> valueList = new ArrayList<>();
						List<Integer> intValueList = new ArrayList<>();
						valueName.getValues().forEach(v -> {
							BigInteger valueBigInteger = convertInstanceOfObject(v, BigInteger.class);
							if (valueBigInteger.compareTo(maxInt) > 0) {
								valueList.add(valueBigInteger.longValue());
							} else {
								intValueList.add(valueBigInteger.intValue());
							}
						});
						if (!intValueList.isEmpty()) {
							custom.put(valueName.getId(), intValueList);
						} else {
							custom.put(valueName.getId(), valueList);
						}
					}

				} else if (valueName.getFirstValue().getClass().getSimpleName().equalsIgnoreCase("BigDecimal")) {
					if (valueName.getValues().size() == 1) {
						BigDecimal value = convertInstanceOfObject(valueName.getFirstValue(), BigDecimal.class);
						double doubleValue = value.doubleValue();
						custom.put(valueName.getId(), doubleValue);
					} else {
						List<Double> valueList = new ArrayList<>();
						valueName.getValues().forEach(v -> {
							BigDecimal value = convertInstanceOfObject(v, BigDecimal.class);
							valueList.add(value.doubleValue());
						});
						custom.put(valueName.getId(), valueList);
					}

				} else if (valueName.getFirstValue().getClass().getSimpleName().equalsIgnoreCase("Boolean")) {
					if (isOpenChoice != null && isOpenChoice == false) {
						List<?> choiceValues = type.getPropertyDefinitions().get(valueName.getId()).getChoices() != null
								? type.getPropertyDefinitions().get(valueName.getId()).getChoices().stream()
										.flatMap(a -> a.getValue().stream()).collect(Collectors.toList())
								: null;
						valueName.getValues().stream()
								.filter(value -> choiceValues != null && choiceValues.size() > 0
										&& !choiceValues.contains(convertInstanceOfObject(value, Boolean.class)))
								.findFirst().ifPresent(a -> {
									throw new IllegalArgumentException(
											"wrong choice for propertyType:" + valueName.getId());
								});
					}

					if (valueName.getValues().size() == 1) {
						Boolean valueBoolean = convertInstanceOfObject(valueName.getFirstValue(), Boolean.class);
						custom.put(valueName.getId(), valueBoolean);
					} else {
						valueName.getValues().forEach(v -> {
							Boolean valueBoolean = convertInstanceOfObject(v, Boolean.class);
							ArrayList<Boolean> valueList = new ArrayList<>();
							valueList.add(valueBoolean.booleanValue());
						});
						custom.put(valueName.getId(), valueName.getValues());
					}

				} else if (type.getPropertyDefinitions().get(valueName.getId()) != null && type.getPropertyDefinitions()
						.get(valueName.getId()).getPropertyType().equals(PropertyType.HTML)) {
					if (valueName.getValues().size() == 1) {
						String value = convertInstanceOfObject(valueName.getFirstValue(), String.class);
						String encodedValue = htmlEncode(value);
						custom.put(valueName.getId(), encodedValue);
					} else {
						List<String> valueList = new ArrayList<>();
						valueName.getValues().forEach(v -> {
							String value = convertInstanceOfObject(valueName.getFirstValue(), String.class);
							valueList.add(htmlEncode(value));
						});
						custom.put(valueName.getId(), valueList);
					}
				} else {
					if (isOpenChoice != null && isOpenChoice == false) {
						List<?> choiceValues = type.getPropertyDefinitions().get(valueName.getId()).getChoices() != null
								? type.getPropertyDefinitions().get(valueName.getId()).getChoices().stream()
										.flatMap(a -> a.getValue().stream()).collect(Collectors.toList())
								: null;
						valueName.getValues().stream()
								.filter(value -> choiceValues != null && choiceValues.size() > 0
										&& !choiceValues.contains(convertInstanceOfObject(value, String.class)))
								.findFirst().ifPresent(a -> {
									throw new IllegalArgumentException(
											"wrong choice for propertyType:" + valueName.getId());
								});
					}
					if (valueName.getValues().size() == 1)
						custom.put(valueName.getId(), valueName.getFirstValue());
					else {
						custom.put(valueName.getId(), valueName.getValues());
					}
				}
			}
			return custom;
		}

		/**
		 * create document based on the given folderId
		 */
		public static String createDocument(String repositoryId, Properties properties, String folderId,
				ContentStream contentStream, VersioningState versioningState, List<String> policies, Acl addAces,
				Acl removeAces, IUserObject userObject, String tracingId, ISpan parentSpan, Map<String, String> headers)
				throws CmisInvalidArgumentException, CmisConstraintException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createDocument", null);
			// Attach the CallContext to a thread local context that can be
			// accessed from everywhere
			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.CREATE), false);
			if (permission) {
				IDocumentObject so = createDocumentIntern(repositoryId, properties, typeId, folderId, contentStream,
						versioningState, policies, addAces, removeAces, userObject, tracingId, span, headers);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return so.getId();
			} else {
				LOG.error("Create type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * returns an documentObject for particular document based on the folderID
		 */
		@SuppressWarnings("unchecked")
		private static IDocumentObject createDocumentIntern(String repositoryId, Properties properties,
				String objectTypeId, String folderId, ContentStream contentStream, VersioningState versioningState,
				List<String> policies, Acl addACEs, Acl removeACEs, IUserObject userObject, String tracingId,
				ISpan parentSpan, Map<String, String> headers)
				throws CmisInvalidArgumentException, CmisConstraintException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createDocumentIntern", null);
			if (addACEs != null && removeACEs != null) {
				LOG.debug("Adding aces: {}, removing aces: {}", addACEs.getAces(), removeACEs.getAces());
			}
			invokeObjectFlowServiceBeforeCreate(repositoryId, folderId, properties, policies, addACEs, removeACEs,
					userObject, null, ObjectFlowType.CREATED);
			AccessControlListImplExt aclAdd = TypeValidators.impl
					.expandAclMakros(userObject == null ? null : userObject.getUserDN(), addACEs);
			Acl aclRemove = TypeValidators.impl.expandAclMakros(userObject == null ? null : userObject.getUserDN(),
					removeACEs);
			List<String> secondaryObjectTypeIds = null;
			if (properties == null || properties.getProperties() == null) {
				LOG.error("createDocumentIntern unknown properties: {}, repositoryId: {}, TraceId: {}", properties,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.PROPERTIES_MUST_BE_SET), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.PROPERTIES_MUST_BE_SET), span));
			}

			// check versioning state
			// if (VersioningState.NONE == versioningState || versioningState ==
			// null) {
			// LOG.error("Versioning not supported!");
			// throw new CmisConstraintException("Versioning not supported!");
			// }

			PropertyData<?> pd = properties.getProperties().get(PropertyIds.NAME);
			String documentName = (String) pd.getFirstValue();
			if (documentName == null || documentName.length() == 0) {
				LOG.error("createDocumentIntern unknown document name, repositoryId: {}, TraceId: {}", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_DOCUMENT), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_DOCUMENT), span));
			}

			PropertyData<?> secondaryObjectType = properties.getProperties().get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (secondaryObjectType != null) {
				secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
			}

			// check name syntax
			if (!NameValidator.impl.isValidName(documentName)) {
				LOG.error("createDocumentIntern error: {}, : {}, repositoryId: {}, TraceId: {}",
						NameValidator.ERROR_ILLEGAL_NAME, documentName, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter
								.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span));
			}

			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			MDocumentObjectDAO documentMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);

			TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
					tracingId, span);
			if (typeDef == null) {
				LOG.error("Method name : {}, unknown typeId: {}, repositoryId: {}, TraceId: {}", "createDocument",
						typeDef, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span));
			}
			if (typeDef.getBaseTypeId() != BaseTypeId.CMIS_DOCUMENT) {
				LOG.error("createDocumentIntern Type: {} is not a document type!, repositoryId: {}, TraceId: {}",
						typeDef.getBaseTypeId(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.TYPE_MUST_BE_DOCUMENT_TYPE), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.TYPE_MUST_BE_DOCUMENT_TYPE), span));
			}
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			// compile the properties
			PropertiesImpl props = compileWriteProperties(repositoryId, typeDef, userObject, properties, null,
					tracingId, span);
			// String name = getStringProperty(properties, PropertyIds.NAME);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			IBaseObject parent = null;
			if (folderId != null) {
				parent = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, folderId, null,
						typeId);
			} else {
				objectTypeId = objectTypeId.equals(BaseTypeId.CMIS_DOCUMENT.value()) ? objectTypeId = "@ROOT@"
						: objectTypeId;
				parent = DBUtils.BaseDAO.getByPath(repositoryId, principalIds, aclPropagation, "/" + objectTypeId,
						typeId);
				if (parent == null) {
					parent = DBUtils.BaseDAO.getByName(repositoryId, "@ROOT@", false, null, typeId);
				}
			}
			if (parent == null) {
				LOG.error("createDocumentIntern unknown parent: {}, repositoryId: {}, TraceId: {}", parent,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span));
			}

			LOG.debug("parentFolderData id: {}, path: {}, getInternalPath: {}", parent.getId(), parent.getPath(),
					parent.getInternalPath());

			PropertyData<?> objectIdProperty = properties.getProperties().get(PropertyIds.OBJECT_ID);
			String objectId = objectIdProperty == null ? null : (String) objectIdProperty.getFirstValue();
			IDocumentObject result = createDocumentObject(repositoryId, parent, objectId, documentName, userObject,
					secondaryObjectTypeIds, contentStream, typeId, documentMorphiaDAO, props.getProperties(), policies,
					aclAdd, aclRemove, versioningState, tracingId, span);

			Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
			LOG.info("FileDetails for repositoryId: {}, is {} ", repositoryId, parameters);
			IStorageService localService = StorageServiceFactory.createStorageService(parameters);
			if (contentStream != null) {
				try {
					if (localService != null) {
						localService.writeContent(result.getId().toString(), result.getContentStreamFileName(),
								result.getPath(), contentStream);
						if (result != null) {
							LOG.debug("Document: {} created successfully", result.getName());
						}
					}
				} catch (Exception ex) {
					documentMorphiaDAO.delete(result.getId(), null, true, false, null);
					LOG.error("createDocumentIntern file creation exception: {}, repositoryId: {}, TraceId: {}", ex,
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.EXCEPTION, ex), span),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new IllegalArgumentException(
							TracingWriter.log(String.format(ErrorMessages.EXCEPTION, ex), span));
				}
			}

			invokeObjectFlowServiceAfterCreate(result, ObjectFlowType.CREATED, null, userObject, headers, repositoryId);

			GregorianCalendar creationDateCalender = new GregorianCalendar();
			creationDateCalender.setTimeInMillis(result.getCreatedAt());
			// set creation date
			addPropertyDateTime(repositoryId, props, typeDef, null, PropertyIds.CREATION_DATE, creationDateCalender,
					userObject);
			// write properties
			// writePropertiesFile(newFile, props);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		/**
		 * inserting documentOject into mongoDB
		 */
		public static IDocumentObject createDocumentObject(String repositoryId, IBaseObject parentData, String objectId,
				String docName, IUserObject userObject, List<String> secondaryObjectTypeIds,
				ContentStream contentStream, String typeId, MDocumentObjectDAO documentMorphiaDAO,
				Map<String, PropertyData<?>> properties, List<String> policies, AccessControlListImplExt addACEs,
				Acl removeACEs, VersioningState versioningState, String tracingId, ISpan parentSpan)
				throws IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createDocumentObject", null);
			IDocumentObject documentObject = null;
			MBaseObjectDAO objectDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			String versionSeriesId = Helpers.getObjectId();
			String versionReferenceId = Helpers.getObjectId();
			Map<String, Object> custom = readCustomPropetiesData(properties, secondaryObjectTypeIds, repositoryId,
					typeId, userObject);
			IBaseObject baseObject = null;
			Tuple2<String, String> p = null;
			IDocumentObject document = DBUtils.DocumentDAO.getDocumentByName(repositoryId, docName,
					parentData.getId().toString());
			if (document == null) {
				TokenImpl token = new TokenImpl(TokenChangeType.CREATED, System.currentTimeMillis());
				p = resolvePathForObject(parentData, docName);
				baseObject = objectDAO.createObjectFacade(docName, BaseTypeId.CMIS_DOCUMENT, typeId, repositoryId,
						secondaryObjectTypeIds,
						properties.get(PropertyIds.DESCRIPTION) == null ? ""
								: properties.get(PropertyIds.DESCRIPTION).getFirstValue().toString(),
						userObject == null ? null : userObject.getUserDN(),
						userObject == null ? null : userObject.getUserDN(), token, p._1(), custom, policies, addACEs,
						p._2(), parentData.getId().toString());
				if (contentStream != null) {
					// p = resolvePathForObject(parentData,
					// contentStream.getFileName());
					// getting path name again
					baseObject = objectDAO.createObjectFacade(docName, BaseTypeId.CMIS_DOCUMENT, typeId, repositoryId,
							secondaryObjectTypeIds,
							properties.get(PropertyIds.DESCRIPTION) == null ? ""
									: properties.get(PropertyIds.DESCRIPTION).getFirstValue().toString(),
							userObject == null ? null : userObject.getUserDN(),
							userObject == null ? null : userObject.getUserDN(), token, p._1(), custom, policies,
							addACEs, p._2(), parentData.getId().toString());
				}

				if (baseObject instanceof ISettableBaseObject) {
					ISettableBaseObject settableBaseObject = (ISettableBaseObject) baseObject;
					if (objectId != null && !objectId.isEmpty()) {
						settableBaseObject.setId(objectId);
					}
				}

				if (versioningState == VersioningState.MAJOR) {
					documentObject = Helpers.getDocumentObject(documentMorphiaDAO, baseObject, true, true, true,
							contentStream, versionSeriesId, versionReferenceId);
				} else if (versioningState == VersioningState.MINOR) {
					documentObject = Helpers.getDocumentObject(documentMorphiaDAO, baseObject, true, false, false,
							contentStream, versionSeriesId, versionReferenceId);
				} else if (versioningState == VersioningState.NONE || versioningState == null) {
					documentObject = Helpers.getDocumentObject(documentMorphiaDAO, baseObject, true, false, false,
							contentStream, versionSeriesId, versionReferenceId);
				} else if (versioningState == VersioningState.CHECKEDOUT) {
					documentObject = Helpers.getDocumentObject(documentMorphiaDAO, baseObject, true, true, true,
							contentStream, versionSeriesId, versionReferenceId);
				}

				documentMorphiaDAO.commit(documentObject);

				if (documentObject != null) {
					LOG.debug("Successfully created documentObject: {}, with version: {}, and with contentStream: {}",
							documentObject, versioningState, contentStream);
				}

				if (removeACEs != null)
					validateAcl(repositoryId, removeACEs, baseObject.getId(), baseObject);
				if (versioningState == VersioningState.CHECKEDOUT) {
					Holder<String> objectsId = new Holder<String>(baseObject.getId().toString());
					String pwcId = CmisVersioningServices.Impl.checkOut(repositoryId, objectsId, null, null, userObject,
							tracingId, span);
					documentObject = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, pwcId, null);
				}
				return documentObject;
			} else {
				if (contentStream != null) {
					Map<String, Object> updateProps = new HashMap<String, Object>();
					updateProps.put("contentStreamLength", contentStream.getLength());
					updateProps.put("contentStreamMimeType", contentStream.getMimeType());
					documentMorphiaDAO.update(document.getId(), updateProps);
				}
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return document;
			}
		}

		private static Tuple2<String, String> resolvePathForObject(IBaseObject parentData, String docName) {
			String path = null;
			String cmisPath = null;
			if (parentData.getInternalPath() == null) {
				path = "," + parentData.getId() + ",";
				cmisPath = "/" + docName;
			} else {
				path = parentData.getInternalPath() + parentData.getId() + ",";
				if (parentData.getPath().equals("/")) {
					cmisPath = "/" + docName;
				} else {
					cmisPath = parentData.getPath() + "/" + docName;
				}
			}
			if (parentData != null && cmisPath != null) {
				LOG.debug("Parent id :{}, document Location:{}", parentData.getId(), cmisPath);
			}

			return new Tuple2<>(path, cmisPath);
		}

		/**
		 * creating a document based on the another DocumentObject
		 */
		@SuppressWarnings("unchecked")
		public static String createDocumentFromSource(String repositoryId, String sourceId, Properties properties,
				String folderId, VersioningState versioningState, List<String> policies, Acl addAces, Acl removeAces,
				IUserObject userObject, String tracingId, ISpan parentSpan, Map<String, String> headers)
				throws CmisInvalidArgumentException, CmisConstraintException, CmisObjectNotFoundException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createDocumentFromSource", null);
			IDocumentObject doc = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, sourceId, null);

			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					doc.getTypeId(), EnumSet.of(PermissionType.CREATE), false);
			if (permission) {
				if (addAces != null && removeAces != null) {
					LOG.debug("Adding aces: {}, removing aces: {}", addAces.getAces(), removeAces.getAces());
				}
				invokeObjectFlowServiceBeforeCreate(repositoryId, folderId, properties, policies, addAces, removeAces,
						userObject, null, ObjectFlowType.CREATED);
				AccessControlListImplExt aclAdd = TypeValidators.impl
						.expandAclMakros(userObject == null ? null : userObject.getUserDN(), addAces);
				Acl aclRemove = TypeValidators.impl.expandAclMakros(userObject == null ? null : userObject.getUserDN(),
						removeAces);
				List<String> secondaryObjectTypeIds = null;

				MDocumentObjectDAO documentMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MDocumentObjectDAO.class);
				if (properties == null || properties.getProperties() == null) {
					Map<String, Object> propertiesdata = doc.getProperties();
					Map<String, List<String>> pros = new HashMap<String, List<String>>();
					if (propertiesdata != null) {
						for (Map.Entry<String, Object> entry : propertiesdata.entrySet()) {
							List<String> list = new ArrayList<String>();
							list.add(entry.getValue().toString());
							pros.put(entry.getKey(), list);
						}
					}
					getSourceProperties(pros, PropertyIds.NAME, doc.getName());
					getSourceProperties(pros, PropertyIds.OBJECT_TYPE_ID, doc.getTypeId());
					properties = CmisPropertyConverter.Impl.createNewProperties(pros, repositoryId, userObject);
				}
				String objectTypeId = getObjectTypeId(properties, repositoryId, tracingId, span);
				PropertyData<?> pd = properties.getProperties().get(PropertyIds.NAME);
				String documentName = (String) pd.getFirstValue();
				if (null == documentName || documentName.length() == 0) {
					LOG.error("createDocumentFromSource unknown document name: {}, repositoryId: {}, TraceId: {}",
							documentName, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_DOCUMENT), span),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisInvalidArgumentException(String
							.format(TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_DOCUMENT), span)));
				}

				// check name syntax
				if (!NameValidator.impl.isValidName(documentName)) {
					LOG.error("createDocumentFromSource error: {}, : {} , TraceId: {}",
							NameValidator.ERROR_ILLEGAL_NAME, documentName, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(
											String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisInvalidArgumentException(TracingWriter
							.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span));
				}
				String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);

				TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
						tracingId, span);
				if (typeDef == null) {
					LOG.error("Method name: {}, unknown typeId: {}, repositoryId: {}, TraceId: {}",
							"createDocumentFromSource", typeDef, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span));
				}
				if (typeDef.getBaseTypeId() != BaseTypeId.CMIS_DOCUMENT) {
					LOG.error(
							"createDocumentFromSource Type: {} must be a document type!, repositoryId: {}, TraceId: {}",
							typeDef.getBaseTypeId(), repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.TYPE_MUST_BE_DOCUMENT_TYPE), span),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisInvalidArgumentException(
							TracingWriter.log(String.format(ErrorMessages.TYPE_MUST_BE_DOCUMENT_TYPE), span));
				}

				PropertyData<?> secondaryObjectType = properties.getProperties()
						.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
				if (secondaryObjectType != null) {
					secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
				}
				// compile the properties
				PropertiesImpl props = compileWriteProperties(repositoryId, typeDef, userObject, properties, null,
						tracingId, span);
				String[] principalIds = Helpers.getPrincipalIds(userObject);
				// String name = getStringProperty(properties,
				// PropertyIds.NAME);
				String systemAdmin = System.getenv("SYSTEM_ADMIN");
				boolean aclPropagation = Stream.of(userObject.getGroups())
						.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
				IBaseObject parent = null;
				if (folderId != null) {
					parent = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, folderId, null,
							typeId);
				} else {
					objectTypeId = objectTypeId.equals(BaseTypeId.CMIS_DOCUMENT.value()) ? objectTypeId = "@ROOT@"
							: objectTypeId;
					parent = DBUtils.BaseDAO.getByName(repositoryId, objectTypeId, false, null, typeId);
					if (parent == null) {
						parent = DBUtils.BaseDAO.getByName(repositoryId, "@ROOT@", false, null, typeId);
					}
				}

				if (parent == null) {
					LOG.error("createDocumentFromSource unknown parent: {}, repositoryId: {}, TraceId: {}", parent,
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisInvalidArgumentException(
							TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span));
				}

				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				IDocumentObject sourceResult = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, sourceId, null);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);
				ContentStream contentStream = null;
				if (sourceResult.getContentStreamFileName() != null) {
					contentStream = localService.getContent(sourceResult.getContentStreamFileName(),
							sourceResult.getPath(), sourceResult.getContentStreamMimeType(),
							BigInteger.valueOf(sourceResult.getContentStreamLength()),
							sourceResult.getContentStreamFileName());
				}

				PropertyData<?> objectIdProperty = properties.getProperties().get(PropertyIds.OBJECT_ID);
				String objectId = objectIdProperty == null ? null : (String) objectIdProperty.getFirstValue();
				IDocumentObject result = createDocumentObject(repositoryId, parent, objectId, documentName, userObject,
						secondaryObjectTypeIds, contentStream, typeId, documentMorphiaDAO, props.getProperties(),
						policies, aclAdd, aclRemove, versioningState, tracingId, span);
				if (contentStream != null) {
					try {
						localService.writeContent(result.getId().toString(), sourceResult.getContentStreamFileName(),
								result.getPath(), contentStream);
						if (result != null) {
							LOG.debug("Document: {} created successfully", result.getName());
						}

					} catch (Exception ex) {
						documentMorphiaDAO.delete(result.getId(), null, true, false, null);
						LOG.error("createDocumentFromSource file creation exception: {}, repositoryId: {}, TraceId: {}",
								ex, repositoryId, span != null ? span.getTraceId() : null);
						TracingApiServiceFactory.getApiService().updateSpan(span,
								TracingErrorMessage.message(
										TracingWriter.log(String.format(ErrorMessages.EXCEPTION, ex), span),
										ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
						TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
						throw new IllegalArgumentException(
								TracingWriter.log(String.format(ErrorMessages.EXCEPTION, ex), span));
					}
				}
				invokeObjectFlowServiceAfterCreate(result, ObjectFlowType.CREATED, null, userObject, headers, repositoryId);
				GregorianCalendar creationDateCalender = new GregorianCalendar();
				creationDateCalender.setTimeInMillis(result.getCreatedAt());
				// set creation date
				addPropertyDateTime(repositoryId, props, typeDef, null, PropertyIds.CREATION_DATE, creationDateCalender,
						userObject);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return result.getId();
			} else {
				LOG.error("Create type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * create a item based on folderID
		 * @param headers 
		 */
		public static String createItem(String repositoryId, Properties properties, String folderId,
				List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject, String tracingId,
				ISpan parentSpan, Map<String, String> headers) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createItem", null);
			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.CREATE), false);
			if (permission) {
				IBaseObject so = createItemIntern(repositoryId, properties, typeId, folderId, policies, addAces,
						removeAces, userObject, tracingId, span, headers);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return so.getId();
			} else {
				LOG.error("Create type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * returns item
		 * @param headers 
		 */
		@SuppressWarnings("unchecked")
		private static IBaseObject createItemIntern(String repositoryId, Properties properties, String objectTypeId,
				String folderId, List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject,
				String tracingId, ISpan parentSpan, Map<String, String> headers)
				throws CmisInvalidArgumentException, CmisObjectNotFoundException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createItemIntern", null);
			if (addAces != null && removeAces != null) {
				LOG.debug("Adding aces: {}, removing aces: {}", addAces.getAces(), removeAces.getAces());
			}
			invokeObjectFlowServiceBeforeCreate(repositoryId, folderId, properties, policies, addAces, removeAces,
					userObject, null, ObjectFlowType.CREATED);
			AccessControlListImplExt aclAdd = TypeValidators.impl
					.expandAclMakros(userObject == null ? null : userObject.getUserDN(), addAces);
			Acl aclRemove = TypeValidators.impl.expandAclMakros(userObject == null ? null : userObject.getUserDN(),
					removeAces);

			List<String> secondaryObjectTypeIds = null;
			PropertyData<?> pd = properties.getProperties().get(PropertyIds.NAME);
			String itemName = (String) pd.getFirstValue();
			if (null == itemName || itemName.length() == 0) {
				LOG.error("createItemIntern unknown item name: {}, repositoryId: {}, TraceId: {}", itemName,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_ITEM), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_ITEM), span));
			}

			// check name syntax
			if (!NameValidator.impl.isValidName(itemName)) {
				LOG.error("createItemIntern error: {}, : {}, repositoryId: {}, TraceId: {} ",
						NameValidator.ERROR_ILLEGAL_NAME, itemName, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter
								.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span));
			}

			PropertyData<?> secondaryObjectType = properties.getProperties().get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (secondaryObjectType != null) {
				secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
			}

			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
					tracingId, span);
			if (typeDef == null) {
				LOG.error("Method name: {}, unknown typeId: {}, repositoryId: {}, TraceId: {}", "createItem", typeDef,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId, span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId, span));
			}
			// check if the given type is a folder type
			if (!typeDef.getBaseTypeId().equals(BaseTypeId.CMIS_ITEM)) {
				LOG.error(
						"createItemIntern cannot create a folder with a non-folder type: {}, repositoryId: {}, TraceId: {}"
								+ typeDef.getId(),
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService()
						.updateSpan(span,
								TracingErrorMessage.message(TracingWriter.log(String.format(
										ErrorMessages.CANNOT_CREATE_FOLDER_WITH_NON_FOLDER, typeDef.getId()), span),
										ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter
						.log(String.format(ErrorMessages.CANNOT_CREATE_FOLDER_WITH_NON_FOLDER, typeDef.getId()), span));
			}

			PropertiesImpl props = compileWriteProperties(repositoryId, typeDef, userObject, properties, null,
					tracingId, span);
			IBaseObject parent = null;
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			if (folderId != null) {
				parent = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, folderId, null,
						typeId);
			} else {
				objectTypeId = objectTypeId.equals(BaseTypeId.CMIS_ITEM.value()) ? objectTypeId = "@ROOT@"
						: objectTypeId;
				parent = DBUtils.BaseDAO.getByPath(repositoryId, principalIds, aclPropagation, "/" + objectTypeId,
						typeId);
				if (parent == null) {
					parent = DBUtils.BaseDAO.getByName(repositoryId, "@ROOT@", false, null, typeId);
				}
			}
			if (parent != null) {
				LOG.debug("parentFolderData id: {}, path: {}, getInternalPath: {}", parent.getId(), parent.getPath(),
						parent.getInternalPath());
			}

			PropertyData<?> objectIdProperty = properties.getProperties().get(PropertyIds.OBJECT_ID);
			String objectId = objectIdProperty == null ? null : (String) objectIdProperty.getFirstValue();
			IBaseObject result = createItemObject(repositoryId, parent, objectId, itemName, userObject,
					secondaryObjectTypeIds, typeId, props.getProperties(), policies, aclAdd, aclRemove, tracingId,
					span);
			invokeObjectFlowServiceAfterCreate(result, ObjectFlowType.CREATED, null, userObject, headers, repositoryId);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		/**
		 * inserting itemObject into mongoDB
		 */
		private static IBaseObject createItemObject(String repositoryId, IBaseObject parentData, String objectId,
				String itemName, IUserObject userObject, List<String> secondaryObjectTypeIds, String typeId,
				Map<String, PropertyData<?>> properties, List<String> policies, AccessControlListImplExt aclAdd,
				Acl aclRemove, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException, IllegalArgumentException {
			// 0) folder id
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createItemObject", null);
			Tuple2<String, String> p = resolvePathForObject(parentData, itemName);

			Map<String, Object> custom = readCustomPropetiesData(properties, secondaryObjectTypeIds, repositoryId,
					typeId, userObject);
			MBaseObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			IBaseObject itemObject = DBUtils.BaseDAO.getByName(repositoryId, itemName, false,
					parentData.getId().toString(), typeId);
			if (itemObject != null) {
				LOG.error("createItemObject object id already present: {}, repositoryId: {}, TraceId: {}",
						itemObject.getId(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.ITEM_PRESENT, itemObject.getId()), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.ITEM_PRESENT, itemObject.getId()), span));
			}

			TokenImpl token = new TokenImpl(TokenChangeType.CREATED, System.currentTimeMillis());
			IBaseObject result = baseMorphiaDAO.createObjectFacade(itemName, BaseTypeId.CMIS_ITEM, typeId, repositoryId,
					secondaryObjectTypeIds,
					properties.get(PropertyIds.DESCRIPTION) == null ? ""
							: properties.get(PropertyIds.DESCRIPTION).getFirstValue().toString(),
					userObject == null ? null : userObject.getUserDN(),
					userObject == null ? null : userObject.getUserDN(), token, p._1(), custom, policies, aclAdd, p._2(),
					parentData.getId().toString());

			if (result instanceof ISettableBaseObject) {
				ISettableBaseObject settableBaseObject = (ISettableBaseObject) result;
				if (objectId != null && !objectId.isEmpty()) {
					settableBaseObject.setId(objectId);
				}
			}

			baseMorphiaDAO.commit(result, typeId);
			if (result != null) {
				LOG.debug("Item: {} created successfully", result.getName());
			}
			if (aclRemove != null)
				validateAcl(repositoryId, aclRemove, result.getId(), result);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		/**
		 * create a relationship based on two Documents
		 */
		public static String createRelationship(String repositoryId, String folderId, Properties properties,
				List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject, String tracingId,
				ISpan parentSpan)
				throws CmisInvalidArgumentException, CmisObjectNotFoundException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createRelationship", null);
			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.CREATE), false);
			if (permission) {
				IRelationObject so = createRelationshipIntern(repositoryId, folderId, properties, typeId, policies,
						addAces, removeAces, userObject, tracingId, span);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return so == null ? null : so.getId();
			} else {
				LOG.error("Create type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * returns relationship Document
		 */
		@SuppressWarnings({ "unchecked", "unused" })
		private static IRelationObject createRelationshipIntern(String repositoryId, String folderId,
				Properties properties, String typeId, List<String> policies, Acl addAces, Acl removeAces,
				IUserObject userObject, String tracingId, ISpan parentSpan)
				throws CmisInvalidArgumentException, CmisObjectNotFoundException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createRelationshipIntern", null);
			Map<String, Object> attrMap = new HashMap<String, Object>();
			MRelationObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MRelationObjectDAO.class);
			// invokeObjectFlowServiceBeforeCreate(repositoryId, folderId, properties,
			// policies, addAces, removeAces,
			// userObject, null, ObjectFlowType.CREATED);
			List<String> secondaryObjectTypeIds = null;
			TypeValidator.validateRequiredSystemProperties(properties);

			if (addAces != null && removeAces != null) {
				LOG.debug("Adding aces:{}, removing aces: {}", addAces.getAces(), removeAces.getAces());
			}
			AccessControlListImplExt aclAdd = TypeValidators.impl
					.expandAclMakros(userObject == null ? null : userObject.getUserDN(), addAces);
			Acl aclRemove = TypeValidators.impl.expandAclMakros(userObject == null ? null : userObject.getUserDN(),
					removeAces);

			PropertyData<?> secondaryObjectType = properties.getProperties().get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (secondaryObjectType != null) {
				secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
			}
			PropertyData<?> relationTypeProperty = properties.getProperties().get(RELATION_NAME);
			if (relationTypeProperty == null) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"cannot create a relationship without a relation_name.", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP), span));
			}
			String relationName = (String) relationTypeProperty.getFirstValue();
			if (relationName == null || relationName.isEmpty()) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"cannot create a relationship without a relation_name.", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP), span));
			}
			// get required properties
			PropertyData<?> pd = properties.getProperties().get(PropertyIds.SOURCE_ID);
			if (pd == null) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"cannot create a relationship without a sourceId.", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(
										String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_SOURCEID), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter
						.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_SOURCEID), span));
			}

			String sourceId = (String) pd.getFirstValue();
			if (null == sourceId || sourceId.length() == 0) {
				LOG.error("Create relationship exception: {}, repositoryId: {},  TraceId: {}",
						"cannot create a relationship without a sourceId.", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(
										String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_SOURCEID), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter
						.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_SOURCEID), span));
			}

			pd = properties.getProperties().get(PropertyIds.TARGET_ID);
			if (pd == null) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"cannot create a relationship without a targetId.", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(
										String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_TARGETID), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter
						.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_TARGETID), span));
			}
			String targetId = (String) pd.getFirstValue();
			if (null == targetId || targetId.length() == 0) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"cannot create a relationship without a targetId.", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(
										String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_TARGETID), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter
						.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITHOUT_TARGETID), span));
			}
			// TODO:after implement Version
			// boolean cmis11 = context.getCmisVersion() !=
			// CmisVersion.CMIS_1_0;
			if (!typeId.equalsIgnoreCase(CustomTypeId.CMIS_EXT_RELATIONSHIP.value())) {
				LOG.error("Create relationship typeId must use cmis base type: {}, repositoryId: {}, TraceId: {}",
						CustomTypeId.CMIS_EXT_RELATIONSHIP.value(), repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.TYPEID_MUST_USE_BASE_TYPE,
										CustomTypeId.CMIS_EXT_RELATIONSHIP.value()), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter.log(String.format(
						ErrorMessages.TYPEID_MUST_USE_BASE_TYPE, CustomTypeId.CMIS_EXT_RELATIONSHIP.value()), span));

			}
			TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
					tracingId, span);

			// check if the given type is a relationship type
			if (!typeDef.getBaseTypeId().equals(BaseTypeId.CMIS_RELATIONSHIP)) {
				LOG.error("Create relationship exception: {}, {}, repositoryId: {}, TraceId: {}",
						"cannot create a relationship with a non-relationship type: ", typeDef.getId(), repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITH_NON_RELATIONSHIP_TYPE),
								span), ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter
						.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP_WITH_NON_RELATIONSHIP_TYPE), span));
			}

			// set default properties
			Properties propertiesNew;
			Map<String, PropertyData<?>> propMap = properties.getProperties();
			Map<String, PropertyData<?>> propMapNew = setDefaultProperties(repositoryId, typeDef, propMap);
			if (propMapNew != propMap) { // NOSONAR
				propertiesNew = new PropertiesImpl(propMapNew.values());
			} else {
				propertiesNew = properties;
			}
			// validate ACL
			// TypeValidator.validateAcl(typeDef, aclAdd, aclRemove);
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			IBaseObject sourceObj = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, false, sourceId, null,
					typeId);
			IBaseObject targetObj = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, false, targetId, null,
					typeId);
			String sourceTypeId = sourceObj != null ? sourceObj.getTypeId() : null;
			String targetTypeId = targetObj != null ? targetObj.getTypeId() : null;
			if (sourceTypeId == null) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"wrong sourceId,SourceObject should not be null", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.WRONG_SOURCEID), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.WRONG_SOURCEID), span));
			}
			if (targetTypeId == null) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"wrong targetId,TargetObject should not be null", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.WRONG_TARGETID), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.WRONG_TARGETID), span));
			}
			validateRelationshipDocuments(repositoryId, relationName, sourceTypeId, targetTypeId, false,
					objectMorphiaDAO, typeId, tracingId, span);
			// get name from properties
			pd = propMap.get(PropertyIds.NAME);
			if (pd == null) {
				LOG.error("Create relationship exception: {}, repositoryId: {}, TraceId: {}",
						"cannot create a relationship without a Name.", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_RELATIONSHIP), span));
			}
			String name = (String) pd.getFirstValue();
			IBaseObject parent = null;
			PropertyData<?> pdType = properties.getProperties().get(PropertyIds.OBJECT_TYPE_ID);
			String objectTypeId = (String) pdType.getFirstValue();
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			if (folderId != null) {
				parent = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, folderId, null,
						typeId);
			} else {
				objectTypeId = objectTypeId.equals(BaseTypeId.CMIS_RELATIONSHIP.value()) ? objectTypeId = "@ROOT@"
						: objectTypeId;
				parent = DBUtils.BaseDAO.getByPath(repositoryId, principalIds, aclPropagation, "/" + objectTypeId,
						typeId);
				if (parent == null) {
					parent = DBUtils.BaseDAO.getByName(repositoryId, "@ROOT@", false, null, typeId);
				}
			}

			IRelationObject storedObject = createRelationshipObject(repositoryId, parent, name, secondaryObjectTypeIds,
					propMapNew, userObject, typeDef.getId(), policies, aclAdd, tracingId, span);
			// invokeObjectFlowServiceAfterCreate(storedObject, ObjectFlowType.CREATED,
			// null, userObject);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return storedObject;
		}

		/**
		 * inserting relationshipObject into mongoDB
		 */
		private static IRelationObject createRelationshipObject(String repositoryId, IBaseObject parentData,
				String name, List<String> secondaryObjectTypeId, Map<String, PropertyData<?>> properties,
				IUserObject userObject, String typeId, List<String> policies, AccessControlListImplExt aclAdd,
				String tracingId, ISpan parentSpan) throws CmisObjectNotFoundException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisNavigationService::createRelationshipObject", null);
			Map<String, Object> custom = readCustomPropetiesData(properties, secondaryObjectTypeId, repositoryId,
					typeId, userObject);
			MRelationObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MRelationObjectDAO.class);
			TokenImpl token = new TokenImpl(TokenChangeType.CREATED, System.currentTimeMillis());
			Tuple2<String, String> p = resolvePathForObject(parentData, name);

			PropertyData<?> objectIdProperty = properties.get(PropertyIds.OBJECT_ID);
			String objectId = objectIdProperty == null ? null : (String) objectIdProperty.getFirstValue();
			IRelationObject result = baseMorphiaDAO.createObjectFacade(name, BaseTypeId.CMIS_RELATIONSHIP, typeId,
					properties.get(PropertyIds.DESCRIPTION) == null ? ""
							: properties.get(PropertyIds.DESCRIPTION).getFirstValue().toString(),
					userObject == null ? null : userObject.getUserDN(),
					userObject == null ? null : userObject.getUserDN(), token, custom, aclAdd, p._2());
			if (result.equals(null)) {
				LOG.error("createRelationshipObject unknown relation object: {}, repositoryId: {}, TraceId: {}", result,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.IMPOSSIBLE_CREATE_FOLDER), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.IMPOSSIBLE_CREATE_FOLDER), span));
			}
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			IRelationObject itemObject = DBUtils.RelationshipDAO.getRelationshipByObjectId(repositoryId, principalIds,
					aclPropagation, result.getId(), null, typeId);
			if (itemObject != null) {
				LOG.error("Relationship object already present: {}, repositoryId: {}, TraceId: {}", itemObject.getId(),
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.TYPE_ID_PRESENT, typeId), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.TYPE_ID_PRESENT, typeId), span));
			}

			if (result instanceof ISettableBaseObject) {
				ISettableBaseObject settableBaseObject = (ISettableBaseObject) result;
				if (objectId != null && !objectId.isEmpty()) {
					settableBaseObject.setId(objectId);
				}
			}

			baseMorphiaDAO.commit(result, typeId);

			if (result != null) {
				LOG.debug("Created relationship: {}", result.getName());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		/**
		 * Validating the CMIS Relation Objects
		 */
		private static void validateRelationshipDocuments(String repositoryId, String relationName, String sourceTypeId,
				String targetTypeId, Boolean cmis11, MRelationObjectDAO baseMorphiaDAO, String typeId, String tracingId,
				ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::validateRelationshipDocuments", null);
			LOG.debug("ValidateRelationships documents for source: {}, target: {}", sourceTypeId, targetTypeId);

			IBaseObject relMdObj = DBUtils.BaseDAO.getByName(repositoryId, relationName, false, null, typeId);
			Map<String, Object> relationProps = relMdObj != null ? relMdObj.getProperties() : null;
			if (relationProps == null) {
				LOG.error("Relation_md object not present with name: {}, repositoryId: {}, TraceId: {}", relationName,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.RELATIONMD_NOT_PRESENT), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.RELATIONMD_NOT_PRESENT), span));
			}
			String sourceTable = relationProps.get("source_table").toString();
			if (sourceTable == null) {
				LOG.error("SourceTable  not present in relationship object: {}, repositoryId: {}, TraceId: {}",
						sourceTable, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.SOURCETABLE_NOT_PRESENT), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.SOURCETABLE_NOT_PRESENT), span));
			}
			String targetTable = relationProps.get("target_table").toString();
			if (targetTable == null) {
				LOG.error("target table  not present in relationship object: {}, repositoryId: {}, TraceId: {}",
						sourceTable, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.TARGETTABLE_NOT_PRESENT), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.TARGETTABLE_NOT_PRESENT), span));
			}
			if (!sourceTable.equalsIgnoreCase(sourceTypeId)) {
				LOG.error("Wrong source relationship document object: {}, repositoryId: {}, TraceId: {}", sourceTypeId,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.WRONG_RELATIONSHIP_OBJECT), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.WRONG_RELATIONSHIP_OBJECT), span));
			}
			if (!targetTable.equalsIgnoreCase(targetTypeId)) {
				LOG.error("Wrong target relationship document object: {}, repositoryId: {}, TraceId: {}", targetTable,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.WRONG_RELATIONSHIP_OBJECT), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.WRONG_RELATIONSHIP_OBJECT), span));
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		}

		/**
		 * Setting the DefaultProperties
		 */
		private static Map<String, PropertyData<?>> setDefaultProperties(String repositoryId, TypeDefinition typeDef,
				Map<String, PropertyData<?>> properties) {

			Map<String, PropertyData<?>> propertiesReturn = properties;
			Map<String, PropertyDefinition<?>> propDefs = typeDef.getPropertyDefinitions();
			boolean hasCopied = false;

			for (PropertyDefinition<?> propDef : propDefs.values()) {
				String propId = propDef.getId();
				List<?> defaultVal = propDef.getDefaultValue();
				if (defaultVal != null && !defaultVal.isEmpty() && null == properties.get(propId)) {
					if (!hasCopied) {
						// copy because it is an unmodified collection
						propertiesReturn = new HashMap<String, PropertyData<?>>(properties);
						hasCopied = true;
					}
					Object value = propDef.getCardinality() == Cardinality.SINGLE ? defaultVal.get(0) : defaultVal;
					if (value instanceof Integer) {
						value = BigInteger.valueOf(((Integer) value).longValue());
					}
					if (value instanceof Double) {
						value = BigDecimal.valueOf(((Double) value).longValue());
					}
					CopperCmsRepository fObject = new CopperCmsRepository(repositoryId);
					PropertyData<?> pd = fObject.getObjectFactory().createPropertyData(propDef, value);
					// set property:
					propertiesReturn.put(propId, pd);
				}
			}
			LOG.debug("Default properties: {}", propertiesReturn);

			return propertiesReturn;
		}

		/**
		 * create a policy for particular folderId
		 */
		public static String createPolicy(String repositoryId, Properties properties, String folderId,
				List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject, String tracingId,
				ISpan parentSpan, Map<String, String> headers)
				throws CmisInvalidArgumentException, CmisObjectNotFoundException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createPolicy", null);
			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.CREATE), false);
			if (permission) {
				IBaseObject policy = createPolicyIntern(repositoryId, properties, typeId, folderId, policies, addAces,
						removeAces, userObject, tracingId, span, headers);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return policy.getId();
			} else {
				LOG.error("Create type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.CREATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * returns an CMISOject for particular folderId
		 */
		@SuppressWarnings("unchecked")
		private static IBaseObject createPolicyIntern(String repositoryId, Properties properties, String objectTypeId,
				String folderId, List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject,
				String tracingId, ISpan parentSpan, Map<String, String> headers)
				throws CmisInvalidArgumentException, CmisObjectNotFoundException, IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createPolicyIntern", null);
			if (addAces != null && removeAces != null) {
				LOG.debug("Adding aces: {}, removing aces: {}", addAces.getAces(), removeAces.getAces());
			}
			invokeObjectFlowServiceBeforeCreate(repositoryId, folderId, properties, policies, addAces, removeAces,
					userObject, null, ObjectFlowType.CREATED);
			AccessControlListImplExt aclAdd = TypeValidators.impl
					.expandAclMakros(userObject == null ? null : userObject.getUserDN(), addAces);
			Acl aclRemove = TypeValidators.impl.expandAclMakros(userObject == null ? null : userObject.getUserDN(),
					removeAces);

			// get required properties
			List<String> secondaryObjectTypeIds = null;

			PropertyData<?> pd = properties.getProperties().get(PropertyIds.NAME);
			String policyName = (String) pd.getFirstValue();
			if (null == policyName || policyName.length() == 0) {
				LOG.error("createPolicyIntern unknown policy name: {}, repositoryId: {}, TraceId: {}", policyName,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_POLICY), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.CANNOT_CREATE_POLICY), span));
			}
			if (!NameValidator.impl.isValidName(policyName)) {
				LOG.error("createPolicyIntern error: {}, : {}, repositoryId: {}, TraceId: {}",
						NameValidator.ERROR_ILLEGAL_NAME, policyName, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter
								.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.ERROR, NameValidator.ERROR_ILLEGAL_NAME), span));
			}

			PropertyData<?> secondaryObjectType = properties.getProperties().get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (secondaryObjectType != null) {
				secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
			}

			String typeId = getObjectTypeId(properties, repositoryId, tracingId, span);
			TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, typeId, null, userObject,
					tracingId, span);
			if (typeDef == null) {
				LOG.error("createPolicyIntern unknown typeId: {}, repositoryId: {}, TraceId: {}", typeDef, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, typeId), span));
			}
			// check if the given type is a folder type
			if (!typeDef.getBaseTypeId().equals(BaseTypeId.CMIS_POLICY)) {
				LOG.error(
						"createPolicyIntern cannot create a policy with a non-folder type: {}, repositoryId: {}, TraceId: {}",
						typeDef.getId(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService()
						.updateSpan(span,
								TracingErrorMessage.message(TracingWriter.log(String.format(
										ErrorMessages.CANNOT_CREATE_POLICY_WITH_NON_FOLDER, typeDef.getId()), span),
										ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(TracingWriter
						.log(String.format(ErrorMessages.CANNOT_CREATE_POLICY_WITH_NON_FOLDER, typeDef.getId()), span));
			}
			PropertiesImpl props = compileWriteProperties(repositoryId, typeDef, userObject, properties, null,
					tracingId, span);
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			IBaseObject parent = null;
			if (folderId != null) {
				folderId = folderId.equals("@ROOT@") ? folderId = objectTypeId : folderId;
				if (folderId.matches("-?[0-9a-fA-F]+")) {
					parent = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, folderId, null,
							typeId);
				} else {
					parent = DBUtils.BaseDAO.getByName(repositoryId, folderId, false, null, typeId);
				}
			} else {
				objectTypeId = objectTypeId.equals(BaseTypeId.CMIS_POLICY.value()) ? objectTypeId = "@ROOT@"
						: objectTypeId;
				parent = DBUtils.BaseDAO.getByPath(repositoryId, principalIds, aclPropagation, "/" + objectTypeId,
						typeId);
				if (parent == null) {
					parent = DBUtils.BaseDAO.getByName(repositoryId, "@ROOT@", false, null, typeId);
				}
			}
			if (parent == null) {
				LOG.error("createPolicyIntern parent is unknown:{}, repositoryId: {}, TraceId: {}", parent,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PARENT), span));
			}

			LOG.debug("parentFolderData id: {}, path: {}, getInternalPath: {}", parent.getId(), parent.getPath(),
					parent.getInternalPath());

			PropertyData<?> objectIdProperty = properties.getProperties().get(PropertyIds.OBJECT_ID);
			String objectId = objectIdProperty == null ? null : (String) objectIdProperty.getFirstValue();
			IBaseObject result = createPolicyObject(repositoryId, parent, objectId, policyName, userObject,
					secondaryObjectTypeIds, typeId, props.getProperties(), policies, aclAdd, aclRemove, tracingId,
					span);
			invokeObjectFlowServiceAfterCreate(result, ObjectFlowType.CREATED, null, userObject, headers, repositoryId);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		/**
		 * Inserting CMISPolicyObject into MongoDB
		 */
		private static IBaseObject createPolicyObject(String repositoryId, IBaseObject parentData, String objectId,
				String policyName, IUserObject userObject, List<String> secondaryObjectTypeIds, String typeId,
				Map<String, PropertyData<?>> properties, List<String> polices, AccessControlListImplExt aclAdd,
				Acl aclRemove, String tracingId, ISpan parentSpan) throws IllegalArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::createPolicyObject", null);

			Tuple2<String, String> p = resolvePathForObject(parentData, policyName);
			Map<String, Object> custom = readCustomPropetiesData(properties, secondaryObjectTypeIds, repositoryId,
					typeId, userObject);
			MBaseObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			IBaseObject policyObject = DBUtils.BaseDAO.getByName(repositoryId, policyName, false,
					parentData.getId().toString(), typeId);
			if (policyObject != null) {
				LOG.error("createPolicyObject policy Object already present: {}, repositoryId: {}, TraceId: {}",
						policyObject.getName(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter
								.log(String.format(ErrorMessages.POLICY_PRESENT, policyObject.getName()), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(
						TracingWriter.log(String.format(ErrorMessages.POLICY_PRESENT, policyObject.getName()), span));
			}

			TokenImpl token = new TokenImpl(TokenChangeType.CREATED, System.currentTimeMillis());
			IBaseObject result = baseMorphiaDAO.createObjectFacade(policyName, BaseTypeId.CMIS_POLICY, typeId,
					repositoryId, secondaryObjectTypeIds,
					properties.get(PropertyIds.DESCRIPTION) == null ? ""
							: properties.get(PropertyIds.DESCRIPTION).getFirstValue().toString(),
					userObject == null ? null : userObject.getUserDN(),
					userObject == null ? null : userObject.getUserDN(), token, p._1(), custom, polices, aclAdd, p._2(),
					parentData.getId().toString());

			if (result instanceof ISettableBaseObject) {
				ISettableBaseObject settableBaseObject = (ISettableBaseObject) result;
				if (objectId != null && !objectId.isEmpty()) {
					settableBaseObject.setId(objectId);
				}
			}
			baseMorphiaDAO.commit(result, typeId);
			if (result != null) {
				LOG.debug("Policy: {} created successfully", result.getName());
			}
			if (aclRemove != null)
				validateAcl(repositoryId, aclRemove, result.getId(), result);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		/**
		 * BulkUpdate the properties present in CMIS 1.1
		 */
		public static List<BulkUpdateObjectIdAndChangeToken> bulkUpdateProperties(String repositoryId,
				List<BulkUpdateObjectIdAndChangeToken> objectIdAndChangeToken, Properties properties,
				List<String> addSecondaryTypeIds, List<String> removeSecondaryTypeIds, ObjectInfoHandler objectInfos,
				IUserObject userObject, String tracingId, ISpan parentSpan, Map<String, String> headers) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::bulkUpdateProperties", null);
			List<BulkUpdateObjectIdAndChangeToken> result = new ArrayList<BulkUpdateObjectIdAndChangeToken>();
			for (BulkUpdateObjectIdAndChangeToken obj : objectIdAndChangeToken) {
				Holder<String> objId = new Holder<String>(obj.getId());
				Holder<String> changeToken = new Holder<String>(obj.getChangeToken());
				try {
					String typeId = CmisPropertyConverter.Impl.getTypeIdForObject(repositoryId, null, obj.getId(),
							null);
					updateProperties(repositoryId, objId, changeToken, properties, null, objectInfos, userObject,
							typeId, tracingId, span, headers);
					result.add(new BulkUpdateObjectIdAndChangeTokenImpl(obj.getId(), changeToken.getValue()));
				} catch (Exception e) {
					LOG.error(
							"updating properties in bulk upadate failed for object: {}, {}, repositoryId: {}, TraceId: {}",
							obj.getId(), e, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span),
									ErrorMessages.EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				}
			}
			if (result != null) {
				LOG.debug("updating properties in bulk upadate success object count: {}", result.size());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		/**
		 * Update the properties
		 */
		@SuppressWarnings("unchecked")
		public static void updateProperties(String repositoryId, Holder<String> objectId, Holder<String> changeToken,
				Properties properties, Acl acl, ObjectInfoHandler objectInfos, IUserObject userObject, String typeId,
				String tracingId, ISpan parentSpan, Map<String, String> headers)
				throws CmisRuntimeException, CmisObjectNotFoundException, CmisUpdateConflictException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::updateProperties", null);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.UPDATE), false);
			if (permission) {
				Map<String, Object> updatecontentProps = new HashMap<String, Object>();
				Map<String, Object> objectFlowUpdateContentProps = new HashMap<String, Object>();
				String id = objectId.getValue().trim();
				invokeObjectFlowServiceBeforeCreate(repositoryId, id, properties, null, acl, null, userObject, null,
						ObjectFlowType.UPDATED);
				if (properties == null) {
					LOG.error("Method name: {}, no properties given for object id: {}, repositoryId: {}, TraceId: {}",
							"updateProperties", id, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(TracingWriter
									.log(String.format(ErrorMessages.UPDATE_FAILED, objectId.getValue()), span),
									ErrorMessages.RUNTIME_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisRuntimeException(
							TracingWriter.log(String.format(ErrorMessages.UPDATE_FAILED, objectId.getValue()), span));
				}
				IBaseObject data = null;
				MBaseObjectDAO baseMorphiaDAO = null;
				MNavigationServiceDAO navigationMorphiaDAO = null;
				String systemAdmin = System.getenv("SYSTEM_ADMIN");
				boolean aclPropagation = Stream.of(userObject.getGroups())
						.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
				try {
					baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
							MBaseObjectDAO.class);
					navigationMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
							.getObjectService(repositoryId, MNavigationServiceDAO.class);
					String[] principalIds = Helpers.getPrincipalIds(userObject);
					data = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, id, null, typeId);
				} catch (MongoException e) {
					LOG.error("updateProperties unknown object: {}, repositoryId: {}, TraceId: {}", objectId,
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
				}
				if (changeToken != null && changeToken.getValue() != null
						&& Long.valueOf(data.getChangeToken().getTime()) > Long.valueOf(changeToken.getValue())) {
					LOG.error("updateProperties failed: changeToken does not match: {}, repositoryId: {},  TraceId: {}",
							changeToken, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.CHANGETOKEN_DOESNT_MATCH), span),
									ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisUpdateConflictException(
							TracingWriter.log(String.format(ErrorMessages.CHANGETOKEN_DOESNT_MATCH), span));
				}

				// only for updating name
				TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, data.getTypeId(), null,
						userObject, tracingId, span);
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);

				PropertyData<?> customData = properties.getProperties().get(PropertyIds.NAME);
				if (customData != null && customData.getId().equalsIgnoreCase(PropertyIds.NAME)) {
					updatecontentProps.put("name", customData.getFirstValue());
					updatecontentProps.put("path", gettingPath(data.getPath(), customData.getFirstValue()));
					if (data.getBaseId() == BaseTypeId.CMIS_FOLDER) {
						try {
							localService.rename(data.getPath(),
									gettingPath(data.getPath(), customData.getFirstValue()));
						} catch (Exception e) {
							LOG.error("updateProperties folder rename exception: {}, repositoryId: {}, TraceId: {}", e,
									repositoryId, span != null ? span.getTraceId() : null);
							TracingApiServiceFactory.getApiService().updateSpan(span,
									TracingErrorMessage.message(
											TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span),
											ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
							TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
							throw new IllegalArgumentException(
									TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span));
						}

						updateChildPath(repositoryId, data.getName(), customData.getFirstValue().toString(), id,
								baseMorphiaDAO, navigationMorphiaDAO, localService, userObject, data.getInternalPath(),
								data.getAcl(), data.getTypeId(), tracingId, span, headers);
					}
				}

				PropertiesImpl props = compileWriteProperties(repositoryId, typeDef, userObject, properties, data,
						tracingId, span);
				Long modifiedTime = System.currentTimeMillis();
				TokenImpl token = new TokenImpl(TokenChangeType.UPDATED, modifiedTime);
				updatecontentProps.put("modifiedAt", modifiedTime);
				updatecontentProps.put("modifiedBy", userObject == null ? null : userObject.getUserDN());
				updatecontentProps.put("token", token);
				String description = props.getProperties().get(PropertyIds.DESCRIPTION) != null
						? props.getProperties().get(PropertyIds.DESCRIPTION).getFirstValue().toString()
						: null;
				if (description != null) {
					updatecontentProps.put("description", description);
				}
				Set<Map.Entry<String, PropertyData<?>>> customData1 = props.getProperties().entrySet();
				for (Map.Entry<String, PropertyData<?>> customValues1 : customData1) {
					if (!(customValues1.getKey().equals(PropertyIds.SECONDARY_OBJECT_TYPE_IDS))) {
						PropertyData<?> valueName1 = customValues1.getValue();
						if (!valueName1.getValues().isEmpty()) {
							if (valueName1.getFirstValue().getClass().getSimpleName()
									.equalsIgnoreCase("GregorianCalendar")) {
								if (valueName1.getValues().size() == 1) {
									GregorianCalendar value = convertInstanceOfObject(valueName1.getFirstValue(),
											GregorianCalendar.class);
									Long time = value.getTimeInMillis();
									updatecontentProps.put("properties." + valueName1.getId(), time.longValue());
									objectFlowUpdateContentProps.put(valueName1.getId(),
											valueName1.getFirstValue().toString());
								} else {
									List<Long> valueList = new ArrayList<>();
									valueName1.getValues().forEach(v -> {
										GregorianCalendar value = convertInstanceOfObject(v, GregorianCalendar.class);
										Long time = value.getTimeInMillis();
										valueList.add(time.longValue());
									});
									updatecontentProps.put("properties." + valueName1.getId(), valueList);
									objectFlowUpdateContentProps.put(valueName1.getId(),
											(List<String>) valueName1.getValues());
								}

							} else if (valueName1.getFirstValue().getClass().getSimpleName()
									.equalsIgnoreCase("BigInteger")) {

								if (valueName1.getValues().size() == 1) {
									BigInteger valueBigInteger = convertInstanceOfObject(valueName1.getFirstValue(),
											BigInteger.class);
									int value = valueBigInteger.intValue();
									updatecontentProps.put("properties." + valueName1.getId(), value);
									objectFlowUpdateContentProps.put(valueName1.getId(),
											valueName1.getFirstValue().toString());
								} else {
									List<Integer> valueList = new ArrayList<>();
									valueName1.getValues().forEach(v -> {
										BigInteger valueBigInteger = convertInstanceOfObject(v, BigInteger.class);
										valueList.add(valueBigInteger.intValue());
									});
									updatecontentProps.put("properties." + valueName1.getId(), valueList);
									objectFlowUpdateContentProps.put(valueName1.getId(),
											(List<String>) valueName1.getValues());
								}

							} else if (valueName1.getFirstValue().getClass().getSimpleName()
									.equalsIgnoreCase("BigDecimal")) {

								if (valueName1.getValues().size() == 1) {
									BigDecimal value = convertInstanceOfObject(valueName1.getFirstValue(),
											BigDecimal.class);
									double doubleValue = value.doubleValue();
									updatecontentProps.put("properties." + valueName1.getId(), doubleValue);
									objectFlowUpdateContentProps.put(valueName1.getId(),
											valueName1.getFirstValue().toString().toString());
								} else {
									List<Double> valueList = new ArrayList<>();
									valueName1.getValues().forEach(v -> {
										BigDecimal value = convertInstanceOfObject(valueName1.getFirstValue(),
												BigDecimal.class);
										valueList.add(value.doubleValue());
									});
									updatecontentProps.put("properties." + valueName1.getId(), valueList);
									objectFlowUpdateContentProps.put(valueName1.getId(),
											(List<String>) valueName1.getValues());
								}

							} else {
								if (valueName1.getValues().size() == 1) {
									updatecontentProps.put("properties." + valueName1.getId(),
											valueName1.getFirstValue());
									objectFlowUpdateContentProps.put(valueName1.getId(), valueName1.getFirstValue());
								} else {
									updatecontentProps.put("properties." + valueName1.getId(), valueName1.getValues());
									objectFlowUpdateContentProps.put(valueName1.getId(),
											(List<String>) valueName1.getValues());
								}
							}
						} else {
							updatecontentProps.put("properties." + valueName1.getId(), valueName1.getValues());
							objectFlowUpdateContentProps.put(valueName1.getId(), (List<String>) valueName1.getValues());
						}
					}
				}
				baseMorphiaDAO.update(repositoryId, id, updatecontentProps, typeId);
				invokeObjectFlowServiceAfterCreate(data, ObjectFlowType.UPDATED, objectFlowUpdateContentProps,
						userObject, headers, repositoryId);
				if (updatecontentProps != null) {
					LOG.debug("updateProperties for: {}, object: {}", id, updatecontentProps);
				}
				if (properties.getProperties().containsKey(PropertyIds.SECONDARY_OBJECT_TYPE_IDS)) {
					List<String> secondaryTypes = data.getSecondaryTypeIds();
					List<String> secondaryObjectTypeIds = null;
					if (secondaryTypes == null) {
						secondaryTypes = new ArrayList<String>();
					}
					PropertyData<?> secondaryObjectType = properties.getProperties()
							.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
					if (secondaryObjectType != null) {
						secondaryObjectTypeIds = (List<String>) secondaryObjectType.getValues();
					}
					if (secondaryObjectTypeIds.isEmpty()) {
						DBUtils.BaseDAO.updateBaseSecondaryTypeObject(repositoryId, secondaryObjectTypeIds,
								data.getId(), typeId);
					} else {
						secondaryTypes.addAll(secondaryObjectTypeIds);
						secondaryTypes = secondaryTypes.stream().distinct().collect(Collectors.toList());
						DBUtils.BaseDAO.updateBaseSecondaryTypeObject(repositoryId, secondaryTypes, data.getId(),
								typeId);
					}
					if (secondaryTypes != null) {
						LOG.debug("updateSecondaryProperties for: {}, object: {}", id, secondaryTypes);
					}
				}
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			} else {
				LOG.error("Update type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		private static void updateChildPath(String repositoryId, String oldName, String newName, String id,
				MBaseObjectDAO baseMorphiaDAO, MNavigationServiceDAO navigationMorphiaDAO, IStorageService localService,
				IUserObject userObject, String dataPath, AccessControlListImplExt dataAcl, String typeId,
				String tracingId, ISpan parentSpan, Map<String, String> headers) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::updateChildPath", null);
			if (id != null) {
				LOG.info("updateChildPath for object: {}", id);
			}
			String path = "," + id.toString() + ",";
			List<? extends IBaseObject> children = getDescendants(repositoryId, path, dataPath, dataAcl,
					navigationMorphiaDAO, userObject, typeId);
			if (children.size() > 0) {
				for (IBaseObject child : children) {
					Map<String, Object> updatePath = new HashMap<>();
					String[] internalPath = child.getInternalPath().split(",");
					int folderpathId = getUpdateFolderPathId(internalPath, id);
					updatePath.put("path", gettingFolderPath(child.getPath(), newName, oldName, folderpathId));
					try {
						localService.rename(child.getPath(),
								gettingFolderPath(child.getPath(), newName, oldName, folderpathId));
					} catch (Exception e) {
						LOG.error("updateProperties folder rename exception: {}, repositoryId: {}, TraceId: {}", e,
								repositoryId, span != null ? span.getTraceId() : null);
						TracingApiServiceFactory.getApiService().updateSpan(span,
								TracingErrorMessage.message(
										TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span),
										ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
						TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
						throw new IllegalArgumentException(
								TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span));
					}

					baseMorphiaDAO.update(repositoryId, child.getId(), updatePath, typeId);
					invokeObjectFlowServiceAfterCreate(child, ObjectFlowType.UPDATED, null, userObject, headers, repositoryId);
				}
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		}

		private static String gettingPath(String path, Object newName) {
			String[] folderNames = path.split("/");
			String root = null;
			folderNames[folderNames.length - 1] = newName.toString();
			for (String folderName : folderNames) {
				if (!folderName.isEmpty()) {
					if (root == null) {
						root = "/" + folderName;
					} else {
						root = root + "/" + folderName;
					}

				}
			}
			return root;

		}

		private static String gettingFolderPath(String path, Object newName, String oldName, int folderpathId) {
			String[] folderNames = path.split("/");
			int i = 0;
			String root = null;
			for (String folderName : folderNames) {
				if (!folderName.isEmpty()) {
					if (folderName.equals(oldName)) {
						if (i == folderpathId) {
							folderNames[i] = newName.toString();
						}
					}
				}
				i++;
			}

			for (String folderName : folderNames) {
				if (!folderName.isEmpty()) {
					if (root == null) {
						root = "/" + folderName;
					} else {
						root = root + "/" + folderName;
					}

				}

			}
			return root;
		}

		/**
		 * CMIS getContentStream.
		 */
		public static ContentStream getContentStream(String repositoryId, String objectId, String streamId,
				BigInteger offset, BigInteger length, IUserObject userObject, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::getContentStream", null);

			IDocumentObject docDetails = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId, null);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					docDetails.getTypeId(), EnumSet.of(PermissionType.VIEW_ONLY), false);
			if (permission) {
				try {
					Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
					IStorageService localService = StorageServiceFactory.createStorageService(parameters);
					ContentStream contentStream = null;
					if (docDetails.getContentStreamFileName() != null) {
						BigInteger Contentlength;
						if (offset != null) {
							Contentlength = BigInteger.valueOf(docDetails.getContentStreamLength()).subtract(offset);
						} else {
							Contentlength = BigInteger.valueOf(docDetails.getContentStreamLength());
						}
						String fileName = docDetails.getProperties() != null
								? (String) docDetails.getProperties().get(PropertyIds.CONTENT_STREAM_FILE_NAME) != null
										? (String) docDetails.getProperties().get(PropertyIds.CONTENT_STREAM_FILE_NAME)
										: docDetails.getContentStreamFileName()
								: docDetails.getContentStreamFileName();
						contentStream = localService.getContent(docDetails.getContentStreamFileName(),
								docDetails.getPath(), docDetails.getContentStreamMimeType(), Contentlength, fileName);
						LOG.info("ContentStream: {}", contentStream);
					}

					// if (contentStream.equals(null)) {
					// LOG.error("ContentStream should not be :{}",
					// contentStream);
					// throw new CmisObjectNotFoundException("Unkonwn
					// ObjectId");
					// }
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
					return contentStream;
				} catch (Exception e) {
					LOG.error("getContentStream this file does not exist: {}, repositoryId: {}, TraceId: {}", objectId,
							repositoryId, span != null ? span.getTraceId() : null);
					MetricsInputs.markDownloadErrorMeter();
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span));
				}
			} else {
				LOG.error("viewonly type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.VIEWONLY_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.VIEWONLY_PERMISSION_DENIED, userObject.getUserDN()), span));
			}

		}

		/**
		 * set the contentStream.
		 */
		public static void setContentStream(String repositoryId, Holder<String> objectId, Boolean overwrite,
				Holder<String> changeToken, ContentStream contentStream, IUserObject userObject, String tracingId,
				ISpan parentSpan, Map<String, String> headers) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::setContentStream", null);
			String id = objectId.getValue().trim();
			IDocumentObject object = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId.getValue().trim(),
					null);

			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					object.getTypeId(), EnumSet.of(PermissionType.READ, PermissionType.UPDATE), false);
			if (permission) {
				MDocumentObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MDocumentObjectDAO.class);
				Map<String, Object> updatecontentProps = new HashMap<String, Object>();
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);

				if (changeToken != null && changeToken.getValue() != null
						&& Long.valueOf(object.getChangeToken().getTime()) > Long.valueOf(changeToken.getValue())) {
					LOG.error("set content failed changeToken does not match: {}, repositoryId: {}, TraceId: {}",
							changeToken, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.SET_CONTENT_FAILED), span),
									ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisUpdateConflictException(
							TracingWriter.log(String.format(ErrorMessages.SET_CONTENT_FAILED), span));
				}
				Map<String, Object> updateProps = new HashMap<String, Object>();
				if (overwrite != null && overwrite.booleanValue()) {
					if (contentStream != null && contentStream.getStream() != null) {
						localService.setContent(object.getId().toString(), contentStream.getFileName(),
								object.getContentStreamFileName(), object.getPath(), contentStream);

						// update name and path, since its possible a new file
						// is
						// uploaded on this for replacing
						updateProps.put("name", contentStream.getFileName());
						updateProps.put("path", gettingPath(object.getPath(), contentStream.getFileName()));
					}
				}
				if (overwrite != null && overwrite.booleanValue()) {
					TokenImpl token = new TokenImpl(TokenChangeType.UPDATED, System.currentTimeMillis());
					updatecontentProps.put("token", token);
					updatecontentProps.put("modifiedAt", token.getTime());
					updatecontentProps.put("contentStreamLength", contentStream.getLength());
					updatecontentProps.put("contentStreamMimeType", contentStream.getMimeType());
					updatecontentProps.put("contentStreamFileName", contentStream.getFileName());
					baseMorphiaDAO.update(id, updatecontentProps);

				}

				invokeObjectFlowServiceAfterCreate(object, ObjectFlowType.UPDATED, null, userObject, headers, repositoryId);

				if (updatecontentProps != null) {
					LOG.debug("setContentStream updateObjects id: {}, properties: {}", id, updatecontentProps);
				}
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			} else {
				LOG.error("Update type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * append the contentStream present in CMIS 1.1.
		 */
		public static void appendContentStream(String repositoryId, Holder<String> objectId, Holder<String> changeToken,
				ContentStream appendStream, Boolean isLastChunk, IUserObject userObject, String tracingId,
				ISpan parentSpan, Map<String, String> headers) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::appendContentStream", null);
			String id = objectId.getValue().trim();
			IDocumentObject docDetails = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, id, null);

			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					docDetails.getTypeId(), EnumSet.of(PermissionType.READ, PermissionType.UPDATE), false);
			if (permission) {
				Map<String, Object> updatecontentProps = new HashMap<String, Object>();
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				MDocumentObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MDocumentObjectDAO.class);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);
				Long modifiedTime = System.currentTimeMillis();
				TokenImpl token = new TokenImpl(TokenChangeType.UPDATED, modifiedTime);
				updatecontentProps.put("token", token);
				if (isLastChunk != null) {
					if (appendStream != null && appendStream.getStream() != null) {
						ContentStream newContent = localService.appendContent(docDetails.getId().toString(),
								docDetails.getContentStreamFileName(), docDetails.getPath(), appendStream, isLastChunk);
						if (newContent != null && newContent.getStream() != null && appendStream != null
								&& appendStream.getStream() != null) {
							updatecontentProps.put("contentStreamLength", newContent.getLength());
							updatecontentProps.put("contentStreamMimeType", appendStream.getMimeType());
							updatecontentProps.put("contentStreamFileName", newContent.getFileName());
							updatecontentProps.put("modifiedAt", modifiedTime);
						} else if (newContent != null && newContent.getStream() != null) {
							updatecontentProps.put("contentStreamLength", newContent.getLength());
							updatecontentProps.put("contentStreamMimeType", newContent.getMimeType());
							updatecontentProps.put("contentStreamFileName", newContent.getFileName());
							updatecontentProps.put("modifiedAt", modifiedTime);
						}
					}
				}
				baseMorphiaDAO.update(id, updatecontentProps);
				invokeObjectFlowServiceAfterCreate(docDetails, ObjectFlowType.UPDATED, null, userObject, headers, repositoryId);

				LOG.debug("appendContentStream updateObjects for object: {}, {}", id, updatecontentProps);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			} else {
				LOG.error("Update type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * delete the contentStream.
		 */
		public static void deleteContentStream(String repositoryId, Holder<String> objectId, Holder<String> changeToken,
				IUserObject userObject, String tracingId, ISpan parentSpan, Map<String, String> headers) throws CmisStorageException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::deleteContentStream", null);
			String id = objectId.getValue().trim();
			IDocumentObject docDetails = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, id, null);

			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					docDetails.getTypeId(), EnumSet.of(PermissionType.READ, PermissionType.DELETE), false);
			if (permission) {
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				MDocumentObjectDAO docorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MDocumentObjectDAO.class);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);
				if (changeToken != null && changeToken.getValue() != null
						&& Long.valueOf(docDetails.getChangeToken().getTime()) > Long.valueOf(changeToken.getValue())) {
					LOG.error("delete content failed: changeToken does not match in repositoryId: {}, TraceId: {}",
							changeToken, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.DELETE_CONTENT_FAILED), span),
									ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisUpdateConflictException(
							TracingWriter.log(String.format(ErrorMessages.DELETE_CONTENT_FAILED), span));
				}
				boolean contentDeleted = localService.deleteContent(docDetails.getContentStreamFileName(),
						docDetails.getPath(), docDetails.getContentStreamMimeType());
				if (!contentDeleted) {
					// LOG.error("Unknown ContentStreamID:{}", objectId);
					// throw new CmisStorageException("Deletion content
					// failed!");
				}
				List<String> updatecontentProps = new ArrayList<>();
				updatecontentProps.add("contentStreamLength");
				updatecontentProps.add("contentStreamMimeType");
				updatecontentProps.add("contentStreamFileName");
				updatecontentProps.add("contentStreamId");
				TokenImpl updateToken = new TokenImpl(TokenChangeType.UPDATED, System.currentTimeMillis());
				docorphiaDAO.delete(id, updatecontentProps, false, true, updateToken);

				invokeObjectFlowServiceAfterCreate(docDetails, ObjectFlowType.DELETED, null, userObject, headers, repositoryId);

				if (updatecontentProps != null) {
					LOG.debug("deleteContentStream, removeFields id: {}, properties: {}", id, updatecontentProps);
				}
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			} else {
				LOG.error("Delete type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.DELETE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.DELETE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * delete the object.
		 */
		public static void deleteObject(String repositoryId, String objectId, Boolean allVersions, Boolean forceDelete,
				IUserObject userObject, String typeId, String tracingId, ISpan parentSpan, Map<String, String> headers)
				throws CmisObjectNotFoundException, CmisNotSupportedException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::deleteObject", null);
			invokeObjectFlowServiceBeforeCreate(repositoryId, objectId, null, null, null, null, userObject, allVersions,
					ObjectFlowType.DELETED);
			IBaseObject data = null;
			MBaseObjectDAO baseMorphiaDAO = null;
			MDocumentObjectDAO docMorphiaDAO = null;
			MNavigationServiceDAO navigationMorphiaDAO = null;
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			try {
				baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
						MBaseObjectDAO.class);
				docMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
						MDocumentObjectDAO.class);
				navigationMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
						MNavigationServiceDAO.class);
				String[] principalIds = Helpers.getPrincipalIds(userObject);
				data = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, objectId, null,
						typeId);
			} catch (MongoException e) {
				LOG.error("deleteObject object not found in repositoryId: {}, TraceId: {}", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span),
								ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisUpdateConflictException(
						TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span));
			}
			if (data == null) {
				LOG.error("deleteObject Object id: {}, null in : {} repository!, TraceId: {}", objectId, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.OBJECT_NULL), span));
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					data.getTypeId(), EnumSet.of(PermissionType.READ, PermissionType.DELETE), false);
			if (permission) {
				if (data.getName().equalsIgnoreCase("@ROOT@")) {
					LOG.error("deleteObject failed: {}, repositoryId: {}, TraceId: {}", "can't delete a root folder.",
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.DELETE_OBJECT_FAILED), span),
									ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisNotSupportedException(
							TracingWriter.log(String.format(ErrorMessages.DELETE_OBJECT_FAILED), span));
				}
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);

				deleteObjectChildrens(repositoryId, data, baseMorphiaDAO, docMorphiaDAO, navigationMorphiaDAO,
						localService, userObject, allVersions, forceDelete, aclPropagation, typeId, tracingId, span, headers);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			} else {
				LOG.error("Delete type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.DELETE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.DELETE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * delete ObjectChildren
		 */
		public static void deleteObjectChildrens(String repositoryId, IBaseObject data, MBaseObjectDAO baseMorphiaDAO,
				MDocumentObjectDAO docMorphiaDAO, MNavigationServiceDAO navigationMorphiaDAO,
				IStorageService localService, IUserObject userObject, Boolean allVersions, Boolean forceDelete,
				boolean aclPropagation, String typeId, String tracingId, ISpan parentSpan, Map<String, String> headers) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::deleteObjectChildrens", null);
			String[] principalIds = Helpers.getPrincipalIds(userObject);

			if (data == null) {
				LOG.error("deleteObjectChildrens object not found! in repositoryId: {}, TraceId: {}", repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span));
			}
			TokenImpl token = new TokenImpl(TokenChangeType.DELETED, System.currentTimeMillis());
			RepositoryManagerFactory.getFileDetails(repositoryId);

			if (data.getBaseId() == BaseTypeId.CMIS_FOLDER) {
				String path = "," + data.getId() + ",";
				List<? extends IBaseObject> children = getDescendants(repositoryId, path, data.getInternalPath(),
						data.getAcl(), navigationMorphiaDAO, userObject, typeId);
				if (data != null && children != null) {
					LOG.debug("descendants for object: {}, {}", data.getId(), children);
				}

				for (IBaseObject child : children) {
					if (child.getBaseId().toString().equalsIgnoreCase(BaseTypeId.CMIS_DOCUMENT.value())) {
						IDocumentObject doc = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, child.getId(),
								null);
						// boolean contentDeleted =
						// localService.deleteContent(doc.getContentStreamFileName(),
						// null);
						// if (!contentDeleted) {
						// LOG.error("Unknown ContentStreamID:{}",
						// child.getId());
						// throw new CmisStorageException("Deletion content
						// failed!");
						// }

						invokeObjectFlowServiceAfterCreate(doc, ObjectFlowType.DELETED, null, userObject, headers, repositoryId);
					}
					baseMorphiaDAO.delete(repositoryId, principalIds, child.getId(),
							forceDelete == null ? false : forceDelete, aclPropagation, token, typeId);
				}
				localService.deleteFolder(data.getPath());
				baseMorphiaDAO.delete(repositoryId, principalIds, data.getId(),
						forceDelete == null ? false : forceDelete, aclPropagation, token, typeId);

				LOG.info("ObjectId: {}, with baseType: {} is deleted", data.getId(), data.getBaseId());

			} else if (data.getBaseId() == BaseTypeId.CMIS_DOCUMENT && allVersions == false) {
				IDocumentObject doc = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, data.getId(), null);
				if (doc != null) {
					String previousVersionObjectId = doc.getPreviousVersionObjectId();
					if (previousVersionObjectId != null) {
						Map<String, Object> updateProps = new HashMap<String, Object>();
						updateProps.put("isLatestVersion", true);
						docMorphiaDAO.update(previousVersionObjectId, updateProps);
					}
					if (doc.getContentStreamFileName() != null) {
						boolean contentDeleted = localService.deleteContent(doc.getContentStreamFileName(),
								doc.getPath(), doc.getContentStreamMimeType());
						invokeObjectFlowServiceAfterCreate(doc, ObjectFlowType.DELETED, null, userObject, headers, repositoryId);
						if (!contentDeleted) {
							// LOG.error("Unknown ContentStreamID:{}",
							// doc.getId());
							// throw new CmisStorageException("Deletion content
							// failed!");
						}
					}
					baseMorphiaDAO.delete(repositoryId, principalIds, data.getId(),
							forceDelete == null ? false : forceDelete, aclPropagation, token, typeId);
					LOG.info("Object: {}, with baseType:{} is deleted", data.getId(), data.getBaseId());
				}
			} else if (data.getBaseId() == BaseTypeId.CMIS_DOCUMENT && allVersions == true) {
				IDocumentObject doc = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, data.getId(), null);
				String versionRefId = doc.getVersionReferenceId();
				List<? extends IDocumentObject> versionedDocument = DBUtils.DocumentDAO.getAllVersion(repositoryId,
						versionRefId);
				for (IDocumentObject document : versionedDocument) {
					if (document.getContentStreamFileName() != null) {
						boolean contentDeleted = localService.deleteContent(document.getContentStreamFileName(),
								document.getPath(), document.getContentStreamMimeType());
						invokeObjectFlowServiceAfterCreate(doc, ObjectFlowType.DELETED, null, userObject, headers, repositoryId);
						if (!contentDeleted) {
							// LOG.error("Unknown ContentStreamID:{}",
							// doc.getId());
							// throw new CmisStorageException("Deletion content
							// failed!");
						}
					}
					baseMorphiaDAO.delete(repositoryId, principalIds, document.getId(),
							forceDelete == null ? false : forceDelete, aclPropagation, token, typeId);
					LOG.info("Object: {}, with baseType: {}, document: {} deleted", data.getId(), data.getBaseId(),
							document.getId());

				}
			} else {
				baseMorphiaDAO.delete(repositoryId, principalIds, data.getId(),
						forceDelete == null ? false : forceDelete, aclPropagation, token, typeId);
				invokeObjectFlowServiceAfterCreate(data, ObjectFlowType.DELETED, null, userObject, headers, repositoryId);
				LOG.info("Object: {}, with baseType: {} is deleted", data.getId(), data.getBaseId());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		}

		/**
		 * deleteTree from a set of properties.
		 */
		public static FailedToDeleteData deleteTree(String repositoryId, String folderId, Boolean allVers,
				Boolean forceDelete, UnfileObject unfile, Boolean continueOnFail, IUserObject userObject, String typeId,
				String tracingId, ISpan parentSpan) throws CmisObjectNotFoundException, CmisInvalidArgumentException,
				CmisNotSupportedException, CmisStorageException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::deleteTree", null);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.READ, PermissionType.DELETE), false);
			if (permission) {
				invokeObjectFlowServiceBeforeCreate(repositoryId, folderId, null, null, null, null, userObject, allVers,
						ObjectFlowType.DELETED);
				// boolean allVersions = (null == allVers ? true : allVers);
				UnfileObject unfileObjects = (null == unfile ? UnfileObject.DELETE : unfile);
				// boolean continueOnFailure = (null == continueOnFail ? false :
				// continueOnFail);
				String[] principalIds = Helpers.getPrincipalIds(userObject);
				String systemAdmin = System.getenv("SYSTEM_ADMIN");
				boolean aclPropagation = Stream.of(userObject.getGroups())
						.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
				IBaseObject data = null;
				MBaseObjectDAO baseMorphiaDAO = null;
				MNavigationServiceDAO navigationMorphiaDAO = null;
				try {
					baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
							MBaseObjectDAO.class);
					navigationMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
							.getObjectService(repositoryId, MNavigationServiceDAO.class);

					data = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, folderId, null,
							typeId);
				} catch (MongoException e) {
					LOG.error("deleteTree object not found: {}, repositoryId: {}, TraceId: {}", folderId, repositoryId,
							span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span));
				}
				List<String> failedToDeleteIds = new ArrayList<String>();
				FailedToDeleteDataImpl result = new FailedToDeleteDataImpl();
				if (data == null) {
					LOG.error("deleteTree cannot delete object with id: {}, {}, repositoryId: {}, TraceId: {}",
							folderId, "Object does not exist.", repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.CANNOT_DELETE, folderId), span),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisInvalidArgumentException(
							TracingWriter.log(String.format(ErrorMessages.CANNOT_DELETE, folderId), span));
				}

				if (!(data.getBaseId() == BaseTypeId.CMIS_FOLDER)) {
					LOG.error(
							"deleteTree can only be invoked on a folder but id: {}  does not refer to a folder, repositoryId: {}, TraceId: {}",
							folderId, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.CANNOT_BE_INVOKED), span),
									ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisInvalidArgumentException(
							TracingWriter.log(String.format(ErrorMessages.CANNOT_BE_INVOKED), span));
				}

				if (unfileObjects == UnfileObject.UNFILE) {
					LOG.error("deleteTree: {}, repositoryId: {}, TraceId: {}",
							" This repository does not support unfile operations.", repositoryId,
							span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.DOESNT_SUPPORT), span),
									ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisNotSupportedException(
							TracingWriter.log(String.format(ErrorMessages.DOESNT_SUPPORT), span));
				}

				// check if it is the root folder
				if (data.getName().equalsIgnoreCase("@ROOT@")) {
					LOG.error("deleteTree: {}, repositoryId: {}, TraceId: {}", "can't delete a root folder.",
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.DELETE_OBJECT_FAILED), span),
									ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisNotSupportedException(
							TracingWriter.log(String.format(ErrorMessages.DELETE_OBJECT_FAILED), span));
				}

				@SuppressWarnings("unused")
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				// IStorageService localService = null;
				String path = "," + folderId + ",";
				List<? extends IBaseObject> children = getDescendants(repositoryId, path, data.getInternalPath(),
						data.getAcl(), navigationMorphiaDAO, userObject, data.getTypeId());
				if (children != null && data != null) {
					LOG.debug("descendants for object: {}, {}", data.getId(), children);
				}
				TokenImpl token = new TokenImpl(TokenChangeType.DELETED, System.currentTimeMillis());
				// TODO: optimize here
				for (IBaseObject child : children) {
					baseMorphiaDAO.delete(repositoryId, principalIds, child.getId(),
							forceDelete == null ? false : forceDelete, aclPropagation, token, child.getTypeId());
					IBaseObject childCheck = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation,
							child.getId(), null, child.getTypeId());
					if (childCheck != null) {
						failedToDeleteIds.add(child.getId().toString());
					}
				}
				// TODO: optimize here
				// IStorageService localService =
				// MongoStorageDocument.createStorageService(parameters);
				// localService.deleteFolder(data.getPath());
				baseMorphiaDAO.delete(repositoryId, principalIds, folderId, forceDelete == null ? false : forceDelete,
						aclPropagation, token, data.getTypeId());
				IBaseObject parentCheck = DBUtils.BaseDAO.getByObjectId(repositoryId, null, false, folderId, null,
						data.getTypeId());
				if (parentCheck != null) {
					failedToDeleteIds.add(folderId.toString());
				}
				LOG.info("failedToDeleteIds for folder: {}, {}", folderId, failedToDeleteIds);
				result.setIds(failedToDeleteIds);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return result;
			} else {
				LOG.error("Delete type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.DELETE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.DELETE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		/**
		 * move object from source to target
		 */

		public static ObjectData moveObject(String repositoryId, Holder<String> objectId, String targetFolderId,
				String sourceFolderId, ObjectInfoHandler objectInfos, IUserObject userObject, String typeId,
				String tracingId, ISpan parentSpan) throws CmisObjectNotFoundException, CmisNotSupportedException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::moveObject", null);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					typeId, EnumSet.of(PermissionType.READ, PermissionType.UPDATE), false);
			if (permission) {
				IBaseObject data = null;
				MBaseObjectDAO baseMorphiaDAO = null;
				MNavigationDocServiceDAO navigationMorphiaDAO = null;
				MDocumentObjectDAO documentMorphiaDAO = null;
				String[] principalIds = Helpers.getPrincipalIds(userObject);
				String systemAdmin = System.getenv("SYSTEM_ADMIN");
				boolean aclPropagation = Stream.of(userObject.getGroups())
						.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
				try {
					baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
							MBaseObjectDAO.class);
					navigationMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
							.getObjectService(repositoryId, MNavigationDocServiceDAO.class);
					documentMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
							MDocumentObjectDAO.class);
					String id = objectId.getValue();
					data = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, id, null, typeId);
				} catch (MongoException e) {
					LOG.error("moveObject object not found:{}, repositoryId: {}, TraceId: {}", objectId.getValue(),
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.OBJECT_NOT_FOUND), span));
				}
				if (data == null) {
					LOG.error("moveObject unknown object: {}, repositoryId: {}, TraceId: {}", objectId.getValue(),
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(TracingWriter
									.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId.getValue()), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId.getValue()), span));

				}
				if (data.getName().equalsIgnoreCase("@ROOT@")) {
					LOG.error("moveObject rootFolder: {}, have no move operation: {}, repositoryId: {}, TraceId: {}",
							data.getId(), objectId.getValue(), repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.NO_MOVE_OPERATION), span),
									ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisNotSupportedException(
							TracingWriter.log(String.format(ErrorMessages.NO_MOVE_OPERATION), span));
				}
				IBaseObject soTarget = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation,
						targetFolderId, null, data.getTypeId());
				if (null == soTarget) {
					LOG.error("moveObject unknown target folder: {}, repositoryId: {}, TraceId: {}", targetFolderId,
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TARGET_FOLDER), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TARGET_FOLDER), span));
				} else if (soTarget.getBaseId() == BaseTypeId.CMIS_FOLDER) {
				} else {
					LOG.error(
							"moveObject destination move operation must be a folder: {}, repositoryId: {}, TraceId: {}",
							targetFolderId, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.DESTINATION_MUST_BE_FOLDER), span),
									ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisNotSupportedException(
							TracingWriter.log(String.format(ErrorMessages.DESTINATION_MUST_BE_FOLDER), span));
				}
				Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
				IStorageService localService = StorageServiceFactory.createStorageService(parameters);
				IBaseObject soSource = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation,
						sourceFolderId, null, data.getTypeId());
				if (null == soSource) {
					LOG.error("moveObject unknown source folder: {}, repositoryId: {}, TraceId: {}", sourceFolderId,
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.UNKNOWN_SOURCE_FOLDER), span),
									ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisObjectNotFoundException(
							TracingWriter.log(String.format(ErrorMessages.UNKNOWN_SOURCE_FOLDER), span));
				} else if (soSource.getBaseId() == BaseTypeId.CMIS_FOLDER) {
				} else {
					LOG.error("moveObject source move operation must be a folder:{}, TraceId: {}", sourceFolderId,
							span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.MOVE_MUST_BE_FOLDER), span),
									ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisNotSupportedException(
							TracingWriter.log(String.format(ErrorMessages.MOVE_MUST_BE_FOLDER), span));
				}
				String updatePath = soTarget.getInternalPath() + soTarget.getId() + ",";
				String cmisUpdatePath = soTarget.getPath() + "/" + data.getName();
				if (data.getBaseId() == BaseTypeId.CMIS_FOLDER || data.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
					if (data.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
						boolean checkParent = checkParentFolder(data.getPath(), soSource.getName());
						if (!checkParent) {
							LOG.error(
									"moveObject destination is not a parent folder for this document: {}, repositoryId: {}, TraceId: {}",
									targetFolderId, repositoryId, span != null ? span.getTraceId() : null);
							TracingApiServiceFactory.getApiService()
									.updateSpan(span,
											TracingErrorMessage.message(
													TracingWriter.log(
															String.format(
																	ErrorMessages.DESTINATION_NOT_A_PARENT_FOLDER),
															span),
													ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
							TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
							throw new CmisNotSupportedException(TracingWriter
									.log(String.format(ErrorMessages.DESTINATION_NOT_A_PARENT_FOLDER), span));

						}
						try {
							IDocumentObject docObj = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
									data.getId(), null);
							if (docObj.getContentStreamFileName() != null) {
								localService.moveDocument(docObj.getId().toString(), docObj.getContentStreamFileName(),
										data.getPath(), soTarget.getPath(), docObj.getContentStreamMimeType(),
										BigInteger.valueOf(docObj.getContentStreamLength()));
								LOG.info("Document: {}, moved from source: {}, to target: {} location",
										docObj.getContentStreamFileName(), data.getPath(), soTarget.getPath());
							}

						} catch (Exception ex) {
							LOG.error("Move Object failed: {}, repositoryId: {}, TraceId: {}", ex, repositoryId,
									span != null ? span.getTraceId() : null);
							TracingApiServiceFactory.getApiService().updateSpan(span,
									TracingErrorMessage.message(
											TracingWriter.log(String.format(ErrorMessages.MOVE_OBJECT_FAILED), span),
											ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
							TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
							throw new IllegalArgumentException(
									TracingWriter.log(String.format(ErrorMessages.MOVE_OBJECT_FAILED), span));
						}
					} else if (data.getBaseId() == BaseTypeId.CMIS_FOLDER) {
						try {
							localService.moveFolder(soTarget.getId().toString(), data.getPath(), soTarget.getPath());
							LOG.info("Folder: {}, moved from source: {}, to target location: {}, repositoryId: {}",
									soTarget.getId().toString(), data.getPath(), soTarget.getPath(), repositoryId);
						} catch (Exception ex) {
							LOG.error("Move Object Failed: {}, repositoryId: {}, TraceId: {}", ex, repositoryId,
									span != null ? span.getTraceId() : null);
							TracingApiServiceFactory.getApiService().updateSpan(span,
									TracingErrorMessage.message(
											TracingWriter.log(String.format(ErrorMessages.EXCEPTION, ex), span),
											ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
							TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
							throw new IllegalArgumentException(
									TracingWriter.log(String.format(ErrorMessages.EXCEPTION, ex), span));
						}
					}

				}

				Long modifiedTime = System.currentTimeMillis();
				TokenImpl token = new TokenImpl(TokenChangeType.UPDATED, modifiedTime);
				if (data.getBaseId() == BaseTypeId.CMIS_FOLDER) {
					Map<String, Object> updateProps = new HashMap<String, Object>();
					updateProps.put("parentId", soTarget.getId().toString());
					updateProps.put("internalPath", updatePath);
					updateProps.put("path", cmisUpdatePath);
					updateProps.put("modifiedAt", modifiedTime);
					updateProps.put("modifiedBy", userObject == null ? null : userObject.getUserDN());
					updateProps.put("token", token);
					baseMorphiaDAO.update(repositoryId, data.getId(), updateProps, typeId);
					if (data != null) {
						LOG.debug("update object id: {}, baseTypeId: {}, props: {}", data.getId(), data.getBaseId(),
								updateProps);
					}
					String path = data.getInternalPath() + data.getId() + ",";
					List<? extends IDocumentObject> children = getChildrens(repositoryId, path, data.getInternalPath(),
							data.getAcl(), navigationMorphiaDAO, userObject, data.getTypeId());
					if (children != null && children.size() > 0) {
						for (IDocumentObject child : children) {
							if (child.getBaseId() == BaseTypeId.CMIS_FOLDER) {
								String childPath = child.getInternalPath() + child.getId() + ",";
								String updateChildPath = updatePath + data.getId() + "," + child.getId() + ",";
								String cmisPath = cmisUpdatePath + "/" + child.getName();
								moveChildFolder(repositoryId, childPath, baseMorphiaDAO, navigationMorphiaDAO,
										documentMorphiaDAO, updateChildPath, updatePath, child.getId(), data.getId(),
										cmisPath, userObject, child.getInternalPath(), child.getAcl(), modifiedTime,
										child.getTypeId(), tracingId, span);
							} else {
								Map<String, Object> updatePropsDoc = new HashMap<String, Object>();
								String docName = child.getContentStreamFileName() != null
										? child.getContentStreamFileName()
										: child.getName();
								String cmisPath = cmisUpdatePath + "/" + docName;
								updatePropsDoc.put("internalPath", updatePath + data.getId() + ",");
								updatePropsDoc.put("path", cmisPath);
								updateProps.put("modifiedAt", modifiedTime);
								updateProps.put("modifiedBy", userObject == null ? null : userObject.getUserDN());
								if (child.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
									documentMorphiaDAO.update(child.getId(), updatePropsDoc);
								} else {
									baseMorphiaDAO.update(repositoryId, child.getId(), updatePropsDoc, typeId);
								}
								if (updatePropsDoc != null) {
									LOG.debug("update objects id: {}, baseType: {}, props: {}", child.getId(),
											child.getBaseId(), updatePropsDoc);
								}
							}
						}
					}

				} else {
					String updateDocPath = soTarget.getInternalPath() + soTarget.getId() + ",";
					IDocumentObject docObject = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, data.getId(),
							null);
					String docName = docObject.getContentStreamFileName() != null ? docObject.getContentStreamFileName()
							: data.getName();
					String cmisPath = soTarget.getPath() + "/" + docName;
					Map<String, Object> updateProps = new HashMap<String, Object>();
					updateProps.put("parentId", soTarget.getId().toString());
					updateProps.put("internalPath", updateDocPath);
					updateProps.put("path", cmisPath);
					updateProps.put("modifiedAt", modifiedTime);
					updateProps.put("modifiedBy", userObject == null ? null : userObject.getUserDN());
					updateProps.put("token", token);
					if (data.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
						documentMorphiaDAO.update(data.getId(), updateProps);
					} else {
						baseMorphiaDAO.update(repositoryId, data.getId(), updateProps, typeId);
					}
					if (updateProps != null) {
						LOG.debug("updateObjects: {}, baseType: {}, props: {}", data.getId(), data.getBaseId(),
								updateProps);
					}
				}
				LOG.info("getting object for this object id: {}", data.getId());
				ObjectData objectData = getObject(repositoryId, data.getId(), null, false, IncludeRelationships.NONE,
						null, false, false, objectInfos, userObject, data.getBaseId(), data.getTypeId(), tracingId,
						span);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return objectData;
			} else {
				LOG.error("Update type permission denied for this user: {}, repository: {}, TraceId: {}",
						userObject.getUserDN(), repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(
								String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span),
								ErrorMessages.ROLE_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRoleValidationException(TracingWriter
						.log(String.format(ErrorMessages.UPDATE_PERMISSION_DENIED, userObject.getUserDN()), span));
			}
		}

		private static boolean checkParentFolder(String path, String targetName) {
			String[] folderNames = path.split("/");
			String parentFolderName = folderNames[folderNames.length - 2];
			if (parentFolderName.equalsIgnoreCase(targetName)) {
				return true;
			}
			return false;

		}

		/**
		 * Checks the child folders
		 */
		private static void moveChildFolder(String repositoryId, String path, MBaseObjectDAO baseMorphiaDAO,
				MNavigationDocServiceDAO navigationMorphiaDAO, MDocumentObjectDAO documentMorphiaDAO, String childPath,
				String parentPath, String childId, String parentId, String cmisPath, IUserObject userObject,
				String dataPath, AccessControlListImplExt dataAcl, Long modifiedTime, String typeId, String tracingId,
				ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::moveChildFolder", null);
			if (childPath != null) {
				LOG.debug("moveChildFolder on childPath: {}", childPath);
			}
			parentPath = parentPath + parentId.toString() + ",";
			TokenImpl token = new TokenImpl(TokenChangeType.UPDATED, System.currentTimeMillis());
			Map<String, Object> updateProps = new HashMap<String, Object>();
			updateProps.put("parentId", parentId.toString());
			updateProps.put("internalPath", parentPath);
			updateProps.put("path", cmisPath);
			updateProps.put("modifiedAt", modifiedTime);
			updateProps.put("modifiedBy", userObject == null ? null : userObject.getUserDN());
			updateProps.put("token", token);
			baseMorphiaDAO.update(repositoryId, childId, updateProps, typeId);
			List<? extends IDocumentObject> children = getChildrens(repositoryId, path, dataPath, dataAcl,
					navigationMorphiaDAO, userObject, typeId);
			if (children != null && children.size() > 0) {
				for (IDocumentObject child : children) {
					if (child.getBaseId() == BaseTypeId.CMIS_FOLDER) {
						String childPaths = child.getInternalPath() + child.getId() + ",";
						String childUpdatePaths = childPath + child.getId() + ",";
						String cmisPaths = cmisPath + "/" + child.getName();
						moveChildFolder(repositoryId, childPaths, baseMorphiaDAO, navigationMorphiaDAO,
								documentMorphiaDAO, childUpdatePaths, parentPath, child.getId(), childId, cmisPaths,
								userObject, child.getInternalPath(), child.getAcl(), modifiedTime, child.getTypeId(),
								tracingId, span);
					} else {
						Map<String, Object> updatePropsDoc = new HashMap<String, Object>();
						String docName = child.getContentStreamFileName() != null ? child.getContentStreamFileName()
								: child.getName();
						String pathCmis = cmisPath + "/" + docName;
						updatePropsDoc.put("internalPath", childPath);
						updatePropsDoc.put("path", pathCmis);
						updateProps.put("modifiedAt", modifiedTime);
						updateProps.put("modifiedBy", userObject == null ? null : userObject.getUserDN());
						updateProps.put("token", token);
						if (child.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
							documentMorphiaDAO.update(child.getId(), updatePropsDoc);
						} else {
							baseMorphiaDAO.update(repositoryId, child.getId(), updatePropsDoc, typeId);
						}
					}
					if (updateProps != null) {
						LOG.debug("updateObjects: {}, baseType: {}, props: {}", child.getId(), child.getBaseId(),
								updateProps);
					}
				}
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		}

		/**
		 * Checks and compiles a property set that can be written to disc.
		 */

		private static PropertiesImpl compileWriteProperties(String repositoryId, TypeDefinition type,
				IUserObject userObject, Properties properties, IBaseObject data, String tracingId, ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisObjectService::compileWriteProperties", null);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			PropertiesImpl result = new PropertiesImpl();
			// Set<String> addedProps = new HashSet<String>();
			Map<String, PropertyData<?>> propMap = properties.getProperties();

			if (properties == null || propMap == null) {
				LOG.error("Method name: {}, unknown properties: {}, repositoryId: {}, TraceId: {}",
						"compileWriteProperties", properties, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.NO_PROPERTIES), span),
								ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new IllegalArgumentException(TracingWriter.log(String.format(ErrorMessages.NO_PROPERTIES), span));
			}

			if (type == null) {
				LOG.error("Method name: {}, unknown typeId: {}, repositoryId: {}, TraceId: {}",
						"compileWriteProperties", type, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, type), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_TYPE_ID, type), span));
			}

			List<?> secondaryObjectTypeIds = null;
			PropertyData<?> secondaryObjectType = propMap.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (secondaryObjectType != null) {
				secondaryObjectTypeIds = secondaryObjectType.getValues();
			} else {
				if (data != null && data.getSecondaryTypeIds() != null) {
					secondaryObjectTypeIds = data.getSecondaryTypeIds();
				}
			}

			PropertyDefinition<?> propTypes = null;
			// check if all required properties are there
			for (PropertyData<?> prop : propMap.values()) {
				propTypes = null;
				Map<String, PropertyDefinition<?>> property = type.getPropertyDefinitions();
				propTypes = property.get(prop.getId());
				if (propTypes == null) {
					if (secondaryObjectTypeIds != null) {
						Map<String, PropertyDefinition<?>> secondaryPropertyDefinition = new HashMap<>();
						List<? extends TypeDefinition> secondaryObject = CmisTypeServices.Impl.checkTypePermissionList(
								typePermissionFlow, repositoryId, userObject, secondaryObjectTypeIds);
						secondaryObject.stream().collect(Collectors.toList()).forEach(e -> {
							Map<String, PropertyDefinition<?>> secondaryProperty = e.getPropertyDefinitions();
							secondaryProperty.entrySet().stream().collect(Collectors.toList()).forEach(t -> {
								if (t.getValue().getId().equals(prop.getId())) {
									secondaryPropertyDefinition.put(t.getKey(), t.getValue());
								}
							});
						});
						propTypes = secondaryPropertyDefinition.get(prop.getId());
					}

				}
				if (propTypes == null) {
					LOG.error("Method name: {}, unknown propertiesTypes: {}, repositoryId: {}, TraceId: {}",
							"compileWriteProperties", prop.getId(), repositoryId,
							span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PROPERTY), span),
									ErrorMessages.ILLEGAL_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new IllegalArgumentException(
							TracingWriter.log(String.format(ErrorMessages.UNKNOWN_PROPERTY), span));
				}

				// can it be set?
				// if (propTypes.getUpdatability() == Updatability.READONLY) {
				// LOG.error("Unknown propertiesTypes:{}", prop.getId());
				// throw new CmisObjectNotFoundException("Property '" +
				// prop.getId() + "' is readonly!");
				// }

				if (!((propTypes.getId().equals(PropertyIds.OBJECT_TYPE_ID))
						|| (propTypes.getId().equals(PropertyIds.NAME))
						|| (propTypes.getId().equals(PropertyIds.OBJECT_ID)))) {
					result.addProperty(prop);
				}
			}
			// set Default Properties
			Map<String, PropertyData<?>> propMapNew = setDefaultProperties(repositoryId, type, propMap);
			if (propMapNew != propMap) { // NOSONAR
				result = new PropertiesImpl(propMapNew.values());
			}
			if (result != null) {
				LOG.debug("compileWriteProperties data count: {}", result.getPropertyList().size());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;
		}

		private static <T> T convertInstanceOfObject(Object o, Class<T> clazz) {
			try {
				return clazz.cast(o);
			} catch (ClassCastException e) {
				return null;
			}
		}

		public static Set<String> splitFilter(String filter) {
			if (filter == null) {
				return null;
			}

			if (filter.trim().length() == 0) {
				return null;
			}

			Set<String> result = new HashSet<String>();
			for (String s : filter.split(",")) {
				s = s.trim();
				if (s.equals("*")) {
					return null;
				} else if (s.length() > 0) {
					result.add(s);
				}
			}

			// set a few base properties
			// query name == id (for base type properties)
			result.add(PropertyIds.OBJECT_ID);
			result.add(PropertyIds.OBJECT_TYPE_ID);
			result.add(PropertyIds.BASE_TYPE_ID);
			if (result != null && filter != null) {
				LOG.debug("after splitFilter: {}, {}", filter, result);
			}
			return result;
		}

		/**
		 * Gets the type id from a set of properties.
		 */
		public static String getObjectTypeId(Properties properties, String repositoryId, String tracingId, ISpan span) {

			if (properties == null || properties.getProperties() == null) {
				LOG.error("getObjectTypeId unknown properties: {}, repositoryId: {}, TraceId: {}", properties,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.PROPERTIES_MUST_BE_SET), span),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.PROPERTIES_MUST_BE_SET), span));
			}

			PropertyData<?> typeProperty = properties.getProperties().get(PropertyIds.OBJECT_TYPE_ID);
			if (typeProperty != null) {
				LOG.debug("getObjectTypeId for: {}, properties: {}", PropertyIds.OBJECT_TYPE_ID, typeProperty);
			}

			if (!(typeProperty instanceof PropertyId)) {
				LOG.error("Type Id must be set!");
				throw new CmisInvalidArgumentException("Type Id must be set!");
			}

			String typeId = ((PropertyId) typeProperty).getFirstValue();
			if (typeId == null) {
				LOG.error("Type Id must be set!");
				throw new CmisInvalidArgumentException("Type Id must be set!");
			}
			return typeId;
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

			List<RenditionData> renditions = getRenditions(repositoryId, so, null, null, BigInteger.ZERO,
					BigInteger.ZERO, null, so.getTypeId(), null, null);
			if (renditions == null || renditions.size() == 0) {
				objInfo.setRenditionInfos(null);
			} else {
				List<RenditionInfo> infos = new ArrayList<RenditionInfo>();
				for (RenditionData rendition : renditions) {
					RenditionInfoImpl info = new RenditionInfoImpl();
					info.setKind(rendition.getKind());
					info.setId(rendition.getStreamId());
					info.setContentType(rendition.getMimeType());
					info.setLength(rendition.getBigLength());
					info.setTitle(rendition.getTitle());
					infos.add(info);
				}
				objInfo.setRenditionInfos(infos);
			}

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

		public static boolean getAclAccess(String repositoryId, IBaseObject data, IUserObject userObject) {
			boolean acessPermission = false;
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			if (data != null && !data.getName().equals("@ROOT@")) {
				LOG.debug("getAclAccess for object: {}, repository: {}", data.getId().toString(), repositoryId);
				String systemAdmin = System.getenv("SYSTEM_ADMIN");
				boolean aclPropagation = Stream.of(userObject.getGroups())
						.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
				if (data.getAcl().getAclPropagation()
						.equalsIgnoreCase(AclPropagation.REPOSITORYDETERMINED.toString())) {
					acessPermission = true;
				} else {

					String[] queryResult = data.getInternalPath().split(",");
					List<IBaseObject> folderChildren = Stream.of(queryResult).filter(t -> !t.isEmpty())
							.map(t -> DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, t, null,
									data.getTypeId()))
							.collect(Collectors.<IBaseObject>toList());
					List<AccessControlListImplExt> mAcl = null;
					if (folderChildren.size() == 1) {
						mAcl = new ArrayList<>();
						mAcl.add(data.getAcl());
					} else {
						mAcl = folderChildren.stream().filter(t -> t != null && t.getAcl() != null).map(t -> t.getAcl())
								.collect(Collectors.<AccessControlListImplExt>toList());
					}

					String[] getPrincipalIds = Helpers.getPrincipalIds(userObject);

					List<List<Ace>> parentAce = mAcl.stream().filter(t -> t.getAces() != null && t.getAces().size() > 0)
							.map(t -> t.getAces()).collect(Collectors.toList());
					parentAce.add(data.getAcl().getAces());

					// for (MAclImpl acl : mAcl) {
					// List<Ace> listAce = acl.getAces().stream().filter(t ->
					// Arrays.stream(getPrincipalIds).parallel()
					// .anyMatch(t.getPrincipalId()::contains) ==
					// true).collect(Collectors.toList());
					// if (listAce.size() > 1 || listAce.size() == 1) {
					// acessPermission = true;
					// break;
					// }
					// }
					// if (!acessPermission) {
					// List<Ace> dataAce =
					// data.getAcl().getAces().stream().filter(t
					// -> Arrays.stream(getPrincipalIds)
					// .parallel().anyMatch(t.getPrincipalId()::contains) ==
					// true).collect(Collectors.toList());
					// if (dataAce.size() > 1 || dataAce.size() == 1) {
					// acessPermission = true;
					// }
					// }
					for (List<Ace> ace : parentAce) {
						List<Ace> listAce = ace.stream()
								.filter(t -> Arrays.stream(getPrincipalIds).parallel().anyMatch(
										x -> Objects.equals(x.toLowerCase(), t.getPrincipalId().toLowerCase())) == true)
								.collect(Collectors.toList());
						if (listAce.size() >= 1) {
							acessPermission = true;
							break;
						}
					}

				}
			}
			return acessPermission;
		}

		private static void validateAcl(String repositoryId, Acl removeAces, String objectId, IBaseObject object) {
			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, null, false, objectId, null,
					object.getTypeId());
			LOG.info("RemoveAcl: {}, for object: {}", removeAces, objectId);
			if (data.getAcl() != null) {
				if (removeAces != null) {
					for (Ace ace : removeAces.getAces()) {
						List<Ace> aces = data.getAcl().getAces();
						for (int i = 0; i < aces.size(); i++) {
							if (aces.get(i).toString().equals(ace.toString())) {
								aces.remove(i);
							}
						}
					}
				}
				Long modifiedTime = System.currentTimeMillis();
				TokenImpl token = new TokenImpl(TokenChangeType.SECURITY, modifiedTime);
				DBUtils.BaseDAO.updateAcl(repositoryId, data.getAcl(), token, objectId, modifiedTime,
						object.getTypeId());
				if (data != null) {
					LOG.debug("updatedAcl: {}, for object: {}", data.getAcl(), objectId);
				}
			}
		}

		private static List<? extends IBaseObject> getDescendants(String repository, String path, String dataPath,
				AccessControlListImplExt dataAcl, MNavigationServiceDAO navigationMorphiaDAO, IUserObject userObject,
				String typeId) {
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String[] queryResult = dataPath.split(",");
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			List<IBaseObject> folderChildren = Stream.of(queryResult).filter(t -> !t.isEmpty())
					.map(t -> DBUtils.BaseDAO.getByObjectId(repository, principalIds, aclPropagation, t, null, typeId))
					.collect(Collectors.<IBaseObject>toList());

			List<AccessControlListImplExt> mAcl = null;
			if (folderChildren.size() == 1) {
				mAcl = new ArrayList<>();
				mAcl.add(dataAcl);
			} else {
				mAcl = folderChildren.stream().filter(t -> t.getAcl() != null).map(t -> t.getAcl())
						.collect(Collectors.<AccessControlListImplExt>toList());
			}

			List<? extends IBaseObject> children = new ArrayList<>();

			boolean objectOnly = true;
			for (AccessControlListImplExt acl : mAcl) {
				if (acl.getAclPropagation().equalsIgnoreCase("REPOSITORYDETERMINED")
						|| acl.getAclPropagation().equalsIgnoreCase("PROPAGATE")) {
					List<Ace> listAce = acl.getAces().stream()
							.filter(t -> Arrays.stream(principalIds).parallel().anyMatch(
									x -> Objects.equals(x.toLowerCase(), t.getPrincipalId().toLowerCase())) == true)
							.collect(Collectors.toList());
					if (listAce.size() >= 1) {
						children = navigationMorphiaDAO.getDescendants(path, principalIds, false, null, null, null);
						objectOnly = false;
						break;
					}
				}
			}
			// Acl Propagation ObjectOnly
			if (objectOnly) {
				children = navigationMorphiaDAO.getDescendants(path, principalIds, aclPropagation, null, null, null);
			}

			LOG.debug("Descentdants for path: {}, {}", path, children);

			return children;

		}

		private static List<? extends IDocumentObject> getChildrens(String repositoryId, String path, String dataPath,
				AccessControlListImplExt dataAcl, MNavigationDocServiceDAO navigationMorphiaDAO, IUserObject userObject,
				String typeId) {
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			String[] queryResult = dataPath.split(",");
			List<IBaseObject> folderChildren = Stream.of(queryResult).filter(t -> !t.isEmpty()).map(
					t -> DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, t, null, typeId))
					.collect(Collectors.<IBaseObject>toList());

			List<AccessControlListImplExt> mAcl = null;
			if (folderChildren.size() == 1) {
				mAcl = new ArrayList<>();
				mAcl.add(dataAcl);
			} else {
				mAcl = folderChildren.stream().filter(t -> t.getAcl() != null).map(t -> t.getAcl())
						.collect(Collectors.<AccessControlListImplExt>toList());
			}

			List<? extends IDocumentObject> children = new ArrayList<>();
			boolean objectOnly = true;
			for (AccessControlListImplExt acl : mAcl) {
				if (acl.getAclPropagation().equalsIgnoreCase("REPOSITORYDETERMINED")
						|| acl.getAclPropagation().equalsIgnoreCase("PROPAGATE")) {
					List<Ace> listAce = acl.getAces().stream()
							.filter(t -> Arrays.stream(principalIds).parallel().anyMatch(
									x -> Objects.equals(x.toLowerCase(), t.getPrincipalId().toLowerCase())) == true)
							.collect(Collectors.toList());
					if (listAce.size() >= 1) {
						children = navigationMorphiaDAO.getChildren(path, principalIds, false, -1, -1, null, null, null,
								null, repositoryId, typeId);
						objectOnly = false;
						break;
					}
				}
			}

			// Acl Propagation ObjectOnly
			if (objectOnly) {
				children = navigationMorphiaDAO.getChildren(path, principalIds, aclPropagation, -1, -1, null, null,
						null, null, repositoryId, typeId);

			}

			LOG.debug("Children for path: {}, {}", path, children);

			return children;
		}

		public static String htmlEncode(String s) {
			StringBuilder out = new StringBuilder();
			for (int i = 0; i < s.length(); i++) {
				char c = s.charAt(i);
				out.append(c > 0x7f || c == '"' || c == '&' || c == '<' || c == '>' ? "&#" + (int) c + ";" : c);
			}
			return out.toString();
		}

		private static Object checkPrimaryKeyType(String value, PropertyDefinition<?> propDef) {
			if (propDef == null) {
				return null;
			}
			if (value != null && !value.isEmpty()) {
				if (propDef.getPropertyType().equals(PropertyType.DATETIME)) {
					return Long.parseLong(value);
				} else if (propDef.getPropertyType().equals(PropertyType.DECIMAL)) {
					return new BigDecimal(value);
				} else if (propDef.getPropertyType().equals(PropertyType.INTEGER)) {
					return Integer.parseInt(value);
				} else {
					return value;
				}
			}
			return null;
		}

		private static void invokeObjectFlowServiceAfterCreate(IBaseObject doc, ObjectFlowType invokeMethod,
				Map<String, Object> updatedValues, IUserObject userObject, Map<String, String> headers, String repositoryId) {
			Map<String, IObjectFlowFactory> objectFlowFactoryMap = ObjectFlowFactory.getObjectFlowFactoryMap();
			if (objectFlowFactoryMap != null && objectFlowFactoryMap.size() > 0) {
				objectFlowFactoryMap.forEach((key, objectFlowFactory) -> {
					IObjectFlowService objectFlowService = ObjectFlowFactory.createObjectFlowService(key,
							doc.getRepositoryId());
					if (objectFlowService != null) {
						LOG.info("invokeObjectFlowServiceAfterCreate for objectId: {}, InvokeMethod: {}",
								doc != null ? doc.getId() : null, invokeMethod);
						if (ObjectFlowType.CREATED.equals(invokeMethod)) {
							objectFlowService.afterCreation(doc, userObject, headers, repositoryId);
						} else if (ObjectFlowType.UPDATED.equals(invokeMethod)) {
							objectFlowService.afterUpdate(doc, updatedValues, userObject, headers, repositoryId);
						} else if (ObjectFlowType.DELETED.equals(invokeMethod)) {
							objectFlowService.afterDeletion(doc, userObject, headers, repositoryId);
						}
					}
				});
			}
		}

		private static Map<String, List<String>> getSourceProperties(Map<String, List<String>> props, String key,
				String value) {
			List<String> list = new ArrayList<String>();
			list.add(value);
			props.put(key, list);
			return props;
		}

		private static void invokeObjectFlowServiceBeforeCreate(String repositoryId, String objectId,
				Properties properties, List<String> policies, Acl addAces, Acl removeAces, IUserObject userObject,
				Boolean allVers, ObjectFlowType invokeMethod) {
			Map<String, IObjectFlowFactory> objectFlowFactoryMap = ObjectFlowFactory.getObjectFlowFactoryMap();
			if (objectFlowFactoryMap != null && objectFlowFactoryMap.size() > 0) {
				objectFlowFactoryMap.forEach((key, objectFlowFactory) -> {
					IObjectFlowService objectFlowService = ObjectFlowFactory.createObjectFlowService(key, repositoryId);
					if (objectFlowService != null) {
						try {
							LOG.info(
									"invokeObjectFlowServiceBeforeCreate for objectId: {}, InvokeMethod: {}" + objectId,
									invokeMethod);
							boolean resultFlow = false;
							if (ObjectFlowType.CREATED.equals(invokeMethod)) {
								resultFlow = objectFlowService.beforeCreation(repositoryId, objectId, properties,
										policies, addAces, removeAces, userObject);
							} else if (ObjectFlowType.UPDATED.equals(invokeMethod)) {
								resultFlow = objectFlowService.beforeUpdate(repositoryId, objectId, properties, addAces,
										userObject);
							} else if (ObjectFlowType.DELETED.equals(invokeMethod)) {
								resultFlow = objectFlowService.beforeDeletion(repositoryId, objectId, allVers,
										userObject);
							}
							if (!resultFlow) {
								LOG.error("Operation failed with ObjectFlowService for InvokeMethod: {}", invokeMethod);
								throw new IllegalArgumentException(
										"Operation failed with ObjectFlowService for InvokeMethod: " + invokeMethod);
							}
						} catch (Exception ex) {
							LOG.error(
									"Operation failed with ObjectFlowService for InvokeMethod: {}, with exception: {}",
									invokeMethod, ex.getMessage());
							throw new IllegalArgumentException(ex.getMessage());
						}
					}
				});

			}
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

		private static void addRootFolder(String repositoryId) {
			Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
			IStorageService localService = StorageServiceFactory.createStorageService(parameters);
			if (parameters.get("root") != null) {
				try {
					if (localService != null) {
						LOG.debug("localService calling createFolder: {}, {}, repositoryId: {}",
								localService.getClass().getName() != null ? localService.getClass().getName() : null,
								parameters.get("root"), repositoryId);
					}

					localService.createFolder(parameters.get("root"), parameters.get("root"),
							"/" + parameters.get("root"));
				} catch (IOException e) {
					LOG.error("Folder creation exception: {}, repositoryId: {}", e.getMessage(), repositoryId);
					throw new IllegalArgumentException(e.getMessage());
				}
			}
		}
	}

	public static int getUpdateFolderPathId(String[] internalPath, String id) {
		for (int i = 1; i < internalPath.length; i++) {
			if (internalPath[i].equals(id)) {
				return i - 1;
			}
		}
		return 0;
	}
}
