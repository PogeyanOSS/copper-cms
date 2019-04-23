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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.ExtensionsData;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUpdateConflictException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.common.TypePermissionType;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.api.storage.IStorageService;
import com.pogeyan.cmis.api.uri.exception.CmisRoleValidationException;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MimeUtils;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.api.utils.TracingWriter;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.impl.factory.StorageServiceFactory;
import com.pogeyan.cmis.impl.factory.TypeServiceFactory;
import com.pogeyan.cmis.impl.utils.CmisPropertyConverter;
import com.pogeyan.cmis.impl.utils.DBUtils;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class CmisVersioningServices {
	private static final Logger LOG = LoggerFactory.getLogger(CmisVersioningServices.class);

	public static class Impl {

		public static List<ObjectData> getAllVersions(String repositoryId, String objectId, String versionSeriesId,
				String filter, Boolean includeAllowableActions, ExtensionsData extension, ObjectInfoHandler objectInfos,
				IUserObject userObject, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException, CmisUpdateConflictException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisVersioningServices::getAllVersions", null);
			IDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId, null);
			List<ObjectData> objectData = new ArrayList<ObjectData>();
			if (data == null) {
				LOG.error("Method name: {}, unknown object Id:{}, repositoryid: {}, TraceId: {}", "getAllVersions",
						objectId, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
			}
			String versionReferenceId = data.getVersionReferenceId();

			// if (data.getIsPrivateWorkingCopy()) {
			// LOG.error("objectId:{},is not a PWC document ", objectId);
			// throw new CmisUpdateConflictException(objectId + " is PWC not a
			// document");
			// }

			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			List<? extends IDocumentObject> documentObject = DBUtils.DocumentDAO.getAllVersion(repositoryId,
					versionReferenceId);
			Collections.reverse(documentObject);
			for (IDocumentObject documentData : documentObject) {
				ObjectData object = CmisObjectService.Impl.compileObjectData(documentData.getRepositoryId(),
						documentData, filterCollection, includeAllowableActions, false, true, null, null, null,
						userObject, tracingId, span);
				objectData.add(object);
			}
			if (objectData != null) {
				LOG.debug("getAllVersions result data count:{}, for this id: {}", objectData.size(), objectId);
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return objectData;
		}

		public static ObjectData getObjectOfLatestVersion(String repositoryId, String objectId, String versionSeriesId,
				boolean major, String filter, Boolean includeAllowableActions, String renditionFilter,
				Boolean includePolicyIds, Boolean includeAcl, ExtensionsData extension, ObjectInfoHandler objectInfos,
				IUserObject userObject, String tracingId, ISpan parentSpan) throws CmisObjectNotFoundException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisVersioningServices::getObjectOfLatestVersion", null);
			IDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId, null);
			if (data == null) {
				LOG.error("Method name: {}, unknown object Id:{}, repositoryid: {}, TraceId: {}", "getAllVersions",
						objectId, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
			}
			String versionReferenceId = data.getVersionReferenceId();
			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			IDocumentObject docObj = DBUtils.DocumentDAO.getLatestVersion(repositoryId, versionReferenceId, major);
			if (docObj == null) {
				LOG.error(
						"getObjectOfLatestVersion error while getting latest version: {}, repositoryid: {}, TraceId: {}",
						objectId, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.ERROR_IN_VERSION, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.ERROR_IN_VERSION, objectId), span));
			}
			ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, docObj, filterCollection,
					includeAllowableActions, false, true, objectInfos, null, null, userObject, tracingId, span);

			LOG.debug("getObjectOfLatestVersion result data count: {}", objectData);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return objectData;
		}

		public static Properties getPropertiesOfLatestVersion(String repositoryId, String objectId,
				String versionSeriesId, Boolean major, String filter, ExtensionsData extension, IUserObject userObject,
				String tracingId, ISpan parentSpan) throws CmisObjectNotFoundException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisVersioningServices::getPropertiesOfLatestVersion", null);
			IBaseObject latestVersionDocument = null;
			IDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId, null);
			if (data == null) {
				LOG.error("Method name: {}, unknown object Id: {}, repositoryid: {}, TraceId: {}",
						"getPropertiesOfLatestVersion", objectId, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
			}

			String versionReferenceId = data.getVersionReferenceId();
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			IDocumentObject docObj = DBUtils.DocumentDAO.getLatestVersion(repositoryId, versionReferenceId, major);
			latestVersionDocument = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, true, docObj.getId(),
					null, data.getTypeId());
			ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, latestVersionDocument,
					filterCollection, true, false, true, null, null, null, userObject, tracingId, span);
			if (objectData != null) {
				LOG.debug("Properties of latest version of object: {}", objectData.getProperties());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return objectData.getProperties();
		}

		public static String checkOut(String repositoryId, Holder<String> objectId, ExtensionsData extension,
				Holder<Boolean> contentCopied, IUserObject userObject, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException, CmisUpdateConflictException, CmisNotSupportedException,
				CmisInvalidArgumentException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisVersioningServices::checkOut", null);
			MDocumentObjectDAO documentObjectDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			IDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId.getValue(), null);
			if (data == null) {
				LOG.error("Method name: {}, unknown object Id: {}, repositoryid: {}, TraceId: {}", "checkOut", objectId,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					data.getTypeId(), TypePermissionType.CREATE);
			if (permission) {
				if (data.getIsVersionSeriesCheckedOut()) {
					LOG.error("checkOut document: {} is already checked out, repositoryid: {}, TraceId: {}",
							objectId.getValue(), repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.ALREADY_CHECKEDOUT), span),
									ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisUpdateConflictException(
							TracingWriter.log(String.format(ErrorMessages.ALREADY_CHECKEDOUT), span));
				}

				if (data.getIsLatestVersion() == false) {
					LOG.error("checkOut only latest version can able to check out in repositoryid: {}, TraceId: {}",
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.CANNOT_CHECKOUT), span),
									ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisUpdateConflictException(
							TracingWriter.log(String.format(ErrorMessages.CANNOT_CHECKOUT), span));
				}

				TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, data.getTypeId(), null,
						userObject, tracingId, span);

				if (!typeDef.getBaseTypeId().equals(BaseTypeId.CMIS_DOCUMENT)) {
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.DOCUMENTS_CHECKOUT), span),
									ErrorMessages.NOT_SUPPORTED_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisNotSupportedException(
							TracingWriter.log(String.format(ErrorMessages.DOCUMENTS_CHECKOUT), span));
				}

				/*
				 * AllowableActionsImpl allowableActions = new
				 * AllowableActionsImpl(); allowableActions
				 * =(AllowableActionsImpl)allowableActions.getAllowableActions()
				 * ; if (((Set<Action>)allowableActions).contains(Action.
				 * CAN_CHECK_OUT)) { LOG.info("Versionable"); } else { throw new
				 * CmisNotSupportedException(
				 * "only versionable document can be checked out"); }
				 */

				MBaseObjectDAO baseObjectDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MBaseObjectDAO.class);
				TokenImpl token = new TokenImpl(TokenChangeType.UPDATED, System.currentTimeMillis());
				IBaseObject baseObject = baseObjectDAO.createObjectFacade(data.getName() + "-pwc",
						BaseTypeId.CMIS_DOCUMENT, data.getTypeId(), repositoryId, data.getSecondaryTypeIds(), "",
						userObject.getUserDN(), userObject.getUserDN(), token, data.getInternalPath(),
						data.getProperties(), data.getPolicies(), data.getAcl(), data.getPath(), data.getParentId());
				IDocumentObject documentObject = documentObjectDAO.createObjectFacade(baseObject, false, false, false,
						false, true, data.getVersionLabel(), data.getVersionSeriesId(), data.getVersionReferenceId(),
						true, userObject.getUserDN(), baseObject.getId(), "Commit Document",
						data.getContentStreamLength(), data.getContentStreamMimeType(), data.getContentStreamFileName(),
						null, objectId.getValue());
				documentObjectDAO.commit(documentObject);
				Map<String, Object> updateProps = new HashMap<String, Object>();
				updateProps.put("isVersionSeriesCheckedOut", true);
				updateProps.put("versionSeriesCheckedOutId", documentObject.getId().toString());
				updateProps.put("versionSeriesCheckedOutBy", userObject.getUserDN());
				documentObjectDAO.update(objectId.getValue(), updateProps);
				LOG.info("Successfully checkout PWC for this document : {}",
						documentObject != null ? documentObject.getId() : null);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return documentObject.getId();
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

		public static String cancelCheckOut(String repositoryId, String objectId, ExtensionsData extension,
				IUserObject userObject, String tracingId, ISpan parentSpan)
				throws CmisUpdateConflictException, CmisUpdateConflictException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisVersioningServices::cancelCheckOut", null);
			MDocumentObjectDAO documentMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);

			IDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId, null);
			if (data == null) {
				LOG.error("Method name: {}, unknown pwc object Id: {}, repositoryid: {}, TraceId: {}", "cancelCheckOut",
						objectId, repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					data.getTypeId(), TypePermissionType.CREATE);
			if (permission) {
				if (data.getIsPrivateWorkingCopy() == false) {
					LOG.error("cancelCheckOut pwc: {}, is not private working copy in repositoryid: {}, TraceId: {}",
							objectId, repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.OBJECTID_NOT_PRIVATE_COPY), span),
									ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisUpdateConflictException(
							TracingWriter.log(String.format(ErrorMessages.OBJECTID_NOT_PRIVATE_COPY), span));
				}

				IDocumentObject document = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
						data.getPreviousVersionObjectId(), null);
				TokenImpl deleteToken = new TokenImpl(TokenChangeType.DELETED, System.currentTimeMillis());
				documentMorphiaDAO.delete(objectId, null, false, false, deleteToken);
				Map<String, Object> updateProps = new HashMap<String, Object>();
				updateProps.put("isVersionSeriesCheckedOut", false);
				documentMorphiaDAO.update(document.getId(), updateProps);
				List<String> removeFields = new ArrayList<>();
				removeFields.add("versionSeriesCheckedOutBy");
				removeFields.add("versionSeriesCheckedOutId");
				TokenImpl updateToken = new TokenImpl(TokenChangeType.UPDATED, System.currentTimeMillis());
				documentMorphiaDAO.delete(document.getId(), removeFields, false, true, updateToken);
				LOG.info("Cancel checkout for this document :{} done", document != null ? document.getId() : null);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return document.getId();
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

		public static String checkIn(String repositoryId, Map<String, List<String>> listProperties,
				ContentStream contentStreamParam, Holder<String> objectId, Boolean majorParam, String checkinComment,
				ObjectInfoHandler objectInfos, String userName, IUserObject userObject, String tracingId,
				ISpan parentSpan) throws CmisObjectNotFoundException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisVersioningServices::checkIn", null);
			MDocumentObjectDAO documentObjectDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			IDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, objectId.getValue(), null);
			if (data == null) {
				LOG.error("Method name: {}, unknown object Id :{}, repositoryid: {}, TraceId: {}", "checkIn", objectId,
						repositoryId, span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = CmisTypeServices.checkCrudPermission(typePermissionFlow, repositoryId, userObject,
					data.getTypeId(), TypePermissionType.CREATE);
			if (permission) {
				if (data.getVersionSeriesId() == null) {
					LOG.error("checkIn document is not versionable: {}, repositoryid: {}, TraceId: {}", objectId,
							repositoryId, span != null ? span.getTraceId() : null);
					TracingApiServiceFactory.getApiService().updateSpan(span,
							TracingErrorMessage.message(
									TracingWriter.log(String.format(ErrorMessages.DOCUMENT_NOT_VERSIONABLE), span),
									ErrorMessages.UPDATE_EXCEPTION, repositoryId, true));
					TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					throw new CmisUpdateConflictException(
							TracingWriter.log(String.format(ErrorMessages.DOCUMENT_NOT_VERSIONABLE), span));
				}
				Map<String, Object> properties = new HashMap<String, Object>();
				if (listProperties != null) {
					for (Map.Entry<String, List<String>> entry : listProperties.entrySet()) {
						if (entry.getValue() == null || StringUtils.isBlank(entry.getValue().get(0))) {
							continue;
						} else {
							properties.put(entry.getKey(), entry.getValue());
						}
					}
				}

				IDocumentObject documentObject = null;
				checkinComment = StringUtils.isBlank(checkinComment) ? "CheckIn Document" : checkinComment;
				if (data.getProperties() != null) {
					properties.putAll(data.getProperties());
					properties.remove(PropertyIds.VERSION_LABEL);
					properties.replace(PropertyIds.LAST_MODIFIED_BY, userName);
				}

				IDocumentObject documentdata = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
						data.getPreviousVersionObjectId(), null);
				TokenImpl token = new TokenImpl(TokenChangeType.CREATED, System.currentTimeMillis());
				MBaseObjectDAO baseObjectDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MBaseObjectDAO.class);
				IBaseObject baseObject = baseObjectDAO.createObjectFacade(documentdata.getName(),
						BaseTypeId.CMIS_DOCUMENT, documentdata.getTypeId(), repositoryId,
						documentdata.getSecondaryTypeIds(), "", documentdata.getCreatedBy(), userName, token,
						data.getInternalPath(), properties, documentdata.getPolicies(), documentdata.getAcl(),
						data.getPath(), data.getParentId());
				String versionSeriesId = Helpers.getObjectId();
				if (data.getIsVersionSeriesCheckedOut()) {
					if (data.getIsPrivateWorkingCopy()) {
						if (majorParam) {
							String versionLable = data.getVersionLabel();
							String[] versionLabelArray = versionLable.split("\\.");
							Double versionLabelMajor = Double.parseDouble(versionLabelArray[0]);
							versionLabelMajor = versionLabelMajor + 1;
							if (contentStreamParam == null) {
								documentObject = documentObjectDAO.createObjectFacade(baseObject, false, true, true,
										true, false, String.valueOf(versionLabelMajor), versionSeriesId.toString(),
										data.getVersionReferenceId(), false, null, null, checkinComment,
										data.getContentStreamLength(), data.getContentStreamMimeType(),
										data.getContentStreamFileName(), data.getContentStreamId(),
										data.getPreviousVersionObjectId());
							} else {
								documentObject = documentObjectDAO.createObjectFacade(baseObject, false, true, true,
										true, false, String.valueOf(versionLabelMajor), versionSeriesId.toString(),
										data.getVersionReferenceId(), false, null, null, checkinComment,
										contentStreamParam.getLength(), contentStreamParam.getMimeType(),
										contentStreamParam.getFileName(), data.getContentStreamId(),
										data.getPreviousVersionObjectId());
							}

						}

						else {
							String versionLabel = data.getVersionLabel();
							String[] versionLabelArray = versionLabel.split("\\.");
							Integer versionLabelMinor = Integer.parseInt(versionLabelArray[1]);
							versionLabelMinor = versionLabelMinor + 2;
							String verLabelMinorString = String.valueOf(versionLabelMinor);
							verLabelMinorString = versionLabelArray[0] + "." + versionLabelMinor;
							if (contentStreamParam == null) {
								documentObject = documentObjectDAO.createObjectFacade(baseObject, false, true, false,
										false, false, String.valueOf(verLabelMinorString), versionSeriesId.toString(),
										data.getVersionReferenceId(), false, null, null, checkinComment,
										data.getContentStreamLength(), data.getContentStreamMimeType(),
										data.getContentStreamFileName(), data.getContentStreamId(),
										data.getPreviousVersionObjectId());
							} else {
								documentObject = documentObjectDAO.createObjectFacade(baseObject, false, true, false,
										false, false, verLabelMinorString, versionSeriesId.toString(),
										data.getVersionReferenceId(), false, null, null, checkinComment,
										contentStreamParam.getLength(), contentStreamParam.getMimeType(),
										contentStreamParam.getFileName(), data.getContentStreamId(),
										data.getPreviousVersionObjectId());
							}
						}

						documentObjectDAO.commit(documentObject);
						Properties updateProperties = CmisPropertyConverter.Impl.createUpdateProperties(listProperties,
								data.getTypeId(), null, Collections.singletonList(objectId.toString()), repositoryId,
								data, userObject);
						if (updateProperties != null) {
							CmisObjectService.Impl.updateProperties(repositoryId,
									new Holder<String>(documentObject.getId()), null, updateProperties, null, null,
									userObject, data.getTypeId(), tracingId, span);
						}
						LOG.debug("checked in object: {}", documentObject != null ? documentObject.getId() : null);
					}
				}
				if (contentStreamParam != null && contentStreamParam.getStream() != null) {
					String fileName;
					if (documentObject.getContentStreamFileName().contains(".")) {
						String[] fileNames = documentObject.getContentStreamFileName().split("\\.(?=[^\\.]+$)");
						String type = MimeUtils.checkFileExtension(fileNames[1]);
						if (type != null) {
							fileName = fileNames[0] + documentObject.getVersionLabel().replace(".", "_") + "."
									+ fileNames[1];
						} else {
							fileName = documentObject.getContentStreamFileName()
									+ documentObject.getVersionLabel().replace(".", "_");
						}

					} else {
						fileName = documentObject.getContentStreamFileName()
								+ documentObject.getVersionLabel().replace(".", "_");
					}
					Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
					IStorageService localService = StorageServiceFactory.createStorageService(parameters);
					Map<String, Object> updatecontentProps = new HashMap<String, Object>();
					try {
						ContentStream versionCustomContentStream = new ContentStreamImpl(fileName,
								contentStreamParam.getBigLength(), documentObject.getContentStreamMimeType(),
								contentStreamParam.getStream());
						localService.writeContent(documentObject.getId().toString(), fileName, documentObject.getPath(),
								versionCustomContentStream);
					} catch (Exception e) {
						LOG.error("checkIn file creation exception:  {}, repositoryid: {}, TraceId: {}", e,
								repositoryId, span != null ? span.getTraceId() : null);
						TracingApiServiceFactory.getApiService().updateSpan(span,
								TracingErrorMessage.message(
										TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e), span),
										ErrorMessages.BASE_EXCEPTION, repositoryId, true));
						TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
					}
					updatecontentProps.put("contentStreamLength", contentStreamParam.getLength());
					updatecontentProps.put("contentStreamFileName", fileName);
					documentObjectDAO.update(documentObject.getId(), updatecontentProps);

				}

				Map<String, Object> updateProps = new HashMap<String, Object>();
				updateProps.put("isLatestVersion", false);
				updateProps.put("isLatestMajorVersion", false);
				updateProps.put("isVersionSeriesCheckedOut", false);
				updateProps.put("versionSeriesCheckedOutId", "");
				updateProps.put("versionSeriesCheckedOutBy", "");
				documentObjectDAO.update(documentdata.getId(), updateProps);
				TokenImpl deleteToken = new TokenImpl(TokenChangeType.DELETED, System.currentTimeMillis());
				documentObjectDAO.delete(objectId.getValue(), null, true, false, deleteToken);
				LOG.info("checkIn PWC done for: {}", objectId);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
				return documentObject.getId();
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
	}
}
