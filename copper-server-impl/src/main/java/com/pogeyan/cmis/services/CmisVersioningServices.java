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
package com.pogeyan.cmis.services;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.apache.commons.lang3.StringUtils;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.DBUtils;
import com.pogeyan.cmis.DatabaseManager;
import com.pogeyan.cmis.RepositoryManager;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.storage.IStorageService;
import com.pogeyan.cmis.data.objects.MBaseObject;
import com.pogeyan.cmis.data.objects.MDocumentObject;
import com.pogeyan.cmis.data.objects.MToken;
import com.pogeyan.cmis.document.storage.StorageDocumentServiceFactory;

public class CmisVersioningServices {
	private static final Logger LOG = LoggerFactory.getLogger(CmisVersioningServices.class);

	public static class Impl {

		public static List<ObjectData> getAllVersions(String repositoryId, String objectId, String versionSeriesId,
				String filter, Boolean includeAllowableActions, ExtensionsData extension, ObjectInfoHandler objectInfos,
				String userName) throws CmisObjectNotFoundException, CmisUpdateConflictException {
			LOG.info("getAllVersions on objectId: {} , repository: {}", objectId, repositoryId);
			MDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, new ObjectId(objectId),
					null);
			List<ObjectData> objectData = new ArrayList<ObjectData>();
			if (data == null) {
				throw new CmisObjectNotFoundException("unknown object Id" + objectId);
			}
			String versionReferenceId = data.getVersionReferenceId();
			// if (data.getIsPrivateWorkingCopy()) {
			// LOG.error("objectId:{},is not a PWC document ", objectId);
			// throw new CmisUpdateConflictException(objectId + " is PWC not a
			// document");
			// }

			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			List<MDocumentObject> documentObject = DBUtils.DocumentDAO.getAllVersion(repositoryId, versionReferenceId);
			Collections.reverse(documentObject);
			for (MDocumentObject documentData : documentObject) {
				ObjectData object = CmisObjectService.Impl.compileObjectData(documentData.getRepositoryId(),
						documentData, filterCollection, includeAllowableActions, false, true, null, null, null,
						userName);
				objectData.add(object);
			}
			if (LOG.isDebugEnabled()) {
				LOG.debug("ObjectData all versions on object: {} , ObjectData: {}", objectId, objectData);
			}
			return objectData;
		}

		public static ObjectData getObjectOfLatestVersion(String repositoryId, String objectId, String versionSeriesId,
				boolean major, String filter, Boolean includeAllowableActions, Object object, String renditionFilter,
				Boolean includePolicyIds, Boolean includeAcl, ExtensionsData extension, ObjectInfoHandler objectInfos,
				String userName) throws CmisObjectNotFoundException {
			LOG.info("getObjectOfLatestVersion on objectId: {} , repository: {}", objectId, repositoryId);
			MDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, new ObjectId(objectId),
					null);
			if (data == null) {
				LOG.error("unknown object Id:{}", objectId);
				throw new CmisObjectNotFoundException("unknown object Id " + objectId);
			}
			String versionReferenceId = data.getVersionReferenceId();
			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			MDocumentObject docObj = DBUtils.DocumentDAO.getLatestVersion(repositoryId, versionReferenceId, major);
			if (docObj == null) {
				LOG.error("error while getting latest version", objectId);
				throw new CmisObjectNotFoundException("error while getting latest version " + objectId);
			}
			ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, docObj, filterCollection,
					includeAllowableActions, false, true, objectInfos, null, null, userName);
			if (LOG.isDebugEnabled() && objectData != null) {
				LOG.debug("ObjectData of latest version: {}", objectData.toString());
			}
			return objectData;
		}

		public static Properties getPropertiesOfLatestVersion(String repositoryId, String objectId,
				String versionSeriesId, Boolean major, String filter, ExtensionsData extension, String userName)
				throws CmisObjectNotFoundException {
			LOG.info("getPropertiesOfLatestVersion on objectId: {}, repository: {}", objectId, repositoryId);
			MBaseObject latestVersionDocument = null;
			MDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, new ObjectId(objectId),
					null);
			if (data == null) {
				LOG.error("unknown object Id:{}", objectId);
				throw new CmisObjectNotFoundException("unknown object Id" + objectId);
			}

			String versionReferenceId = data.getVersionReferenceId();
			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			MDocumentObject docObj = DBUtils.DocumentDAO.getLatestVersion(repositoryId, versionReferenceId, major);
			latestVersionDocument = DBUtils.BaseDAO.getByObjectId(repositoryId, docObj.getId(), null);
			ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, latestVersionDocument,
					filterCollection, true, false, true, null, null, null, userName);
			if (LOG.isDebugEnabled()) {
				LOG.debug("Properties of latest version of object: {}", objectData.getProperties());
			}
			return objectData.getProperties();
		}

		public static ObjectId checkOut(String repositoryId, Holder<String> objectId, ExtensionsData extension,
				Holder<Boolean> contentCopied, String userName) throws CmisObjectNotFoundException,
				CmisUpdateConflictException, CmisNotSupportedException, CmisInvalidArgumentException {
			LOG.info("checkOut on objectId: {} , repository: {}", objectId, repositoryId);
			MDocumentObjectDAO baseMorphiaDAO = DatabaseManager.getInstance(repositoryId).getObjectService(repositoryId,
					MDocumentObjectDAO.class);
			MDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
					new ObjectId(objectId.getValue()), null);
			if (data == null) {
				LOG.error("unknown object Id: {}", objectId);
				throw new CmisObjectNotFoundException("unknown object Id " + objectId);
			}

			if (data.getIsVersionSeriesCheckedOut()) {
				LOG.error("Document: {}, is already checked out.", objectId.getValue());
				throw new CmisUpdateConflictException("Document " + objectId.getValue() + " is already checked out.");
			}

			if (data.getIsLatestVersion() == false) {
				LOG.error("Only latest version can able to check out.");
				throw new CmisUpdateConflictException("Only latest version can able to check out.");
			}

			TypeDefinition typeDef = CmisTypeServices.Impl.getTypeDefinition(repositoryId, data.getTypeId(), null);

			if (!typeDef.getBaseTypeId().equals(BaseTypeId.CMIS_DOCUMENT)) {
				throw new CmisNotSupportedException("Only documents can be checked-out.");
			}

			/*
			 * AllowableActionsImpl allowableActions = new
			 * AllowableActionsImpl(); allowableActions
			 * =(AllowableActionsImpl)allowableActions.getAllowableActions(); if
			 * (((Set<Action>)allowableActions).contains(Action.CAN_CHECK_OUT))
			 * { LOG.info("Versionable"); } else { throw new
			 * CmisNotSupportedException(
			 * "only versionable document can be checked out"); }
			 */

			ObjectId id = new ObjectId();
			MToken token = new MToken(1, System.currentTimeMillis());
			MBaseObject baseObject = new MBaseObject(id, data.getName() + "-pwc", BaseTypeId.CMIS_DOCUMENT,
					"cmis:document", repositoryId, null, "", userName, userName, token, data.getInternalPath(),
					data.getProperties(), data.getPolicies(), data.getAcl(), data.getPath(), data.getParentId());
			MDocumentObject documentObject = new MDocumentObject(baseObject, false, false, false, false, true,
					data.getVersionLabel(), data.getVersionSeriesId(), data.getVersionReferenceId(), true, userName,
					id.toString(), "Commit Document", data.getContentStreamLength(), data.getContentStreamMimeType(),
					data.getContentStreamFileName(), null, objectId.getValue());
			baseMorphiaDAO.commit(documentObject);
			Map<String, Object> updateProps = new HashMap<String, Object>();
			updateProps.put("isVersionSeriesCheckedOut", true);
			updateProps.put("versionSeriesCheckedOutId", documentObject.getId().toString());
			updateProps.put("versionSeriesCheckedOutBy", userName);
			baseMorphiaDAO.update(new ObjectId(objectId.getValue()), updateProps);
			LOG.info("PWC for document :{}", documentObject.getId());
			return documentObject.getId();
		}

		public static ObjectId cancelCheckOut(String repositoryId, String objectId, ExtensionsData extension,
				String userName) throws CmisUpdateConflictException, CmisUpdateConflictException {
			LOG.info("Cancel checkOut on objectId: {} , repository: {}", objectId, repositoryId);
			MDocumentObjectDAO documentMorphiaDAO = DatabaseManager.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);

			MDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, new ObjectId(objectId),
					null);
			if (data == null) {
				LOG.error("unknown pwc object Id " + objectId);
				throw new CmisObjectNotFoundException("unknown pwc object Id " + objectId);
			}

			if (data.getIsPrivateWorkingCopy() == false) {
				LOG.error("pwc: {} is not private working copy.", objectId);
				throw new CmisUpdateConflictException("pwc " + objectId + " is not private working copy.");
			}

			MDocumentObject document = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
					new ObjectId(data.getPreviousVersionObjectId()), null);
			MToken deleteToken = new MToken(2, System.currentTimeMillis());
			documentMorphiaDAO.delete(new ObjectId(objectId), null, false, false, deleteToken);
			Map<String, Object> updateProps = new HashMap<String, Object>();
			updateProps.put("isVersionSeriesCheckedOut", false);
			documentMorphiaDAO.update(document.getId(), updateProps);
			List<String> removeFields = new ArrayList<>();
			removeFields.add("versionSeriesCheckedOutBy");
			removeFields.add("versionSeriesCheckedOutId");
			MToken updateToken = new MToken(1, System.currentTimeMillis());
			documentMorphiaDAO.delete(document.getId(), removeFields, false, true, updateToken);
			return document.getId();
		}

		public static ObjectId checkIn(String repositoryId, Map<String, Object> properties,
				ContentStream contentStreamParam, Holder<String> objectId, Boolean majorParam, String checkinComment,
				ObjectInfoHandler objectInfos, String userName) throws CmisObjectNotFoundException {

			LOG.info("checkIn PWC on objectId: {} , repository: {} ", objectId, repositoryId);
			MDocumentObjectDAO baseMorphiaDAO = DatabaseManager.getInstance(repositoryId).getObjectService(repositoryId,
					MDocumentObjectDAO.class);
			;
			MDocumentObject data = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
					new ObjectId(objectId.getValue()), null);
			if (data == null) {
				LOG.error("unknown object Id:{}, " + objectId);
				throw new CmisObjectNotFoundException("unknown object Id " + objectId);
			}

			if (data.getVersionSeriesId() == null) {
				LOG.error("document is not versionable", objectId);
				throw new CmisUpdateConflictException(objectId + "document is not versionable");
			}

			MDocumentObject documentObject = null;
			ObjectId id = new ObjectId();
			checkinComment = StringUtils.isBlank(checkinComment) ? "CheckIn Document" : checkinComment;
			if (data.getProperties() != null) {
				properties.putAll(data.getProperties());
				properties.remove("cmis:versionLabel");
				properties.replace("cmis:lastModifiedBy", userName);
			}

			MDocumentObject documentdata = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
					new ObjectId(data.getPreviousVersionObjectId()), null);
			MToken token = new MToken(0, System.currentTimeMillis());
			MBaseObject baseObject = new MBaseObject(id, documentdata.getName(), BaseTypeId.CMIS_DOCUMENT,
					"cmis:document", repositoryId, null, "", documentdata.getCreatedBy(), userName, token,
					data.getInternalPath(), properties, documentdata.getPolicies(), documentdata.getAcl(),
					data.getPath(), data.getParentId());
			ObjectId versionSeriesId = new ObjectId();
			if (data.getIsVersionSeriesCheckedOut()) {
				if (data.getIsPrivateWorkingCopy()) {
					if (majorParam) {
						String versionLable = data.getVersionLabel();
						String[] versionLabelArray = versionLable.split("\\.");
						Double versionLabelMajor = Double.parseDouble(versionLabelArray[0]);
						versionLabelMajor = versionLabelMajor + 1;
						if (contentStreamParam == null) {
							documentObject = new MDocumentObject(baseObject, false, true, true, true, false,
									String.valueOf(versionLabelMajor), versionSeriesId.toString(),
									data.getVersionReferenceId(), false, null, null, checkinComment,
									data.getContentStreamLength(), data.getContentStreamMimeType(),
									data.getContentStreamFileName(), data.getContentStreamId(),
									data.getPreviousVersionObjectId());
						} else {
							documentObject = new MDocumentObject(baseObject, false, true, true, true, false,
									String.valueOf(versionLabelMajor), versionSeriesId.toString(),
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
							documentObject = new MDocumentObject(baseObject, false, true, false, false, false,
									String.valueOf(verLabelMinorString), versionSeriesId.toString(),
									data.getVersionReferenceId(), false, null, null, checkinComment,
									data.getContentStreamLength(), data.getContentStreamMimeType(),
									data.getContentStreamFileName(), data.getContentStreamId(),
									data.getPreviousVersionObjectId());
						} else {
							documentObject = new MDocumentObject(baseObject, false, true, false, false, false,
									verLabelMinorString, versionSeriesId.toString(), data.getVersionReferenceId(),
									false, null, null, checkinComment, contentStreamParam.getLength(),
									contentStreamParam.getMimeType(), contentStreamParam.getFileName(),
									data.getContentStreamId(), data.getPreviousVersionObjectId());
						}
					}

					baseMorphiaDAO.commit(documentObject);
					if (LOG.isDebugEnabled()) {
						LOG.debug("checked in object: {}", documentObject.getId());
					}
				}
			}

			Map<String, String> parameters = RepositoryManager.getFileDetails(repositoryId);
			IStorageService localService = StorageDocumentServiceFactory.createStorageService(parameters);
			Map<String, Object> updatecontentProps = new HashMap<String, Object>();
			if (contentStreamParam != null && contentStreamParam.getStream() != null) {
				try {
					localService.writeContent(documentObject.getId().toString(),
							documentObject.getContentStreamFileName(), documentObject.getPath(), contentStreamParam);
				} catch (Exception e) {
					LOG.error("File creation exception:  {}", e.getMessage());
				}
				updatecontentProps.put("contentStreamLength", contentStreamParam.getLength());
				baseMorphiaDAO.update(documentObject.getId(), updatecontentProps);
			}

			Map<String, Object> updateProps = new HashMap<String, Object>();
			updateProps.put("isLatestVersion", false);
			updateProps.put("isLatestMajorVersion", false);
			updateProps.put("isVersionSeriesCheckedOut", false);
			updateProps.put("versionSeriesCheckedOutId", "");
			updateProps.put("versionSeriesCheckedOutBy", "");
			baseMorphiaDAO.update(documentdata.getId(), updateProps);
			MToken deleteToken = new MToken(2, System.currentTimeMillis());
			baseMorphiaDAO.delete(new ObjectId(objectId.getValue()), null, false, false, deleteToken);
			return documentObject.getId();
		}
	}
}
