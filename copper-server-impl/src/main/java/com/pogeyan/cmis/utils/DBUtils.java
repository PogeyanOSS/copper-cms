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
package com.pogeyan.cmis.utils;

import java.util.HashMap;
import java.util.List;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.bson.types.ObjectId;

import com.pogeyan.cmis.api.core.CopperCmsRepository;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.data.objects.MBaseObject;
import com.pogeyan.cmis.data.objects.MDocumentObject;
import com.pogeyan.cmis.service.factory.DatabaseServiceFactory;

public class DBUtils {
	public static class Variables {
		public static final String OBJECTID = "id";
		public static final String PREVIOUSVERSIONOBJECTID = "previousVersionObjectId";
		public static final String NAME = "name";
		public static final String PARENTID = "parentId";
		public static final String PATH = "path";
		public static final String TYPEID = "typeId";
		public static final String BASEID = "baseId";
		public static final String VERSIONREFERENCEID = "versionReferenceId";
		public static final String ISLATESTMAJORVERSION = "isLatestMajorVersion";
		public static final String ISLATESTVERSION = "isLatestVersion";
		public static final String ISPRIVATEWORKINGCOPY = "isPrivateWorkingCopy";
		public static final String TOKEN = "token";
		public static final String ACL = "acl";
		public static final String POLICIES = "policies";
		public static final String SECONDARYTYPEIDS = "secondaryTypeIds";
	}

	public static class BaseDAO {
		@SuppressWarnings("serial")
		public static MBaseObject getByObjectId(String repositoryId, String objectId, String[] mappedColumns) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);
				}
			};
			List<MBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;

		}

		@SuppressWarnings("serial")
		public static MBaseObject getByName(String repositoryId, String name, String parentId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.NAME, name);
					if (parentId != null) {
						put(Variables.PARENTID, parentId);
					}

				}
			};
			List<MBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;

		}

		@SuppressWarnings("serial")
		public static MBaseObject getByNextVersionId(String repositoryId, ObjectId previousVersionObjectId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.PREVIOUSVERSIONOBJECTID, previousVersionObjectId);

				}
			};
			List<MBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;

		}

		@SuppressWarnings("serial")
		public static List<MBaseObject> getByTypeId(String repositoryId, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);

				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, null);

		}

		@SuppressWarnings("serial")
		public static MBaseObject getByPath(String repositoryId, String path) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.PATH, path);

				}
			};
			List<MBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;
		}

		@SuppressWarnings("serial")
		public static MBaseObject getRootFolder(String repositoryId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.NAME, CopperCmsRepository.ROOT_ID);

				}
			};
			List<MBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;
		}

		@SuppressWarnings("serial")
		public static void updatePolicy(String repositoryId, List<String> polIds, String objectId, TokenImpl token) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.POLICIES, polIds);
					put(Variables.TOKEN, token);
				}
			};
			objectMorphiaDAO.update(objectId, updateProps);
		}

		@SuppressWarnings("serial")
		public static void updateAcl(String repositoryId, Acl acl, TokenImpl token, String objectId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.ACL, acl);
					put(Variables.TOKEN, token);
				}
			};
			objectMorphiaDAO.update(objectId, updateProps);
		}

		@SuppressWarnings("serial")
		public static void updateBaseSecondaryTypeObject(String repositoryId, List<String> secondaryObjectTypes,
				String objectId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.SECONDARYTYPEIDS, secondaryObjectTypes);

				}
			};
			objectMorphiaDAO.update(objectId, updateProps);
		}
	}

	public static class DocumentDAO {
		@SuppressWarnings("serial")
		public static MDocumentObject getDocumentByObjectId(String repositoryId, String objectId,
				String[] mappedColumns) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);
				}
			};
			List<MDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;

		}

		@SuppressWarnings("serial")
		public static MDocumentObject getDocumentByName(String repositoryId, String name, String parentId) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.NAME, name);
					if (parentId != null) {
						put(Variables.PARENTID, parentId);
					}

				}
			};
			List<MDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, null);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;

		}

		@SuppressWarnings("serial")
		public static MDocumentObject getLatestVersion(String repositoryId, String referenceVersionObjectId,
				boolean major) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.VERSIONREFERENCEID, referenceVersionObjectId);
					if (major == true) {
						put(Variables.ISLATESTMAJORVERSION, true);

					} else {
						put(Variables.ISLATESTVERSION, true);
					}

				}
			};
			List<MDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, null);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;

		}

		@SuppressWarnings("serial")
		public static List<MDocumentObject> getAllVersion(String repositoryId, String versionReferenceId) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.VERSIONREFERENCEID, versionReferenceId);
					put(Variables.ISPRIVATEWORKINGCOPY, false);

				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, null);

		}

		@SuppressWarnings("serial")
		public static MDocumentObject getByDocumentByPropertiesField(String repositoryId, String typeId,
				String primaryKey, Object primaryKeyValue, String[] mappedColumns) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);
					put("properties." + primaryKey, primaryKeyValue);

				}
			};
			List<MDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;
		}
	}

	public static class RelationshipDAO {
		@SuppressWarnings("serial")
		public static List<MBaseObject> getRelationshipDocuments(String repositoryId, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);
					put(Variables.BASEID, "CMIS:FOLDER");
				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, null);
		}

		@SuppressWarnings("serial")
		public static List<MBaseObject> getRelationshipTargetIds(String repositoryId, String targetId,
				String primaryKey, Object value) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, targetId);
					put("properties." + primaryKey, value);
				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, false, 0, 0, null);

		}

		@SuppressWarnings("serial")
		public static List<MBaseObject> getRelationshipBySourceId(String repositoryId, String sourceId, int maxItems,
				int skipCount, String[] mappedColumns) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put("properties.cmis:sourceId", sourceId);
				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, true, maxItems, skipCount, mappedColumns);
		}

		@SuppressWarnings("serial")
		public static List<MBaseObject> getRelationshipByTargetId(String repositoryId, String targetId, int maxItems,
				int skipCount, String[] mappedColumns) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put("properties.cmis:targetId", targetId);
				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, true, maxItems, skipCount, mappedColumns);
		}
	}
}
