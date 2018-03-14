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

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.bson.types.ObjectId;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.repo.CopperCmsRepository;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;

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
		public static IBaseObject getByObjectId(String repositoryId, String objectId, String typeId,
				String[] mappedColumns) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);
				}
			};
			List<? extends IBaseObject> result = objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues,
					false, 0, 0, mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IBaseObject getByName(String repositoryId, String name, String typeId, String parentId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.NAME, name);
					if (parentId != null) {
						put(Variables.PARENTID, parentId);
					}

				}
			};
			List<? extends IBaseObject> result = objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues,
					false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IBaseObject getByNextVersionId(String repositoryId, String typeId,
				ObjectId previousVersionObjectId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.PREVIOUSVERSIONOBJECTID, previousVersionObjectId);

				}
			};

			List<? extends IBaseObject> result = objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues,
					false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getByTypeId(String repositoryId, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);

				}
			};

			return objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues, false, 0, 0, null);
		}

		@SuppressWarnings("serial")
		public static String getByObjectTypeId(String repositoryId, String objectId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);

				}
			};

			return objectMorphiaDAO.filter(repositoryId, "JBaseObject", fieldsNamesAndValues, false, 0, 0, null).get(0)
					.getTypeId();
		}

		@SuppressWarnings("serial")
		public static String getByPathTypeId(String repositoryId, String path) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.PATH, path);

				}
			};

			return objectMorphiaDAO.filter(repositoryId, "JBaseObject", fieldsNamesAndValues, false, 0, 0, null).get(0)
					.getTypeId();
		}

		@SuppressWarnings("serial")
		public static IBaseObject getByPath(String repositoryId, String typeId, String path) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.PATH, path);
				}
			};
			List<? extends IBaseObject> result = objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues,
					false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IBaseObject getRootFolder(String repositoryId, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.NAME, CopperCmsRepository.ROOT_ID);

				}
			};
			List<? extends IBaseObject> result = objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues,
					false, 0, 0, null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static void updatePolicy(String repositoryId, String typeId, List<String> polIds, String objectId,
				TokenImpl token) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.POLICIES, polIds);
					put(Variables.TOKEN, token);
				}
			};
			objectMorphiaDAO.update(repositoryId, typeId, objectId, updateProps);
		}

		@SuppressWarnings("serial")
		public static void updateAcl(String repositoryId, String typeId, Acl acl, TokenImpl token, String objectId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.ACL, acl);
					put(Variables.TOKEN, token);
				}
			};
			objectMorphiaDAO.update(repositoryId, objectId, typeId, updateProps);
		}

		@SuppressWarnings("serial")
		public static void updateBaseSecondaryTypeObject(String repositoryId, String typeId,
				List<String> secondaryObjectTypes, String objectId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.SECONDARYTYPEIDS, secondaryObjectTypes);
					put("properties." + PropertyIds.SECONDARY_OBJECT_TYPE_IDS, secondaryObjectTypes);

				}
			};
			objectMorphiaDAO.update(repositoryId, objectId, typeId, updateProps);
		}
	}

	public static class DocumentDAO {
		@SuppressWarnings("serial")
		public static IDocumentObject getDocumentByObjectId(String repositoryId, String objectId, String typeId,
				String[] mappedColumns) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);
				}
			};

			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(repositoryId, fieldsNamesAndValues, typeId,
					mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static String getDocumentByTypeID(String repositoryId, String objectId, String[] mappedColumns) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);
				}
			};

			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(repositoryId, fieldsNamesAndValues,
					"JBaseObject", mappedColumns);
			if (result.size() > 0) {
				return result.get(0).getTypeId();
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IDocumentObject getDocumentByName(String repositoryId, String name, String typeId,
				String parentId) {
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
			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(repositoryId, fieldsNamesAndValues, typeId,
					null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IDocumentObject getLatestVersion(String repositoryId, String typeId,
				String referenceVersionObjectId, boolean major) {
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

			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(repositoryId, fieldsNamesAndValues, typeId,
					null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static List<? extends IDocumentObject> getAllVersion(String repositoryId, String typeId,
				String versionReferenceId) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.VERSIONREFERENCEID, versionReferenceId);
					put(Variables.ISPRIVATEWORKINGCOPY, false);

				}
			};

			return objectMorphiaDAO.filter(repositoryId, fieldsNamesAndValues, typeId, null);
		}

		@SuppressWarnings("serial")
		public static IDocumentObject getByDocumentByPropertiesField(String repositoryId, String typeId,
				String primaryKey, Object primaryKeyValue, String[] mappedColumns) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);
					put("properties." + primaryKey, primaryKeyValue);

				}
			};

			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(repositoryId, fieldsNamesAndValues, typeId,
					mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;
		}
	}

	public static class RelationshipDAO {
		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipDocuments(String repositoryId, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);
					put(Variables.BASEID, "CMIS:FOLDER");
				}
			};
			return objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues, false, 0, 0, null);
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipTargetIds(String repositoryId, String typeId,
				String targetId, String primaryKey, Object value) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, targetId);
					put("properties." + primaryKey, value);
				}
			};

			return objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues, false, 0, 0, null);
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipBySourceId(String repositoryId, String typeId,
				String sourceId, int maxItems, int skipCount, String[] mappedColumns) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put("properties.cmis:sourceId", sourceId);
				}
			};

			return objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues, true, maxItems, skipCount,
					mappedColumns);
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipByTargetId(String repositoryId, String typeId,
				String targetId, int maxItems, int skipCount, String[] mappedColumns) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put("properties.cmis:targetId", targetId);
				}
			};
			return objectMorphiaDAO.filter(repositoryId, typeId, fieldsNamesAndValues, true, maxItems, skipCount,
					mappedColumns);
		}
	}

	public static class TypeServiceDAO {
		@SuppressWarnings("unchecked")
		public static List<? extends TypeDefinition> getById(String repositoryId, List<?> typeId) {
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			MDocumentTypeManagerDAO docManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
			List<? extends TypeDefinition> typeDef = ((List<TypeDefinition>) CacheProviderServiceFactory
					.getTypeCacheServiceProvider().get(repositoryId, typeId));
			if (typeDef != null && !typeDef.isEmpty() && typeDef.get(0) == null) {
				typeDef = typeManagerDAO.getById(repositoryId, typeId);
				typeDef.stream().forEach((k) -> {
					if (k.getBaseTypeId().equals(BaseTypeId.CMIS_DOCUMENT)) {
						CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, k.getId(),
								docManagerDAO.getByTypeId(repositoryId, k.getId()));
					} else {
						CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, k.getId(), k);
					}

				});
			}
			return typeDef;
		}

		public static List<? extends TypeDefinition> getChildrenIds(String repositoryId, String parentId, int maxItems,
				int skipCount) {
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			return typeManagerDAO.getChildrenIds(repositoryId, parentId, maxItems, skipCount);

		}

		public static Map<String, PropertyDefinition<?>> getAllPropertyById(String repositoryId, String propId) {
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			return typeManagerDAO.getAllPropertyById(repositoryId, propId);
		}
	}

	public static class DocumentTypeManagerDAO {
		@SuppressWarnings("unchecked")
		public static DocumentTypeDefinition getByTypeId(String repositoryId, String typeId) {
			MDocumentTypeManagerDAO docManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
			List<? extends DocumentTypeDefinition> docType = ((List<DocumentTypeDefinition>) CacheProviderServiceFactory
					.getTypeCacheServiceProvider().get(repositoryId, Arrays.asList(typeId)));
			if (docType != null && !docType.isEmpty() && docType.get(0) == null) {
				DocumentTypeDefinition docTypeDef = docManagerDAO.getByTypeId(repositoryId, typeId);
				CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, docTypeDef.getId(),
						docTypeDef);
				return docTypeDef;
			}
			return docType.get(0);
		}
	}
}
