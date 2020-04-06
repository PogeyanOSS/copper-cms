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

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.commons.lang3.ArrayUtils;
import org.bson.types.ObjectId;

import com.codahale.metrics.MetricRegistryListener.Base;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.repo.CopperCmsRepository;
import com.pogeyan.cmis.api.utils.Helpers;
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
		public static final String MODIFIEDAT = "modifiedAt";
		public static final String REPOID = "repositoryId";
	}

	public static class BaseDAO {
		@SuppressWarnings("serial")
		public static IBaseObject getByObjectId(String repositoryId, String[] principalIds, boolean aclPropagation,
				String objectId, String[] mappedColumns, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);
					put(Variables.REPOID, repositoryId);
				}
			};

			List<? extends IBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, principalIds,
					aclPropagation, false, 0, 0, mappedColumns, typeId);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		public static List<? extends IBaseObject> getObjectsByIds(String repositoryId, List<String> objectIds,
				boolean aclPropagation, int maxItems, int skipCount, String[] mappedColumns, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			return objectMorphiaDAO.getObjects(objectIds, null, aclPropagation, true, maxItems, skipCount,
					mappedColumns, typeId);
		}

		@SuppressWarnings("serial")
		public static IBaseObject getByName(String repositoryId, String name, boolean aclPropagation, String parentId,
				String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.NAME, name);
					if (typeId != null && !ArrayUtils.contains(Helpers.basicTypes, typeId)) {
						put(Variables.PATH, "/" + typeId + "/" + name);
					}
					if (parentId != null) {
						put(Variables.PARENTID, parentId);
					}

				}
			};
			List<? extends IBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, null, aclPropagation,
					false, 0, 0, null, typeId);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IBaseObject getByNextVersionId(String repositoryId, ObjectId previousVersionObjectId,
				boolean aclPropagation) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.PREVIOUSVERSIONOBJECTID, previousVersionObjectId);

				}
			};

			List<? extends IBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, null, aclPropagation,
					false, 0, 0, null, null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getByTypeId(String repositoryId, String typeId,
				boolean aclPropagation) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);

				}
			};

			return objectMorphiaDAO.filter(fieldsNamesAndValues, null, true, aclPropagation, 0, 0, null, typeId);
		}

		@SuppressWarnings("serial")
		public static IBaseObject getByPath(String repositoryId, String[] principalIds, boolean aclPropagation,
				String path, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.PATH, path);
				}
			};
			List<? extends IBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, principalIds,
					aclPropagation, false, 0, 0, null, typeId);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IBaseObject getRootFolder(String repositoryId, String typeId, boolean aclPropagation) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.NAME, CopperCmsRepository.ROOT_ID);

				}
			};
			List<? extends IBaseObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, null, aclPropagation,
					false, 0, 0, null, typeId);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static void updatePolicy(String repositoryId, List<String> polIds, String objectId, TokenImpl token,
				String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.POLICIES, polIds);
					put(Variables.TOKEN, token);
				}
			};
			objectMorphiaDAO.update(repositoryId, objectId, updateProps, typeId);
		}

		@SuppressWarnings("serial")
		public static void updateAcl(String repositoryId, Acl acl, TokenImpl token, String objectId, long modifiedTime,
				String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.ACL, acl);
					put(Variables.TOKEN, token);
					put(Variables.MODIFIEDAT, modifiedTime);
				}
			};
			objectMorphiaDAO.update(repositoryId, objectId, updateProps, typeId);
		}

		@SuppressWarnings("serial")
		public static void updateBaseSecondaryTypeObject(String repositoryId, List<String> secondaryObjectTypes,
				String objectId, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> updateProps = new HashMap<String, Object>() {
				{
					put(Variables.SECONDARYTYPEIDS, secondaryObjectTypes);
					put("properties." + PropertyIds.SECONDARY_OBJECT_TYPE_IDS, secondaryObjectTypes);

				}
			};
			objectMorphiaDAO.update(repositoryId, objectId, updateProps, typeId);
		}
	}

	public static class DocumentDAO {
		@SuppressWarnings("serial")
		public static IDocumentObject getDocumentByObjectId(String repositoryId, String objectId,
				String[] mappedColumns) {
			MDocumentObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.OBJECTID, objectId);
				}
			};

			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IDocumentObject getDocumentByName(String repositoryId, String name, String parentId) {
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
			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static IDocumentObject getLatestVersion(String repositoryId, String referenceVersionObjectId,
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

			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, null);
			if (result.size() > 0) {
				return result.get(0);
			}

			return null;
		}

		@SuppressWarnings("serial")
		public static List<? extends IDocumentObject> getAllVersion(String repositoryId, String versionReferenceId) {
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

			List<? extends IDocumentObject> result = objectMorphiaDAO.filter(fieldsNamesAndValues, mappedColumns);
			if (result.size() > 0) {
				return result.get(0);
			}
			return null;
		}
	}

	public static class RelationshipDAO {
		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipDocuments(String repositoryId, String typeId,
				boolean aclPropagation) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, typeId);
					put(Variables.BASEID, "CMIS:FOLDER");
				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, null, aclPropagation, false, 0, 0, null, typeId);
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipTargetIds(String repositoryId, String targetId,
				boolean aclPropagation, String primaryKey, Object value) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put(Variables.TYPEID, targetId);
					put("properties." + primaryKey, value);
				}
			};

			return objectMorphiaDAO.filter(fieldsNamesAndValues, null, aclPropagation, false, 0, 0, null, targetId);
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipBySourceId(String repositoryId, String sourceId,
				boolean aclPropagation, int maxItems, int skipCount, String[] mappedColumns, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put("properties.cmis:sourceId", sourceId);
				}
			};

			return objectMorphiaDAO.filter(fieldsNamesAndValues, null, aclPropagation, true, maxItems, skipCount,
					mappedColumns, typeId);
		}

		@SuppressWarnings("serial")
		public static List<? extends IBaseObject> getRelationshipByTargetId(String repositoryId, String targetId,
				boolean aclPropagation, int maxItems, int skipCount, String[] mappedColumns, String typeId) {
			MBaseObjectDAO objectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);
			HashMap<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
				{
					put("properties.cmis:targetId", targetId);
				}
			};
			return objectMorphiaDAO.filter(fieldsNamesAndValues, null, aclPropagation, true, maxItems, skipCount,
					mappedColumns, typeId);
		}
	}

	public static class TypeServiceDAO {
		@SuppressWarnings("unchecked")
		public static List<? extends TypeDefinition> getById(String repositoryId, List<?> typeId,
				String[] fieldAccess) {
			List<? extends TypeDefinition> typeDef = ((List<TypeDefinition>) CacheProviderServiceFactory
					.getTypeCacheServiceProvider().get(repositoryId, typeId));
			if (typeDef == null || typeDef != null && !typeDef.isEmpty() && typeDef.get(0) == null) {
				if (typeId != null) {
					MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
							.getObjectService(repositoryId, MTypeManagerDAO.class);
					MDocumentTypeManagerDAO docManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
							.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
					typeDef = typeManagerDAO.getById(typeId, fieldAccess);
					typeDef.stream().forEach((k) -> {
						if (k.getBaseTypeId().equals(BaseTypeId.CMIS_DOCUMENT)) {
							CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, k.getId(),
									docManagerDAO.getByTypeId(k.getId(), fieldAccess));
						} else {
							CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, k.getId(), k);
						}

					});
				}
			}
			return typeDef;
		}

		public static List<? extends TypeDefinition> getChildrenIds(String repositoryId, String parentId, int maxItems,
				int skipCount) {
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			return typeManagerDAO.getChildrenIds(parentId, maxItems, skipCount);

		}

		@SuppressWarnings("unchecked")
		public static PropertyDefinition<?> getAllPropertyById(String repositoryId, String propId,
				String[] fieldAccess) {
			List<? extends TypeDefinition> typeDefList = ((List<TypeDefinition>) CacheProviderServiceFactory
					.getTypeCacheServiceProvider().get(repositoryId, null));
			TypeDefinition typeDef = typeDefList.stream().filter(a -> a.getPropertyDefinitions().get(propId) != null)
					.findFirst().orElse(null);
			if (typeDef != null) {
				return typeDef.getPropertyDefinitions().get(propId);
			} else {
				MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MTypeManagerDAO.class);
				return typeManagerDAO.getAllPropertyById(propId, fieldAccess);
			}

		}
	}

	public static class DocumentTypeManagerDAO {
		@SuppressWarnings("unchecked")
		public static DocumentTypeDefinition getByTypeId(String repositoryId, String typeId, String[] fieldAccess) {
			MDocumentTypeManagerDAO docManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
			List<? extends DocumentTypeDefinition> docType = ((List<DocumentTypeDefinition>) CacheProviderServiceFactory
					.getTypeCacheServiceProvider().get(repositoryId, Arrays.asList(typeId)));
			if (docType != null && !docType.isEmpty() && docType.get(0) == null) {
				DocumentTypeDefinition docTypeDef = docManagerDAO.getByTypeId(typeId, fieldAccess);
				CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, docTypeDef.getId(),
						docTypeDef);
				return docTypeDef;
			}
			return docType.get(0);
		}
	}
}
