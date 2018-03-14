package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jdo.Extent;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;
import javax.jdo.Transaction;

import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.data.jdo.JAclImpl;
import com.pogeyan.cmis.data.jdo.JTokenImpl;
import com.pogeyan.cmis.impl.utils.DBUtils;

import groovy.lang.GroovyObject;

public class JDocumentObjectDAOImpl implements MDocumentObjectDAO {
	private static final Logger LOG = LoggerFactory.getLogger(JDocumentObjectDAOImpl.class);

	@Override
	public void delete(String repositoryId, String objectId, String typeId, List<String> removeProps,
			boolean forceDelete, boolean removefields, TokenImpl token) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			Transaction tx = pm.currentTransaction();
			if (pm != null) {

				Class<?> objectClass = JDOServiceImpl.getInstance().getEnhanceClass(repositoryId,
						JDOHelper.Impl.getJDOTypeId(typeId, false));
				Extent QueryExtent = pm.getExtent(objectClass, true);
				GroovyObject myInstance = (GroovyObject) objectClass.newInstance();
				Query query = pm.newQuery(QueryExtent, "id == " + objectId + " && token.changeType != 2");
				query.setUnique(true);
				myInstance = (GroovyObject) query.execute();
				if (forceDelete) {
					pm.deletePersistent(myInstance);
				} else {
					tx.begin();
					Map<String, Object> tokenUpdate = new HashMap<>();
					tokenUpdate.put("token", 2);
					myInstance = JDOHelper.Impl.setPropDefFields(tokenUpdate, myInstance);
					tx.commit();

				}
			}

		} catch (Exception e) {
			LOG.error("delete Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
	}

	@Override
	public void update(String repositoryId, String objectId, String typeId, Map<String, Object> updateProps) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			Transaction tx = pm.currentTransaction();
			if (pm != null) {
				tx.begin();
				if (updateProps.get("acl") != null) {
					JAclImpl mAcl = JDOHelper.Impl.convertJDOAcl((AccessControlListImplExt) updateProps.get("acl"));
					updateProps.remove("acl");
					updateProps.put("acl", mAcl);
				}
				if (updateProps.get("token") != null) {
					JTokenImpl mToken = JDOHelper.Impl.convertJDOToken((TokenImpl) updateProps.get("token"));
					updateProps.remove("token");
					updateProps.put("stoken", mToken);
				}
				Class<?> objectClass = JDOServiceImpl.getInstance().getEnhanceClass(repositoryId,
						JDOHelper.Impl.getJDOTypeId(typeId, false));
				Extent QueryExtent = pm.getExtent(objectClass, true);
				GroovyObject myInstance = (GroovyObject) objectClass.newInstance();
				Query query = pm.newQuery(QueryExtent, "id == " + objectId + " && token.changeType != 2");
				query.setUnique(true);
				myInstance = (GroovyObject) query.execute();
				myInstance = JDOHelper.Impl.setPropDefFields(updateProps, myInstance);
				tx.commit();
			}

		} catch (Exception e) {
			LOG.error("update Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
	}

	@Override
	public List<? extends IDocumentObject> getCheckOutDocs(String repositoryId, String folderId, String typeId,
			String[] principalIds, boolean aclPropagation, int maxItems, int skipCount, String orderBy) {
		try {
			Map<String, Object> fieldNames = new HashMap<>();
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			Transaction tx = pm.currentTransaction();
			if (pm != null) {
				if (folderId == null) {
					fieldNames.put("typeId", "cmis:document");
					fieldNames.put("isPrivateWorkingCopy", true);
				} else {
					fieldNames.put("parentId", folderId);
					fieldNames.put("typeId", "cmis:document");
					fieldNames.put("isPrivateWorkingCopy", true);
				}
				Class<?> objectClass = JDOServiceImpl.getInstance().getEnhanceClass(repositoryId,
						JDOHelper.Impl.getJDOTypeId(typeId, false));
				Extent QueryExtent = pm.getExtent(objectClass, true);
				Query query = pm.newQuery(QueryExtent);
				if (aclPropagation) {
					query.declareParameters(JDOHelper.Impl.getDeclareParameter(fieldNames));
					query.setFilter(JDOHelper.Impl.getFilterParameter(fieldNames));
				} else {

					query.declareParameters(JDOHelper.Impl.getDeclareParameter(fieldNames));
					query.setFilter(JDOHelper.Impl.getFilterParameter(fieldNames));
				}

				if (orderBy != null) {
					query.setOrdering(orderBy + " desc");
				}
				if (maxItems > 0) {
					query.setRange(skipCount, maxItems);
				} else {
					query.setRange(skipCount, 0);
				}
				if (aclPropagation) {

				}
			}
		} catch (Exception e) {
			LOG.error("getCheckOutDocs Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public long getCheckOutDocsSize(String repositoryId, String folderId, String typeId, String[] principalIds,
			boolean aclPropagation) {

		return 0;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends IDocumentObject> filter(String repositoryId, Map<String, Object> fieldNames, String typeId,
			String[] mappedColumns) {

		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOServiceImpl.getInstance().getEnhanceClass(repositoryId,
						JDOHelper.Impl.getJDOTypeId(typeId, false));
				if (objectClass != null) {
					Extent QueryExtent = pm.getExtent(objectClass, true);
					Query query = pm.newQuery(QueryExtent);
					query.declareParameters(JDOHelper.Impl.getDeclareParameter(fieldNames));
					query.setFilter(JDOHelper.Impl.getFilterParameter(fieldNames));
					if (mappedColumns != null && mappedColumns.length > 0) {

					}
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					return (List<? extends IDocumentObject>) query.executeWithMap(fieldNames);
				}

			}
		} catch (Exception e) {
			LOG.error("getLatestToken Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@Override
	public void commit(String repositoryId, IDocumentObject entity) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			pm.makePersistent(entity);
			tx.commit();
		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
		}
	}

	@Override
	public IDocumentObject createObjectFacade(IBaseObject baseObject, Boolean isImmutable, Boolean isLatestVersion,
			Boolean isMajorVersion, Boolean isLatestMajorVersion, Boolean isPrivateWorkingCopy, String versionLabel,
			String versionSeriesId, String versionReferenceId, Boolean isVersionSeriesCheckedOut,
			String versionSeriesCheckedOutBy, String versionSeriesCheckedOutId, String checkinComment,
			Long contentStreamLength, String contentStreamMimeType, String contentStreamFileName,
			String contentStreamId, String previousVersionObjectId) {
		try {
			Map<String, Object> JBaseObjectClassMap = new HashMap<>();
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(baseObject.getRepositoryId(),
					Arrays.asList(baseObject.getTypeId()));
			GroovyObject myInstance = null;
			if (typeDef != null && typeDef.size() > 0) {
				TypeDefinition type = typeDef.get(0);
				if (type.getParentTypeId() != null) {
					List<Map<String, Object>> listFields = JDOHelper.Impl
							.getPropDefFields(type.getPropertyDefinitions());
					Map<String, Object> classMap = new HashMap<>();
					classMap.put("className", type.getId());
					classMap.put("propDef", listFields);
					if (type.getParentTypeId() != BaseTypeId.CMIS_FOLDER.toString()
							&& type.getParentTypeId() != BaseTypeId.CMIS_ITEM.toString()
							&& type.getParentTypeId() != BaseTypeId.CMIS_POLICY.toString()
							&& type.getParentTypeId() != BaseTypeId.CMIS_SECONDARY.toString()) {
						classMap.put("parentClassName", type.getParentTypeId());

					} else {
						classMap.put("parentClassName", "JDocumentObject");
					}
					Class enhancedCC = new JDOServiceImpl().load(baseObject.getRepositoryId(), baseObject.getTypeId(),
							"JProperties", classMap);
					myInstance = (GroovyObject) enhancedCC.newInstance();
					myInstance = JDOHelper.Impl.setPropDefFields(baseObject.getProperties(), myInstance);
					myInstance.invokeMethod("setProperties", baseObject.getProperties());
				} else {
					Class enhancedCC = new JDOServiceImpl().load(baseObject.getRepositoryId(), "JDocumentObject",
							"JDocumentObject", JBaseObjectClassMap);
					myInstance = (GroovyObject) enhancedCC.newInstance();
				}
			}
			// Document Properties
			myInstance.invokeMethod("setIsImmutable", isImmutable);
			myInstance.invokeMethod("setIsLatestVersion", isLatestVersion);
			myInstance.invokeMethod("setIsMajorVersion", isMajorVersion);
			myInstance.invokeMethod("setIsLatestMajorVersion", isLatestMajorVersion);
			myInstance.invokeMethod("setIsPrivateWorkingCopy", isPrivateWorkingCopy);
			myInstance.invokeMethod("setVersionLabel", versionLabel);
			myInstance.invokeMethod("setVersionSeriesId", versionSeriesId);
			myInstance.invokeMethod("setVersionReferenceId", versionReferenceId);
			myInstance.invokeMethod("setIsVersionSeriesCheckedOut", isVersionSeriesCheckedOut);
			myInstance.invokeMethod("setVersionSeriesCheckedOutBy", versionSeriesCheckedOutBy);
			myInstance.invokeMethod("setVersionSeriesCheckedOutId", versionSeriesCheckedOutId);
			myInstance.invokeMethod("setCheckinComment", checkinComment);
			myInstance.invokeMethod("setContentStreamLength", contentStreamLength);
			myInstance.invokeMethod("setContentStreamMimeType", contentStreamMimeType);
			myInstance.invokeMethod("setContentStreamFileName", contentStreamFileName);
			myInstance.invokeMethod("setContentStreamId", contentStreamId);
			myInstance.invokeMethod("setPreviousVersionObjectId", previousVersionObjectId);

			// Base Properties
			myInstance.invokeMethod("setId", (new ObjectId()).toString());
			myInstance.invokeMethod("setName", baseObject.getName());
			myInstance.invokeMethod("setRepositoryId", baseObject.getRepositoryId());
			myInstance.invokeMethod("setBaseId", baseObject.getBaseId());
			myInstance.invokeMethod("setTypeId", baseObject.getTypeId());
			myInstance.invokeMethod("getSecondaryTypeIds", baseObject.getSecondaryTypeIds());
			myInstance.invokeMethod("setDescription", baseObject.getDescription());
			myInstance.invokeMethod("setCreatedBy", baseObject.getCreatedBy());
			myInstance.invokeMethod("setModifiedBy", baseObject.getModifiedBy());
			myInstance.invokeMethod("setCreatedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setModifiedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setToken", baseObject.getChangeToken());
			myInstance.invokeMethod("setInternalPath", baseObject.getInternalPath());
			myInstance.invokeMethod("setPolicies", baseObject.getPolicies());
			myInstance.invokeMethod("setAcl", baseObject.getAcl());
			myInstance.invokeMethod("setPath", baseObject.getPath());
			myInstance.invokeMethod("setParentId", baseObject.getParentId());
			myInstance.invokeMethod("setPolicies", baseObject.getPolicies());
			return (IDocumentObject) myInstance;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

}
