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

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.data.jdo.JAclImpl;
import com.pogeyan.cmis.data.jdo.JTokenImpl;
import com.pogeyan.cmis.impl.utils.DBUtils;

import groovy.lang.GroovyObject;

public class JBaseObjectDAOImpl implements MBaseObjectDAO {
	private static final Logger LOG = LoggerFactory.getLogger(JBaseObjectDAOImpl.class);

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public IBaseObject getLatestToken(String repositoryId) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOServiceImpl.getInstance().getEnhanceClass(repositoryId, "JBaseObject");
				Extent QueryExtent = pm.getExtent(objectClass, true);
				Query query = pm.newQuery(QueryExtent);
				query.setOrdering("token.time desc");
				query.setUnique(true);
				return (IBaseObject) query.execute();
			}
		} catch (Exception e) {
			LOG.error("getLatestToken Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void delete(String repositoryId, String objectId, String typeId, boolean forceDelete, TokenImpl token) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			Transaction tx = pm.currentTransaction();
			if (pm != null) {
				Class<?> objectClass = JDOServiceImpl.getInstance().getEnhanceClass(repositoryId,
						JDOHelper.Impl.getJDOTypeId(typeId, true));
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

	@SuppressWarnings("unchecked")
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
						JDOHelper.Impl.getJDOTypeId(typeId, true));
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

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public List<? extends IBaseObject> filter(String repositoryId, String typeId, Map<String, Object> fieldNames,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOServiceImpl.getInstance().getEnhanceClass(repositoryId,
						JDOHelper.Impl.getJDOTypeId(typeId, true));
				if (objectClass != null) {
					Extent QueryExtent = pm.getExtent(objectClass, true);
					Query query = pm.newQuery(QueryExtent);
					query.declareParameters(JDOHelper.Impl.getDeclareParameter(fieldNames));
					query.setFilter(JDOHelper.Impl.getFilterParameter(fieldNames));
					if (includePagination) {
						if (maxItems > 0) {
							query.setRange(skipCount, maxItems);
						} else {
							query.setRange(skipCount, 0);
						}
					}
					if (mappedColumns != null && mappedColumns.length > 0) {

					}
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					return (List<? extends IBaseObject>) query.executeWithMap(fieldNames);
				}
			}
		} catch (Exception e) {
			LOG.error("filter Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@Override
	public void commit(String repositoryId, IBaseObject entity) {
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

	@SuppressWarnings({ "rawtypes", "unused" })
	@Override
	public IBaseObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId) {
		try {
			JTypeManagerDAOImpl typeMorphia = new JTypeManagerDAOImpl();
			Map<String, Object> JBaseObjectClassMap = new HashMap<>();
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(fRepositoryId,
					Arrays.asList(typeId));
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
							|| type.getParentTypeId() != BaseTypeId.CMIS_ITEM.toString()
							|| type.getParentTypeId() != BaseTypeId.CMIS_POLICY.toString()
							|| type.getParentTypeId() != BaseTypeId.CMIS_SECONDARY.toString()) {
						classMap.put("parentClassName", type.getParentTypeId());

					} else {
						classMap.put("parentClassName", "JBaseObject");
					}
					Class enhancedCC = new JDOServiceImpl().load(fRepositoryId, typeId, "JProperties", classMap);
					myInstance = (GroovyObject) enhancedCC.newInstance();
					myInstance = JDOHelper.Impl.setPropDefFields(properties, myInstance);
					myInstance.invokeMethod("setProperties", properties);
				} else {
					Class enhancedCC = new JDOServiceImpl().load(fRepositoryId, "JBaseObject", "JBaseObject",
							JBaseObjectClassMap);
					myInstance = (GroovyObject) enhancedCC.newInstance();
				}
			}
			myInstance.invokeMethod("setId", (new ObjectId()).toString());
			myInstance.invokeMethod("setName", name);
			myInstance.invokeMethod("setRepositoryId", fRepositoryId);
			myInstance.invokeMethod("setBaseId", baseId);
			myInstance.invokeMethod("setTypeId", typeId);
			myInstance.invokeMethod("getSecondaryTypeIds", secondaryTypeIds);
			myInstance.invokeMethod("setDescription", description);
			myInstance.invokeMethod("setCreatedBy", createdBy);
			myInstance.invokeMethod("setModifiedBy", modifiedBy);
			myInstance.invokeMethod("setCreatedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setModifiedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setToken", token);
			myInstance.invokeMethod("setInternalPath", internalPath);
			myInstance.invokeMethod("setPolicies", policies);
			myInstance.invokeMethod("setAcl", acl);
			myInstance.invokeMethod("setPath", path);
			myInstance.invokeMethod("setParentId", parentId);
			myInstance.invokeMethod("setPolicies", policies);
			return (IBaseObject) myInstance;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;

	}

}
