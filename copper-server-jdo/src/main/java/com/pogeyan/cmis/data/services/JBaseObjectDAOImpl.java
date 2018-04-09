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

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.bson.types.ObjectId;
import org.datanucleus.store.rdbms.query.ForwardQueryResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.data.jdo.JAclConverter;
import com.pogeyan.cmis.data.jdo.JRelationship;
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
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, "cmis:folder", true);
				Extent QueryExtent = pm.getExtent(objectClass, true);
				Query query = pm.newQuery(QueryExtent);
				query.setOrdering("token.time desc");
				ForwardQueryResult fQueryResult = (ForwardQueryResult) query.execute();
				return (IBaseObject) fQueryResult.get(0);
			}
		} catch (Exception e) {
			LOG.error("getLatestToken Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return null;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void delete(String repositoryId, String objectId, String typeId, boolean forceDelete, TokenImpl token) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, true);
				Extent<?> QueryExtent = pm.getExtent(objectClass, true);
				GroovyObject myInstance = (GroovyObject) objectClass.newInstance();
				Query<?> query = pm.newQuery(QueryExtent);
				query.declareParameters("String objectId,int tokenType");
				query.setFilter("this.id == objectId && token.changeType != tokenType");
				Map<String, Object> field = new HashMap<>();
				field.put("objectId", objectId);
				field.put("tokenType", TokenChangeType.DELETED.value());
				ForwardQueryResult fQueryResult = (ForwardQueryResult) query.executeWithMap(field);
				myInstance = (GroovyObject) fQueryResult.get(0);
				if (forceDelete) {
					pm.deletePersistent(myInstance);
				} else {
					myInstance.invokeMethod("setToken", token);
				}
			}
			tx.commit();
		} catch (Exception e) {
			LOG.error("delete Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
		}

	}

	@Override
	public void update(String repositoryId, String objectId, String typeId, Map<String, Object> updateProps) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, true);
				if (objectClass != null) {
					Extent<?> QueryExtent = pm.getExtent(objectClass, true);
					GroovyObject myInstance = (GroovyObject) objectClass.newInstance();
					Query<?> query = pm.newQuery(QueryExtent);
					query.declareParameters("String objectId,int tokenType");
					query.setFilter("this.id == objectId && token.changeType != tokenType");
					Map<String, Object> field = new HashMap<>();
					field.put("objectId", objectId);
					field.put("tokenType", TokenChangeType.DELETED.value());
					ForwardQueryResult<?> fQueryResult = (ForwardQueryResult<?>) query.executeWithMap(field);
					myInstance = (GroovyObject) fQueryResult.get(0);
					if (updateProps.get("acl") != null) {
						JAclConverter jAcl = new JAclConverter(objectId,
								(AccessControlListImplExt) updateProps.get("acl"));
						myInstance.invokeMethod("setAcl", jAcl);
						updateProps.remove("acl");
					}
					if (updateProps.get("token") != null) {
						TokenImpl tokenInstance = new TokenImpl(TokenChangeType.UPDATED, System.currentTimeMillis());
						myInstance.invokeMethod("setToken", tokenInstance);
						updateProps.remove("token");
					}
					myInstance = JDOHelper.Impl.propDefFields(updateProps, "set", myInstance);
				}
			}
			tx.commit();
		} catch (Exception e) {
			LOG.error("update Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
		}

	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public List<? extends IBaseObject> filter(String repositoryId, String typeId, Map<String, Object> fieldNames,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns) {
		try {

			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			String declareParameter = null;
			String filterParameter = null;
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, true);
				if (objectClass != null) {
					Extent QueryExtent = pm.getExtent(objectClass, true);
					Query query = null;
					query = pm.newQuery(QueryExtent);
					if (mappedColumns == null) {
						query = pm.newQuery(QueryExtent);
					} else {
						query = pm.newQuery(QueryExtent);
						String mappedField = JDOHelper.Impl.getMappedColumns(mappedColumns);
						// query = pm.newQuery("SELECT " + mappedField + ",acl FROM " +
						// objectClass.getName());
						query.setResult(mappedField + ",this.acl");
						query.setResultClass(objectClass);
						// query.declareVariables("com.pogeyan.cmis.data.jdo.JAclImpl jacl;");
					}

					if (typeId.equalsIgnoreCase(BaseTypeId.CMIS_RELATIONSHIP.value())) {
						query.declareVariables("com.pogeyan.cmis.data.jdo.JRelationship rel; ");
						if (fieldNames.get("sourceId") != null) {
							declareParameter = "String sourceId,int tokenType";
							filterParameter = "this.id == rel.baseId && rel.sourceId == sourceId && token.changeType != tokenType";
						} else if (fieldNames.get("targetId") != null) {
							declareParameter = "String targetId,int tokenType";
							filterParameter = "this.id == rel.baseId && rel.targetId == targetId && token.changeType != tokenType";
						}
					} else {
						declareParameter = JDOHelper.Impl.getDeclareParameter(fieldNames);
						filterParameter = JDOHelper.Impl.getFilterParameter(fieldNames);
					}

					query.declareParameters(declareParameter);
					query.setFilter(filterParameter);
					if (includePagination) {
						if (maxItems > 0) {
							query.setRange(skipCount, skipCount + maxItems);
						} else {
							query.setRange(skipCount, 0);
						}
					}
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					// ForwardQueryResult forwardQuery = (ForwardQueryResult)
					// query.executeWithMap(fieldNames);
					List<Object> result = (List<Object>) query.executeWithMap(fieldNames);

					List<String> propField = JDOHelper.Impl.getPropertyDefinition(repositoryId, typeId,
							new ArrayList<>());
					if (!result.isEmpty()) {
						return JDOHelper.Impl.getBaseObject(objectClass, result, propField);
					}
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
	public IBaseObject createObjectFacade(String objectId, String name, BaseTypeId baseId, String typeId,
			String fRepositoryId, List<String> secondaryTypeIds, String description, String createdBy,
			String modifiedBy, TokenImpl token, String internalPath, Map<String, Object> properties,
			List<String> policies, Acl acl, String path, String parentId) {
		try {
			Map<String, Object> JBaseObjectClassMap = new HashMap<>();
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(fRepositoryId,
					Arrays.asList(typeId));
			if (typeDef == null && typeDef != null && typeDef.size() == 0) {
				return null;
			}

			GroovyObject myInstance = null;
			TypeDefinition type = typeDef.get(0);
			if (type.getParentTypeId() != null) {
				Class enhancedCC = JDOHelper.Impl.load(fRepositoryId, typeId, true);
				myInstance = (GroovyObject) enhancedCC.newInstance();
				myInstance = JDOHelper.Impl.propDefFields(properties, "set", myInstance);
				myInstance.invokeMethod("setProperties", properties);
			} else {
				Class enhancedCC = JDOHelper.Impl.load(fRepositoryId, typeId, true);
				myInstance = (GroovyObject) enhancedCC.newInstance();
			}
			if (objectId == null) {
				objectId = (new ObjectId()).toString();
			}

			myInstance.invokeMethod("setId", objectId);
			myInstance.invokeMethod("setName", name);
			myInstance.invokeMethod("setRepositoryId", fRepositoryId);
			myInstance.invokeMethod("setBaseId", baseId);
			myInstance.invokeMethod("setTypeId", typeId);
			myInstance.invokeMethod("setSecondaryTypeIds", secondaryTypeIds);
			myInstance.invokeMethod("setDescription", description);
			myInstance.invokeMethod("setCreatedBy", createdBy);
			myInstance.invokeMethod("setModifiedBy", modifiedBy);
			myInstance.invokeMethod("setCreatedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setModifiedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setToken", token);
			myInstance.invokeMethod("setInternalPath", internalPath);
			myInstance.invokeMethod("setPolicies", policies);
			JAclConverter jAcl = null;
			if (acl != null) {
				jAcl = new JAclConverter(objectId, acl);
			}
			myInstance.invokeMethod("setAcl", jAcl);
			myInstance.invokeMethod("setPath", path);
			myInstance.invokeMethod("setParentId", parentId);
			myInstance.invokeMethod("setPolicies", policies);
			if (properties != null) {
				myInstance.invokeMethod("setPropString", properties.toString());
			}
			if (typeId.equalsIgnoreCase(BaseTypeId.CMIS_RELATIONSHIP.value())) {
				List<JRelationship> relationList = new ArrayList<>();
				JRelationship jRelationship = new JRelationship(objectId, properties.get("cmis:sourceId").toString(),
						properties.get("cmis:targetId").toString());
				relationList.add(jRelationship);
				myInstance.invokeMethod("setRelationship", relationList);
			}
			if (properties != null && properties.size() > 0) {
				if (properties.get(PropertyIds.POLICY_TEXT) != null) {
					myInstance.invokeMethod("setPolicyText", properties.get(PropertyIds.POLICY_TEXT).toString());
				}
			}
			return (IBaseObject) myInstance;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;

	}

}
