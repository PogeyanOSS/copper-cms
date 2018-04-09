package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.jdo.Extent;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.jdo.Query;
import javax.jdo.Transaction;

import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.bson.types.ObjectId;
import org.datanucleus.store.rdbms.query.ForwardQueryResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.data.jdo.JAclConverter;
import com.pogeyan.cmis.impl.utils.DBUtils;

import groovy.lang.GroovyObject;

public class JDocumentObjectDAOImpl implements MDocumentObjectDAO {
	private static final Logger LOG = LoggerFactory.getLogger(JDocumentObjectDAOImpl.class);

	@Override
	public void delete(String repositoryId, String objectId, String typeId, List<String> removeProps,
			boolean forceDelete, boolean removefields, TokenImpl token) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, false);
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
				if (forceDelete) {
					pm.deletePersistent(myInstance);

				} else {
					if (removefields) {
						for (String fieldName : removeProps) {
							String fields = JDOHelper.Impl.getBaseFieldName(fieldName) != null
									? JDOHelper.Impl.getBaseFieldName(fieldName)
									: fieldName;
							String methodivoke = "set" + fields;
							myInstance.invokeMethod(methodivoke, null);
						}
					}
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
			if (pm != null) {
				tx.begin();
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, false);
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
				tx.commit();
			}

		} catch (Exception e) {
			LOG.error("update Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends IDocumentObject> getCheckOutDocs(String repositoryId, String folderId, String typeId,
			String[] principalIds, boolean aclPropagation, int maxItems, int skipCount, String orderBy) {
		try {
			Map<String, Object> fieldNames = new HashMap<>();
			Map<String, Object> principalFields = new HashMap<>();
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				if (folderId == null) {
					fieldNames.put("typeId", "cmis:document");
					fieldNames.put("isPrivateWorkingCopy", true);
				} else {
					fieldNames.put("parentId", folderId);
					fieldNames.put("typeId", "cmis:document");
					fieldNames.put("isPrivateWorkingCopy", true);
				}
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, false);
				Extent<?> QueryExtent = pm.getExtent(objectClass, true);
				Query<?> query = pm.newQuery(QueryExtent);
				query.declareVariables(
						"com.pogeyan.cmis.data.jdo.JAclImpl jacl; com.pogeyan.cmis.data.jdo.JAceImpl jace;");
				String declareParameters = null;
				String filterParameter = null;
				if (aclPropagation) {
					List<String> principalId = Stream.of(principalIds).distinct().collect(Collectors.<String>toList());
					String[] principalIdDistinct = principalId.toArray(new String[principalId.size()]);

					declareParameters = JDOHelper.Impl.getDeclareParameter(fieldNames) + ","
							+ JDOHelper.Impl.getACLDeclareParameter(principalIdDistinct)
							+ ",String path,String baseId,int tokenType";
					filterParameter = JDOHelper.Impl.getFilterParameter(fieldNames)
							+ " && this.id == jacl.baseId && this.id == jace.baseId && this.internalPath.matches(path) && this.baseId == baseId && ("
							+ JDOHelper.Impl.getACLFilterParameter(principalIdDistinct) + ")";
					principalFields = JDOHelper.Impl.getACLMap(principalIdDistinct);
				} else {
					declareParameters = JDOHelper.Impl.getDeclareParameter(fieldNames);
					filterParameter = JDOHelper.Impl.getFilterParameter(fieldNames);
				}
				Map<String, Object> fieldMaps = Stream
						.concat(fieldNames.entrySet().stream(), principalFields.entrySet().stream())
						.collect(Collectors.toMap(entry -> entry.getKey(), // The key
								entry -> entry.getValue() // The value
				));
				query.declareParameters(declareParameters);
				query.setFilter(filterParameter);
				fieldMaps.put("tokenType", TokenChangeType.DELETED.value());

				if (orderBy != null) {
					query.setOrdering(orderBy + " desc");
				}
				if (maxItems > 0) {
					query.setRange(skipCount, skipCount + maxItems);
				} else {
					query.setRange(skipCount, 0);
				}
				List<Object> result = (List<Object>) query.executeWithMap(fieldNames);
				List<String> propField = JDOHelper.Impl.getPropertyDefinition(repositoryId, typeId, new ArrayList<>());
				if (!result.isEmpty()) {
					if (!propField.isEmpty() && propField.size() > 0) {
						return JDOHelper.Impl.getDocumentObject(objectClass, result, propField);
					}
				}
				return (List<? extends IDocumentObject>) query.executeWithMap(fieldNames);
			}
		} catch (Exception e) {
			LOG.error("getCheckOutDocs Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public long getCheckOutDocsSize(String repositoryId, String folderId, String typeId, String[] principalIds,
			boolean aclPropagation) {
		Map<String, Object> fieldNames = new HashMap<>();
		Map<String, Object> principalFields = new HashMap<>();
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		try {
			if (pm != null) {
				if (folderId == null) {
					fieldNames.put("typeId", "cmis:document");
					fieldNames.put("isPrivateWorkingCopy", true);
				} else {
					fieldNames.put("parentId", folderId);
					fieldNames.put("typeId", "cmis:document");
					fieldNames.put("isPrivateWorkingCopy", true);
				}
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, false);
				Extent<?> QueryExtent = pm.getExtent(objectClass, true);
				Query<?> query = pm.newQuery(QueryExtent);
				query.declareVariables(
						"com.pogeyan.cmis.data.jdo.JAclImpl jacl; com.pogeyan.cmis.data.jdo.JAceImpl jace;");
				String declareParameters = null;
				String filterParameter = null;
				if (aclPropagation) {
					List<String> principalId = Stream.of(principalIds).distinct().collect(Collectors.<String>toList());
					String[] principalIdDistinct = principalId.toArray(new String[principalId.size()]);

					declareParameters = JDOHelper.Impl.getDeclareParameter(fieldNames) + ","
							+ JDOHelper.Impl.getACLDeclareParameter(principalIdDistinct)
							+ ",String path,String baseId,int tokenType";
					filterParameter = JDOHelper.Impl.getFilterParameter(fieldNames)
							+ " && this.id == jacl.baseId && this.id == jace.baseId && this.internalPath.matches(path) && this.baseId == baseId && ("
							+ JDOHelper.Impl.getACLFilterParameter(principalIdDistinct) + ")";
					principalFields = JDOHelper.Impl.getACLMap(principalIdDistinct);
				} else {
					declareParameters = JDOHelper.Impl.getDeclareParameter(fieldNames);
					filterParameter = JDOHelper.Impl.getFilterParameter(fieldNames);
				}
				Map<String, Object> fieldMaps = Stream
						.concat(fieldNames.entrySet().stream(), principalFields.entrySet().stream())
						.collect(Collectors.toMap(entry -> entry.getKey(), // The key
								entry -> entry.getValue() // The value
				));
				query.declareParameters(declareParameters);
				query.setFilter(filterParameter);
				fieldMaps.put("tokenType", TokenChangeType.DELETED.value());
				List<IDocumentObject> result = (List<IDocumentObject>) query.executeWithMap(fieldMaps);
				return result.size();
			}
		} catch (Exception e) {
			LOG.error("getCheckOutDocs Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return 0;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends IDocumentObject> filter(String repositoryId, Map<String, Object> fieldNames, String typeId,
			String[] mappedColumns) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, false);
				if (objectClass != null) {
					Extent<?> QueryExtent = pm.getExtent(objectClass, true);
					Query<?> query = pm.newQuery(QueryExtent);
					query.declareParameters(JDOHelper.Impl.getDeclareParameter(fieldNames));
					query.setFilter(JDOHelper.Impl.getFilterParameter(fieldNames));
					if (mappedColumns != null && mappedColumns.length > 0) {

					}
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					List<Object> result = (List<Object>) query.executeWithMap(fieldNames);
					List<String> propField = JDOHelper.Impl.getPropertyDefinition(repositoryId, typeId,
							new ArrayList<>());
					if (!result.isEmpty()) {
						return JDOHelper.Impl.getDocumentObject(objectClass, result, propField);
					}
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

	@SuppressWarnings("unused")
	@Override
	public IDocumentObject createObjectFacade(String objectId, IBaseObject baseObject, Boolean isImmutable,
			Boolean isLatestVersion, Boolean isMajorVersion, Boolean isLatestMajorVersion, Boolean isPrivateWorkingCopy,
			String versionLabel, String versionSeriesId, String versionReferenceId, Boolean isVersionSeriesCheckedOut,
			String versionSeriesCheckedOutBy, String versionSeriesCheckedOutId, String checkinComment,
			Long contentStreamLength, String contentStreamMimeType, String contentStreamFileName,
			String contentStreamId, String previousVersionObjectId) {
		try {
			Map<String, Object> JBaseObjectClassMap = new HashMap<>();
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(baseObject.getRepositoryId(),
					Arrays.asList(baseObject.getTypeId()));
			if (typeDef == null && typeDef != null && typeDef.size() == 0) {
				return null;
			}
			GroovyObject myInstance = null;
			TypeDefinition type = typeDef.get(0);
			if (type.getParentTypeId() != null) {
				Class<?> enhancedCC = JDOHelper.Impl.load(baseObject.getRepositoryId(), baseObject.getTypeId(), false);
				myInstance = (GroovyObject) enhancedCC.newInstance();
				myInstance = JDOHelper.Impl.propDefFields(baseObject.getProperties(), "set", myInstance);
				myInstance.invokeMethod("setProperties", baseObject.getProperties());
			} else {
				Class<?> enhancedCC = JDOHelper.Impl.load(baseObject.getRepositoryId(), "JDocumentObject", false);
				myInstance = (GroovyObject) enhancedCC.newInstance();
			}
			if (objectId == null) {
				objectId = (new ObjectId()).toString();
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
			myInstance.invokeMethod("setId", objectId);
			myInstance.invokeMethod("setName", baseObject.getName());
			myInstance.invokeMethod("setRepositoryId", baseObject.getRepositoryId());
			myInstance.invokeMethod("setBaseId", baseObject.getBaseId());
			myInstance.invokeMethod("setTypeId", baseObject.getTypeId());
			myInstance.invokeMethod("setSecondaryTypeIds", baseObject.getSecondaryTypeIds());
			myInstance.invokeMethod("setDescription", baseObject.getDescription());
			myInstance.invokeMethod("setCreatedBy", baseObject.getCreatedBy());
			myInstance.invokeMethod("setModifiedBy", baseObject.getModifiedBy());
			myInstance.invokeMethod("setCreatedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setModifiedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setToken", baseObject.getChangeToken());
			myInstance.invokeMethod("setInternalPath", baseObject.getInternalPath());
			myInstance.invokeMethod("setPolicies", baseObject.getPolicies());
			if (baseObject.getProperties() != null) {
				myInstance.invokeMethod("setPropString", baseObject.getProperties().toString());
			}
			JAclConverter jAcl = null;
			if (baseObject.getAcl() != null) {
				jAcl = new JAclConverter(objectId, baseObject.getAcl());
			}
			myInstance.invokeMethod("setAcl", jAcl);
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
