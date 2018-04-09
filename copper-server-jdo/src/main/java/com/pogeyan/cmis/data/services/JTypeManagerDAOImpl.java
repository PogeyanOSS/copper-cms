package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.jdo.PersistenceManager;
import javax.jdo.Query;
import javax.jdo.Transaction;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import org.bson.types.ObjectId;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.CmisPropertyBooleanDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPropertyDateTimeDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPropertyDecimalDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPropertyHtmlDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPropertyIdDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPropertyIntegerDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPropertyStringDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPropertyUriDefinitionImpl;
import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.data.jdo.JDocumentTypeObject;
import com.pogeyan.cmis.data.jdo.JPropertyDefinitionImpl;
import com.pogeyan.cmis.data.jdo.JTypeDefinition;
import com.pogeyan.cmis.data.jdo.JTypeMutability;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;

import groovy.lang.GroovyObject;

public class JTypeManagerDAOImpl implements MTypeManagerDAO {

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<? extends TypeDefinition> getById(String repositoryId, List<?> typeId) {
		List<TypeDefinition> typeDef = new ArrayList<>();
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		if (typeId == null) {
			Query query = pm.newQuery("SELECT FROM " + JTypeDefinition.class.getName());
			query.setClass(JTypeDefinition.class);
			List<JTypeDefinition> results = (List<JTypeDefinition>) query.execute();
			if (results.size() > 0) {
				results.forEach((k) -> {
					getTypeDef(k, typeDef);
				});
				return typeDef;
			}
		} else if (typeId.size() == 1) {
			Query query = pm.newQuery("SELECT FROM " + JTypeDefinition.class.getName());
			query.declareParameters("String typeId");
			query.setFilter("this.id == typeId");
			Map<String, String> paramValues = new HashMap<>();
			paramValues.put("typeId", typeId.get(0).toString().trim());
			List<JTypeDefinition> typeObject = (List<JTypeDefinition>) query.executeWithMap(paramValues);
			if (typeObject.size() > 0) {
				return getTypeDef(typeObject.get(0), typeDef);
			}
			return new ArrayList<>();
		} else {
			Query query = pm.newQuery("SELECT FROM " + JTypeDefinition.class);
			query.declareParameters("List<String> typeId");
			query.setFilter("this.id == typeId");
			Map<String, List<String>> paramValues = new HashMap<>();
			paramValues.put("typeId", (List<String>) typeId);
			List<JTypeDefinition> results = (List<JTypeDefinition>) query.executeWithMap(paramValues);
			if (results.size() > 0) {
				results.forEach((k) -> {
					getTypeDef(k, typeDef);
				});
				return typeDef;
			}
		}
		return new ArrayList<>();
	}

	@Override
	public void delete(String repositoryId, String typeId) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			List<? extends TypeDefinition> type = getById(repositoryId, Arrays.asList(typeId));
			if (type.size() > 0) {
				TypeDefinition typeDef = type.get(0);
				pm.deletePersistent(typeDef);
			}
			tx.commit();
		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<? extends TypeDefinition> getChildrenIds(String repositoryId, String parentId, int maxItems,
			int skipCount) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		List<TypeDefinition> typeDef = new ArrayList<>();
		Query query = pm.newQuery(JTypeDefinition.class);
		query.declareParameters("String id");
		query.setFilter("this.parent == id");
		Map<String, String> paramValues = new HashMap<>();
		paramValues.put("id", parentId);
		List<JTypeDefinition> results = (List<JTypeDefinition>) query.executeWithMap(paramValues);
		if (results.size() > 0) {
			results.forEach((k) -> {
				getTypeDef(k, typeDef);
			});
			return typeDef;
		}
		return new ArrayList<>();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Map<String, PropertyDefinition<?>> getAllPropertyById(String repositoryId, String propId) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Query query = pm.newQuery(JPropertyDefinitionImpl.class);
		query.declareParameters("String propID");
		query.setFilter("this.propDefId == propID");
		Map<String, String> paramValues = new HashMap<>();
		paramValues.put("propID", propId);
		List<JPropertyDefinitionImpl> typeObject = (List<JPropertyDefinitionImpl>) query.executeWithMap(paramValues);
		if (typeObject.size() > 0) {
			return getPropertyDefinitions(typeObject.get(0));
		}
		return new HashMap<>();
	}

	@Override
	public <T extends TypeDefinition> void commit(String repositoryId, T entity) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			List<? extends TypeDefinition> typeDef = getById(repositoryId, Arrays.asList(entity.getId()));
			if (typeDef != null && typeDef.isEmpty()) {
				pm.makePersistent(entity);
				tx.commit();
			} else {
				if (entity.getBaseTypeId().equals(BaseTypeId.CMIS_DOCUMENT)) {
					JDocumentTypeObject docEntity = (JDocumentTypeObject) entity;
					JDocumentTypeObject jDocTypeDefinition = (JDocumentTypeObject) typeDef.get(0);
					List<JPropertyDefinitionImpl<?>> prop = convertListJDOPropertyDefinition(
							entity.getPropertyDefinitions());
					jDocTypeDefinition.setListPropertyDefinition(prop);
					jDocTypeDefinition.setDescription(entity.getDescription());
					jDocTypeDefinition.setDisplayName(entity.getDescription());
					jDocTypeDefinition.setLocalName(entity.getLocalName());
					jDocTypeDefinition.setLocalNamespace(entity.getLocalNamespace());
					jDocTypeDefinition.setIsControllableAcl(entity.isControllableAcl());
					jDocTypeDefinition.setIsControllablePolicy(entity.isControllablePolicy());
					jDocTypeDefinition.setIsCreatable(entity.isCreatable());
					jDocTypeDefinition.setIsFileable(entity.isFileable());
					jDocTypeDefinition.setIsFulltextIndexed(entity.isFulltextIndexed());
					jDocTypeDefinition.setIsIncludedInSupertypeQuery(entity.isIncludedInSupertypeQuery());
					jDocTypeDefinition.setIsQueryable(entity.isQueryable());
					jDocTypeDefinition.setIsVersion(docEntity.isVersionable());
					jDocTypeDefinition.setContentStream(docEntity.getContentStreamAllowed());
					jDocTypeDefinition.setParentTypeId(entity.getParentTypeId());
					jDocTypeDefinition.setTypeMutability(convertJDOTypeMutability(entity.getTypeMutability()));
					jDocTypeDefinition.setModifiedAt(System.currentTimeMillis());
					tx.commit();
				} else {
					JTypeDefinition jTypeDefinition = (JTypeDefinition) typeDef.get(0);
					List<JPropertyDefinitionImpl<?>> prop = convertListJDOPropertyDefinition(
							entity.getPropertyDefinitions());
					jTypeDefinition.setListPropertyDefinition(prop);
					jTypeDefinition.setDescription(entity.getDescription());
					jTypeDefinition.setDisplayName(entity.getDescription());
					jTypeDefinition.setLocalName(entity.getLocalName());
					jTypeDefinition.setLocalNamespace(entity.getLocalNamespace());
					jTypeDefinition.setIsControllableAcl(entity.isControllableAcl());
					jTypeDefinition.setIsControllablePolicy(entity.isControllablePolicy());
					jTypeDefinition.setIsCreatable(entity.isCreatable());
					jTypeDefinition.setIsFileable(entity.isFileable());
					jTypeDefinition.setIsFulltextIndexed(entity.isFulltextIndexed());
					jTypeDefinition.setIsIncludedInSupertypeQuery(entity.isIncludedInSupertypeQuery());
					jTypeDefinition.setIsQueryable(entity.isQueryable());
					jTypeDefinition.setParentTypeId(entity.getParentTypeId());
					jTypeDefinition.setTypeMutability(convertJDOTypeMutability(entity.getTypeMutability()));
					jTypeDefinition.setModifiedAt(System.currentTimeMillis());
					tx.commit();
				}
				commitObjectClass(repositoryId, entity);
			}

		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
		}
	}

	@Override
	public TypeDefinition createObjectFacade(String id, String localName, String localNamespace, String displayName,
			String queryName, String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable,
			Boolean isFileable, Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutability typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition, Boolean isVersion,
			ContentStreamAllowed contentStream) {
		if (isVersion != null || contentStream != null) {
			return new JDocumentTypeObject(id, localName, localNamespace, displayName, queryName, description,
					baseTypeId, parent, isCreatable, isFileable, isQueryable, isFulltextIndexed,
					isIncludedInSupertypeQuery, isControllablePolicy, isControllableAcl,
					(TypeMutabilityImpl) typeMutability, propertyDefinition, isVersion, contentStream);
		} else {
			return new JTypeDefinition(id, localName, localNamespace, displayName, queryName, description, baseTypeId,
					parent, isCreatable, isFileable, isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery,
					isControllablePolicy, isControllableAcl, (TypeMutabilityImpl) typeMutability, propertyDefinition);
		}
	}

	public List<TypeDefinition> getTypeDef(JTypeDefinition typeObject, List<TypeDefinition> typeDef) {
		typeObject.setPropertyDefinition(typeObject.getListPropertyDefinition());
		typeDef.add(typeObject);
		return typeDef;

	}

	@SuppressWarnings("rawtypes")
	private List<JPropertyDefinitionImpl<?>> convertListJDOPropertyDefinition(
			Map<String, PropertyDefinition<?>> propertyDefinition) {
		if (propertyDefinition != null) {
			List<JPropertyDefinitionImpl<?>> jdoProperty = new ArrayList<JPropertyDefinitionImpl<?>>();
			Set<Entry<String, PropertyDefinition<?>>> data = propertyDefinition.entrySet();
			for (Entry<String, PropertyDefinition<?>> propertiesValues : data) {
				PropertyDefinition<?> valueName = propertiesValues.getValue();
				JPropertyDefinitionImpl<?> jdoProp = new JPropertyDefinitionImpl();
				jdoProp.setId(valueName.getId());
				jdoProp.setPropId((new ObjectId()).toString());
				jdoProp.setLocalName(valueName.getLocalName());
				jdoProp.setLocalNamespace(valueName.getLocalNamespace());
				jdoProp.setDisplayName(valueName.getDisplayName());
				jdoProp.setQueryName(valueName.getQueryName());
				jdoProp.setDescription(valueName.getDescription());
				jdoProp.setPropertyType(valueName.getPropertyType());
				jdoProp.setCardinality(valueName.getCardinality());
				jdoProp.setUpdatability(valueName.getUpdatability());
				jdoProp.setIsInherited(valueName.isInherited());
				jdoProp.setIsRequired(valueName.isRequired());
				jdoProp.setIsQueryable(valueName.isQueryable());
				jdoProp.setIsOrderable(valueName.isOrderable());
				jdoProp.setIsOpenChoice(valueName.isOpenChoice());
				jdoProp.setChoice(JDOHelper.Impl.setChoice(valueName.getChoices()));
				jdoProperty.add(jdoProp);
			}
			return jdoProperty;
		}
		return null;

	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public Map<String, PropertyDefinition<?>> getPropertyDefinitions(JPropertyDefinitionImpl valueName) {
		Map<String, PropertyDefinition<?>> map = new LinkedHashMap<String, PropertyDefinition<?>>();
		String propertyType = valueName.getPropertyType().toString();
		if (propertyType.equalsIgnoreCase("string")) {
			CmisPropertyStringDefinitionImpl propertyValue = new CmisPropertyStringDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		} else if (propertyType.equalsIgnoreCase("boolean")) {
			CmisPropertyBooleanDefinitionImpl propertyValue = new CmisPropertyBooleanDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		} else if (propertyType.equalsIgnoreCase("id")) {
			CmisPropertyIdDefinitionImpl propertyValue = new CmisPropertyIdDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		} else if (propertyType.equalsIgnoreCase("datetime")) {
			CmisPropertyDateTimeDefinitionImpl propertyValue = new CmisPropertyDateTimeDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		} else if (propertyType.equalsIgnoreCase("decimal")) {
			CmisPropertyDecimalDefinitionImpl propertyValue = new CmisPropertyDecimalDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		} else if (propertyType.equalsIgnoreCase("html")) {
			CmisPropertyHtmlDefinitionImpl propertyValue = new CmisPropertyHtmlDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		} else if (propertyType.equalsIgnoreCase("uri")) {
			CmisPropertyUriDefinitionImpl propertyValue = new CmisPropertyUriDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		} else if (propertyType.equalsIgnoreCase("integer")) {
			CmisPropertyIntegerDefinitionImpl propertyValue = new CmisPropertyIntegerDefinitionImpl(valueName);
			map.put(valueName.getId(), propertyValue);
		}
		return map;
	}

	private JTypeMutability convertJDOTypeMutability(TypeMutability typeMutability) {
		if (typeMutability != null) {
			JTypeMutability mongoTypeMutability = new JTypeMutability();
			mongoTypeMutability.setCanCreate(typeMutability.canCreate());
			mongoTypeMutability.setCanUpdate(typeMutability.canUpdate());
			mongoTypeMutability.setCanUpdate(typeMutability.canDelete());
			return mongoTypeMutability;
		}

		return null;
	}

	private <T extends TypeDefinition> void commitObjectClass(String repositoryId, T entity) {
		try {
			boolean base = true;
			MBaseObjectDAO objectDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
					MBaseObjectDAO.class);
			MDocumentTypeManagerDAO docManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
			MDocumentObjectDAO documentMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentObjectDAO.class);
			CacheProviderServiceFactory.getTypeCacheServiceProvider().remove(repositoryId, entity.getId());
			if (entity.getBaseTypeId().equals(BaseTypeId.CMIS_DOCUMENT)) {
				base = false;
				CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, entity.getId(),
						docManagerDAO.getByTypeId(repositoryId, entity.getId()));
				String className = JDOHelper.Impl.getJDOTypeId(entity.getId(), base);
				if (!className.equalsIgnoreCase("JBaseObject") && !className.equalsIgnoreCase("JDocumentObject")) {
					JDOServiceImpl.getInstance().remove(repositoryId, "com.pogeyan.cmis.data.jdo." + entity.getId());
					Class<?> enhancedCC = JDOHelper.Impl.load(repositoryId, entity.getId(), base);
					GroovyObject myInstance = (GroovyObject) enhancedCC.newInstance();
					myInstance.invokeMethod("setId", (new ObjectId()).toString());
					documentMorphiaDAO.commit(repositoryId, (IDocumentObject) myInstance);
				}
			} else {
				CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, entity.getId(), entity);
				String className = JDOHelper.Impl.getJDOTypeId(entity.getId(), base);
				if (!className.equalsIgnoreCase("JBaseObject") && !className.equalsIgnoreCase("JDocumentObject")) {
					JDOServiceImpl.getInstance().remove(repositoryId, "com.pogeyan.cmis.data.jdo." + entity.getId());
					Class<?> enhancedCC = JDOHelper.Impl.load(repositoryId, entity.getId(), base);
					GroovyObject myInstance = (GroovyObject) enhancedCC.newInstance();
					myInstance.invokeMethod("setId", (new ObjectId()).toString());
					objectDAO.commit(repositoryId, (IBaseObject) myInstance);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
