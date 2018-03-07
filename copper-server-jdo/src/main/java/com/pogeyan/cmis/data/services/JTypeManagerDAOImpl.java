package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;
import javax.jdo.Transaction;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;

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
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.data.jdo.JDocumentTypeObject;
import com.pogeyan.cmis.data.jdo.JPropertyDefinitionImpl;
import com.pogeyan.cmis.data.jdo.JTypeDefinition;
import com.pogeyan.cmis.data.jdo.JTypeObject;

public class JTypeManagerDAOImpl implements MTypeManagerDAO {

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends TypeDefinition> getById(String repositoryId, List<?> typeId) {
		List<TypeDefinition> typeDef = new ArrayList<>();
		PersistenceManager pm = JDOConnection.get().initializePersistenceManager(repositoryId);
		if (typeId == null) {
			Query query = pm.newQuery("SELECT FROM " + JTypeObject.class.getName());
			query.setClass(JTypeObject.class);
			List<JTypeObject> results = (List<JTypeObject>) query.execute();
			if (results.size() > 0) {
				results.forEach((k) -> {
					getTypeDef(k, typeDef);
				});
				return typeDef;
			}
		} else if (typeId.size() == 1) {
			Query query = pm.newQuery(JTypeObject.class);
			query.declareParameters("String typeId");
			query.setFilter("id == typeId");
			Map<String, String> paramValues = new HashMap<>();
			paramValues.put("typeId", typeId.get(0).toString());
			List<JTypeObject> typeObject = (List<JTypeObject>) query.executeWithMap(paramValues);
			return getTypeDef(typeObject.get(0), typeDef);
		} else {
			Query query = pm.newQuery(JTypeObject.class);
			query.declareParameters("List<String> typeId");
			query.setFilter("id = typeId");
			Map<String, List<String>> paramValues = new HashMap<>();
			paramValues.put("typeId", (List<String>) typeId);
			List<JTypeObject> results = (List<JTypeObject>) query.executeWithMap(paramValues);
			if (results.size() > 0) {
				results.forEach((k) -> {
					getTypeDef(k, typeDef);
				});
				return typeDef;
			}
		}
		return null;
	}

	@Override
	public void delete(String repositoryId, String typeId) {
		PersistenceManager pm = JDOConnection.get().initializePersistenceManager(repositoryId);
		pm.deletePersistent(typeId);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends TypeDefinition> getChildrenIds(String repositoryId, String parentId, int maxItems,
			int skipCount) {
		PersistenceManager pm = JDOConnection.get().initializePersistenceManager(repositoryId);
		List<TypeDefinition> typeDef = new ArrayList<>();
		Query query = pm.newQuery(JTypeObject.class);
		query.declareParameters("String id");
		query.setFilter("parent = id");
		Map<String, String> paramValues = new HashMap<>();
		paramValues.put("id", parentId);
		List<JTypeObject> results = (List<JTypeObject>) query.executeWithMap(paramValues);
		if (results.size() > 0) {
			results.forEach((k) -> {
				getTypeDef(k, typeDef);
			});
			return typeDef;
		}
		return null;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Map<String, PropertyDefinition<?>> getAllPropertyById(String repositoryId, String propId) {
		PersistenceManager pm = JDOConnection.get().initializePersistenceManager(repositoryId);
		Query query = pm.newQuery(JPropertyDefinitionImpl.class);
		query.declareParameters("String propID");
		query.setFilter("id == propID");
		Map<String, String> paramValues = new HashMap<>();
		paramValues.put("propID", propId);
		List<JPropertyDefinitionImpl> typeObject = (List<JPropertyDefinitionImpl>) query.executeWithMap(paramValues);
		return getPropertyDefinitions(typeObject.get(0));
	}

	@Override
	public <T extends TypeDefinition> void commit(String repositoryId, T entity) {
		PersistenceManager pm = JDOConnection.get().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			JTypeObject typeObject = null;
			if (entity.getBaseTypeId() != BaseTypeId.CMIS_DOCUMENT) {
				typeObject = new JTypeObject(entity.getId(), entity.getParentTypeId(), ((JTypeDefinition) entity), null,
						entity.getPropertyDefinitions());
			} else {
				typeObject = new JTypeObject(entity.getId(), entity.getParentTypeId(), null,
						((JDocumentTypeObject) entity), entity.getPropertyDefinitions());
			}

			pm.makePersistent(typeObject);
			tx.commit();
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

	public List<TypeDefinition> getTypeDef(JTypeObject typeObject, List<TypeDefinition> typeDef) {
		if (typeObject.getTypeDefinition() != null) {
			JTypeDefinition object = typeObject.getTypeDefinition();
			object.setPropertyDefinition(typeObject.getPropertyDefinition());
			typeDef.add(object);
		} else if (typeObject.getDocTypeDefinition() != null) {
			JDocumentTypeObject object = typeObject.getDocTypeDefinition();
			object.setPropertyDefinition(typeObject.getPropertyDefinition());
			typeDef.add(object);
		}
		return typeDef;
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

}
