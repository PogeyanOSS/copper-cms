package com.pogeyan.cmis.data.services;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;
import javax.jdo.Transaction;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.impl.utils.DBUtils;

import groovy.lang.GroovyObject;

public class JBaseObjectDAOImpl implements MBaseObjectDAO {
	private static final Logger LOG = LoggerFactory.getLogger(JBaseObjectDAOImpl.class);

	@Override
	public IBaseObject getLatestToken(String repositoryId) {
		return null;
	}

	@Override
	public void delete(String repositoryId, String objectId, boolean forceDelete, TokenImpl token) {

	}

	@Override
	public void update(String repositoryId, String objectId, Map<String, Object> updateProps) {
	}

	@Override
	public List<? extends IBaseObject> filter(String repositoryId, Map<String, Object> fieldNames,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns) {
		List<IBaseObject> ff = new ArrayList<>();
		return ff;
	}

	@Override
	public void commit(String repositoryId, IBaseObject entity) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Transaction tx = pm.currentTransaction();
		try {
			tx.begin();
			// JTypeObject typeObject = null;
			// if (entity.getBaseTypeId() != BaseTypeId.CMIS_DOCUMENT) {
			// typeObject = new JTypeObject(entity.getId(), entity.getParentTypeId(),
			// ((JTypeDefinition) entity), null,
			// entity.getPropertyDefinitions());
			// } else {
			// typeObject = new JTypeObject(entity.getId(), entity.getParentTypeId(), null,
			// ((JDocumentTypeObject) entity), entity.getPropertyDefinitions());
			// }
			//
			// pm.makePersistent(typeObject);
			// tx.commit();
		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
		}
	}

	@SuppressWarnings({ "rawtypes", "unused", "unchecked", "resource" })
	@Override
	public IBaseObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId) {
		JTypeManagerDAOImpl typeMorphia = new JTypeManagerDAOImpl();
		Map<String, Object> JBaseObjectClassMap = new HashMap<>();
		List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(fRepositoryId, Arrays.asList(typeId));
		if (typeDef != null && typeDef.size() > 0) {
			TypeDefinition type = typeDef.get(0);
			if (type.getBaseTypeId() != BaseTypeId.CMIS_FOLDER && type.getBaseTypeId() != BaseTypeId.CMIS_DOCUMENT
					&& type.getBaseTypeId() != BaseTypeId.CMIS_ITEM && type.getBaseTypeId() != BaseTypeId.CMIS_POLICY
					&& type.getBaseTypeId() != BaseTypeId.CMIS_SECONDARY) {
				List<Map<String, Object>> listFields = getPropDefFields(type.getPropertyDefinitions());
				Map<String, Object> classMap = new HashMap<>();
				classMap.put("className", type.getId());
				classMap.put("propDef", listFields);

				// String templateString = getTemplateString(fRepositoryId, "JProperties",
				// classMap);
				// byte[] classBy =
				// JdoServiceImpl.get().compileGroovyScript("com.pogeyan.cmis.data.jdo." +
				// type.getId(),
				// templateString, JdoServiceImpl.get().getGroovyClassLoader());
				// Class cc =
				// JdoServiceImpl.get().getGroovyClassLoader().parseClass(templateString);
			} else {
				Map<String, Object> fieldValue = new HashMap<>();
				fieldValue.put("isEmbedded", false);
				JBaseObjectClassMap.put("embedded", fieldValue);
			}
		}
		Class enhancedCC = new JDOServiceImpl().load(fRepositoryId, "JBaseObject", true, JBaseObjectClassMap);
		try {
			// Class<?> class1 = Class.forName("com.pogeyan.cmis.data.jdo.JBaseObject");
			Method method = enhancedCC.getMethod("getInstance");
			GroovyObject myInstance = (GroovyObject) method.invoke(null);
			// myInstance = (GroovyObject) enhancedCC.newInstance();
			myInstance.invokeMethod("setName", name);
			myInstance.invokeMethod("setBaseId", baseId);
			myInstance.invokeMethod("setTypeId", typeId);
			myInstance.invokeMethod("getSecondaryTypeIds", secondaryTypeIds);
			myInstance.invokeMethod("setDescription", description);
			myInstance.invokeMethod("setCreatedBy", createdBy);
			myInstance.invokeMethod("setModifiedBy", modifiedBy);
			myInstance.invokeMethod("setCreatedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setModifiedAt", System.currentTimeMillis());
			myInstance.invokeMethod("setChangeToken", token);
			myInstance.invokeMethod("setInternalPath", internalPath);
			myInstance.invokeMethod("setPolicies", policies);
			myInstance.invokeMethod("setAcl", acl);
			myInstance.invokeMethod("setPath", path);
			myInstance.invokeMethod("setParentId", parentId);
			myInstance.invokeMethod("setPolicies", policies);
			Map<String, Object> fieldValue = (Map<String, Object>) JBaseObjectClassMap.get("embedded");
			if ((boolean) fieldValue.get("isEmbedded")) {

			}
			return (IBaseObject) myInstance;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;

	}

	@SuppressWarnings("unlikely-arg-type")
	private String getPropertyType(String property) {
		if (PropertyType.BOOLEAN.equals(property)) {
			return "boolean";
		} else if (PropertyType.STRING.equals(property) || PropertyType.ID.equals(property)
				|| PropertyType.URI.equals(property)) {
			return "String";
		} else if (PropertyType.INTEGER.equals(property)) {
			return "int";
		} else if (PropertyType.DATETIME.equals(property)) {
			return "long";
		}
		return property;
	}

	private List<Map<String, Object>> getPropDefFields(Map<String, PropertyDefinition<?>> propDef) {
		List<Map<String, Object>> listOfPropDef = new ArrayList<>();
		propDef.forEach((key, value) -> {
			Map<String, Object> propDetails = new HashMap<>();
			propDetails.put("id", value.getId());
			propDetails.put("property", getPropertyType(value.getPropertyType().toString()));
			if (value.getLocalName().equalsIgnoreCase("primary")) {
				propDetails.put("primary", true);
			} else {
				propDetails.put("primary", false);
			}
			listOfPropDef.add(propDetails);
		});
		return listOfPropDef;
	}

}
