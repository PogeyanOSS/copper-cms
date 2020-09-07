package com.pogeyan.cmis.data.dynamo;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
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

public class DTypeObject implements TypeDefinition {

	private static final long serialVersionUID = 1L;

	private String pk;
	private String sk;
	private Map<String, Object> attributes = new HashMap<String, Object>();

	public DTypeObject() {
	}

	public DTypeObject(String pk, String sk, Map<String, Object> attributes) {
		this.pk = pk;
		this.sk = sk;
		this.attributes = attributes;
	}

	public DTypeObject(String id, String localName, String localNamespace, String displayName, String queryName,
			String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable, Boolean isFileable,
			Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutabilityImpl typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {

		pk = id;
		sk = localName;
		setAttributes(localNamespace, displayName, queryName, description, baseTypeId, parent, isCreatable, isFileable,
				isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery, isControllablePolicy, isControllableAcl,
				typeMutability, propertyDefinition);

	}

	public void setAttributes(String localNamespace, String displayName, String queryName, String description,
			BaseTypeId baseTypeId, String parent, Boolean isCreatable, Boolean isFileable, Boolean isQueryable,
			Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery, Boolean isControllablePolicy,
			Boolean isControllableAcl, TypeMutabilityImpl typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {

		attributes.put("localNamespace", localNamespace);
		attributes.put("displayName", displayName);
		attributes.put("queryName", queryName);
		attributes.put("description", description);
		attributes.put("baseTypeId", baseTypeId.value());
		attributes.put("parent", parent);
		attributes.put("isCreatable", isCreatable);
		attributes.put("isFileable", isFileable);
		attributes.put("isQueryable", isQueryable);
		attributes.put("isFulltextIndexed", isFulltextIndexed);
		attributes.put("isIncludedInSupertypeQuery", isIncludedInSupertypeQuery);
		attributes.put("isControllablePolicy", isControllablePolicy);
		attributes.put("isControllableAcl", isControllableAcl);
		attributes.put("typeMutability", convertDynamoTypeMutability(typeMutability));
		attributes.put("propertyDefinition", convertDynamoPropertyDefinition(propertyDefinition));
	}

	private Map<String, Boolean> convertDynamoTypeMutability(TypeMutabilityImpl mTypeMutability) {

		if (mTypeMutability != null) {
			Map<String, Boolean> dynamoTypeMutability = new HashMap<String, Boolean>();
			// Map<String, Map<String,Boolean>> dynamoTypeMutability = new
			// HashMap<String,Object>();

			dynamoTypeMutability.put("canCreate", mTypeMutability.canCreate());
			dynamoTypeMutability.put("canUpdata", mTypeMutability.canUpdate());
			dynamoTypeMutability.put("canDelete", mTypeMutability.canDelete());

			return dynamoTypeMutability;
		}

		return null;
	}

	private Map<String, Object> convertDynamoPropertyDefinition(
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {
		if (propertyDefinition != null) {
			Map<String, Object> dynamoProperty = new HashMap<String, Object>();
			Set<Entry<String, PropertyDefinitionImpl<?>>> data = propertyDefinition.entrySet();
			for (Entry<String, PropertyDefinitionImpl<?>> propertiesValues : data) {
				String id = propertiesValues.getKey();
				PropertyDefinitionImpl<?> valueName = propertiesValues.getValue();
				Map<String, Object> dynamo = new HashMap<String, Object>();
				dynamo.put("id", valueName.getId());
				dynamo.put("localName", valueName.getLocalName());
				dynamo.put("displayName", valueName.getDisplayName());
				dynamo.put("queryName", valueName.getQueryName());
				dynamo.put("description", valueName.getDescription());
				dynamo.put("propertyType", valueName.getPropertyType().value());
				dynamo.put("cardinality", valueName.getCardinality().value());
				dynamo.put("updatability", valueName.getUpdatability().value());
				dynamo.put("isInherited", valueName.isInherited());
				dynamo.put("isRequired", valueName.isRequired());
				dynamo.put("isQueryable", valueName.isQueryable());
				dynamo.put("isOrderable", valueName.isOrderable());
				dynamo.put("isOpenChoice", valueName.isOpenChoice());
				dynamo.put("choice", valueName.getChoices());
				dynamo.put("minValue", valueName.getMinValue() != null ? valueName.getMinValue().intValue() : null);
				dynamo.put("maxValue", valueName.getMaxValue() != null ? valueName.getMaxValue().intValue() : null);
				dynamo.put("maxLength", valueName.getMaxLength() != null ? valueName.getMaxLength().intValue() : null);
				dynamoProperty.put(id, dynamo);
			}
			return dynamoProperty;
		}
		return null;

	}

	public String getPk() {
		return pk;
	}

	public String getSk() {
		return sk;
	}

	public Map<String, Object> getAttributes() {
		return attributes;
	}

	public void setPk(String pk) {
		this.pk = pk;
	}

	public void setSk(String sk) {
		this.sk = sk;
	}

	@Override
	public List<CmisExtensionElement> getExtensions() {
		return null;
	}

	@Override
	public void setExtensions(List<CmisExtensionElement> extensions) {
	}

	@Override
	public String getId() {
		return pk;
	}

	@Override
	public String getLocalName() {
		return sk;
	}

	@Override
	public String getLocalNamespace() {
		return (String) attributes.get("localName");
	}

	@Override
	public String getDisplayName() {
		return (String) attributes.get("displayName");
	}

	@Override
	public String getQueryName() {
		return (String) attributes.get("queryName");
	}

	@Override
	public String getDescription() {
		return (String) attributes.get("description");
	}

	@Override
	public BaseTypeId getBaseTypeId() {
		return BaseTypeId.fromValue((String) attributes.get("baseTypeId"));
	}

	@Override
	public String getParentTypeId() {
		return (String) attributes.get("parent");
	}

	@Override
	public Boolean isCreatable() {
		return (Boolean) attributes.get("isCreatable");
	}

	@Override
	public Boolean isFileable() {
		return (Boolean) attributes.get("isFileable");
	}

	@Override
	public Boolean isQueryable() {
		return (Boolean) attributes.get("isQueryable");
	}

	@Override
	public Boolean isFulltextIndexed() {
		return (Boolean) attributes.get("isFulltextIndexed");
	}

	@Override
	public Boolean isIncludedInSupertypeQuery() {
		return (Boolean) attributes.get("isIncludedInSupertypeQuery");
	}

	@Override
	public Boolean isControllablePolicy() {
		return (Boolean) attributes.get("isControllablePolicy");
	}

	@Override
	public Boolean isControllableAcl() {
		return (Boolean) attributes.get("isControllableAcl");
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Map<String, PropertyDefinition<?>> getPropertyDefinitions() {
		Map<String, PropertyDefinition<?>> map = new LinkedHashMap<String, PropertyDefinition<?>>();
		Map<String, DynamoPropertyDefinition<?>> propertyDefinition = (Map<String, DynamoPropertyDefinition<?>>) attributes
				.get("propertyDefinition");
		if (propertyDefinition != null) {
			Set<Entry<String, DynamoPropertyDefinition<?>>> data = propertyDefinition.entrySet();
			for (Map.Entry<String, DynamoPropertyDefinition<?>> propertiesValues : data) {
				String id = propertiesValues.getKey();
				ObjectMapper mapper = new ObjectMapper();
				SimpleModule module = new SimpleModule();
				module.addDeserializer(Cardinality.class, new CardinalityEnumDeserializer(Cardinality.class));
				module.addDeserializer(Updatability.class, new UpdatabilityEnumDeserializer(Updatability.class));
				module.addDeserializer(PropertyType.class, new PropertyTypeEnumDeserializer(PropertyType.class));
				mapper.registerModule(module);
				PropertyDefinitionImpl<?> valueName = mapper.convertValue(propertiesValues.getValue(), PropertyDefinitionImpl.class);
//				PropertyDefinitionImpl<?> valueName = propertiesValues.getValue();
				String propertyType = valueName.getPropertyType().toString();
				if (propertyType.equalsIgnoreCase("string")) {
					CmisPropertyStringDefinitionImpl propertyValue = new CmisPropertyStringDefinitionImpl(valueName);
					map.put(id, propertyValue);
				} else if (propertyType.equalsIgnoreCase("boolean")) {
					CmisPropertyBooleanDefinitionImpl propertyValue = new CmisPropertyBooleanDefinitionImpl(valueName);
					map.put(id, propertyValue);
				} else if (propertyType.equalsIgnoreCase("id")) {
					CmisPropertyIdDefinitionImpl propertyValue = new CmisPropertyIdDefinitionImpl(valueName);
					map.put(id, propertyValue);
				} else if (propertyType.equalsIgnoreCase("datetime")) {
					CmisPropertyDateTimeDefinitionImpl propertyValue = new CmisPropertyDateTimeDefinitionImpl(
							valueName);
					map.put(id, propertyValue);
				} else if (propertyType.equalsIgnoreCase("decimal")) {
					CmisPropertyDecimalDefinitionImpl propertyValue = new CmisPropertyDecimalDefinitionImpl(valueName);
					map.put(id, propertyValue);
				} else if (propertyType.equalsIgnoreCase("html")) {
					CmisPropertyHtmlDefinitionImpl propertyValue = new CmisPropertyHtmlDefinitionImpl(valueName);
					map.put(id, propertyValue);
				} else if (propertyType.equalsIgnoreCase("uri")) {
					CmisPropertyUriDefinitionImpl propertyValue = new CmisPropertyUriDefinitionImpl(valueName);
					map.put(id, propertyValue);
				} else if (propertyType.equalsIgnoreCase("integer")) {
					CmisPropertyIntegerDefinitionImpl propertyValue = new CmisPropertyIntegerDefinitionImpl(valueName);
					map.put(id, propertyValue);
				}
			}
		}
		return map;

	}

	@SuppressWarnings("unchecked")
	@Override
	public TypeMutability getTypeMutability() {
		Map<String, Boolean> tm = (Map<String, Boolean>) attributes.get("typeMutability");
		DynamoTypeMutability dt = new DynamoTypeMutability();
		dt.setCanCreate(tm.get("canCreate"));
		dt.setCanDelete(tm.get("canDelete"));
		dt.setCanUpdate(tm.get("canUpdate"));
		return dt;
	}

}
