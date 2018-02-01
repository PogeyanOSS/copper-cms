package com.pogeyan.cmis.api.data.common;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

public class TypeDefinitionImpl implements TypeDefinition {
	private static final long serialVersionUID = 1L;
	protected String id;
	protected String localName;
	protected String localNamespace;
	protected String displayName;
	protected String queryName;
	protected String description;
	protected BaseTypeId baseTypeId;
	protected String parent;
	protected Boolean isCreatable;
	protected Boolean isFileable;
	protected Boolean isQueryable;
	protected Boolean isFulltextIndexed;
	protected Boolean isIncludedInSupertypeQuery;
	protected Boolean isControllablePolicy;
	protected Boolean isControllableAcl;

	protected TypeMutabilityImpl typeMutability;
	protected Map<String, PropertyDefinitionImpl<?>> propertyDefinition;

	public TypeDefinitionImpl() {
	}

	public TypeDefinitionImpl(String id, String localName, String localNamespace, String displayName, String queryName,
			String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable, Boolean isFileable,
			Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutabilityImpl typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {
		super();
		this.id = id;
		this.localName = localName;
		this.localNamespace = localNamespace;
		this.displayName = displayName;
		this.queryName = queryName;
		this.description = description;
		this.baseTypeId = baseTypeId;
		this.parent = parent;
		this.isCreatable = isCreatable;
		this.isFileable = isFileable;
		this.isQueryable = isQueryable;
		this.isFulltextIndexed = isFulltextIndexed;
		this.isIncludedInSupertypeQuery = isIncludedInSupertypeQuery;
		this.isControllablePolicy = isControllablePolicy;
		this.isControllableAcl = isControllableAcl;
		this.typeMutability = typeMutability;
		this.propertyDefinition = propertyDefinition;
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
		return this.id;
	}

	@Override
	public String getLocalName() {
		return this.localName;
	}

	@Override
	public String getLocalNamespace() {
		return this.localNamespace;
	}

	@Override
	public String getDisplayName() {
		return this.displayName;
	}

	@Override
	public String getQueryName() {
		return this.queryName;
	}

	@Override
	public String getDescription() {
		return this.description;
	}

	@Override
	public BaseTypeId getBaseTypeId() {
		return baseTypeId;
	}

	@Override
	public String getParentTypeId() {
		return this.parent;
	}

	@Override
	public Boolean isCreatable() {
		return this.isCreatable;
	}

	@Override
	public Boolean isFileable() {
		return this.isFileable;
	}

	@Override
	public Boolean isQueryable() {
		return this.isQueryable;
	}

	@Override
	public Boolean isFulltextIndexed() {
		return this.isFulltextIndexed;
	}

	@Override
	public Boolean isIncludedInSupertypeQuery() {
		return this.isIncludedInSupertypeQuery;
	}

	@Override
	public Boolean isControllablePolicy() {
		return this.isControllablePolicy;
	}

	@Override
	public Boolean isControllableAcl() {
		return this.isControllableAcl;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Map<String, PropertyDefinition<?>> getPropertyDefinitions() {
		Map<String, PropertyDefinition<?>> map = new LinkedHashMap<String, PropertyDefinition<?>>();
		if (propertyDefinition != null) {
			Set<Entry<String, PropertyDefinitionImpl<?>>> data = propertyDefinition.entrySet();
			for (Map.Entry<String, ? extends PropertyDefinition<?>> propertiesValues : data) {
				String id = propertiesValues.getKey();
				PropertyDefinitionImpl<?> valueName = (PropertyDefinitionImpl<?>) propertiesValues.getValue();
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

	@Override
	public TypeMutability getTypeMutability() {
		return this.typeMutability;
	}

	public void setId(String id) {
		this.id = id;
	}

	public void setLocalName(String localName) {
		this.localName = localName;
	}

	public void setLocalNamespace(String localNamespace) {
		this.localNamespace = localNamespace;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public void setQueryName(String queryName) {
		this.queryName = queryName;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void setBaseTypeId(BaseTypeId baseTypeId) {
		this.baseTypeId = baseTypeId;
	}

	public void setParentTypeId(String parent) {
		this.parent = parent;
	}

	public void setIsCreatable(Boolean isCreatable) {
		this.isCreatable = isCreatable;
	}

	public void setIsFileable(Boolean isFileable) {
		this.isFileable = isFileable;
	}

	public void setIsQueryable(Boolean isQueryable) {
		this.isQueryable = isQueryable;
	}

	public void setIsFulltextIndexed(Boolean isFulltextIndexed) {
		this.isFulltextIndexed = isFulltextIndexed;
	}

	public void setIsIncludedInSupertypeQuery(Boolean isIncludedInSupertypeQuery) {
		this.isIncludedInSupertypeQuery = isIncludedInSupertypeQuery;
	}

	public void setIsControllablePolicy(Boolean isControllablePolicy) {
		this.isControllablePolicy = isControllablePolicy;
	}

	public void setIsControllableAcl(Boolean isControllableAcl) {
		this.isControllableAcl = isControllableAcl;
	}

	public void setPropertyDefinition(Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {
		this.propertyDefinition = propertyDefinition;
	}

	public void setTypeMutability(TypeMutabilityImpl typeMutability) {
		this.typeMutability = typeMutability;
	}
}
