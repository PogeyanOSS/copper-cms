package com.pogeyan.cmis.data.jdo;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Embedded;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.NotPersistent;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.PrimaryKey;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

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

@Cacheable("false")
@PersistenceCapable(detachable="true")
@DatastoreIdentity(strategy=IdGeneratorStrategy.IDENTITY)
@Extension(vendorName="datanucleus", key="read-write", value="true")
public class JTypeDefinition implements TypeDefinition {
	private static final long serialVersionUID = -2058798794964755398L;
	@PrimaryKey
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

	@Embedded
	protected JTypeMutability typeMutability;
	@NotPersistent
	protected Map<String, JPropertyDefinitionImpl<?>> propertyDefinition;

	public JTypeDefinition() {
	}

	public JTypeDefinition(String id, String localName, String localNamespace, String displayName, String queryName,
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
		this.typeMutability = convertJDOTypeMutability(typeMutability);
		this.propertyDefinition = convertJDOPropertyDefinition(propertyDefinition);
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
			Set<Entry<String, JPropertyDefinitionImpl<?>>> data = propertyDefinition.entrySet();
			for (Map.Entry<String, JPropertyDefinitionImpl<?>> propertiesValues : data) {
				String id = propertiesValues.getKey();
				JPropertyDefinitionImpl<?> valueName = propertiesValues.getValue();
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
	public TypeMutabilityImpl getTypeMutability() {
		return this.typeMutability;
	}

	private JTypeMutability convertJDOTypeMutability(TypeMutabilityImpl mTypeMutability) {
		if (mTypeMutability != null) {
			JTypeMutability mongoTypeMutability = new JTypeMutability();
			mongoTypeMutability.setCanCreate(mTypeMutability.canCreate());
			mongoTypeMutability.setCanUpdate(mTypeMutability.canUpdate());
			mongoTypeMutability.setCanUpdate(mTypeMutability.canDelete());
			return mongoTypeMutability;
		}

		return null;
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

	public void setTypeMutability(JTypeMutability typeMutability) {
		this.typeMutability = typeMutability;
	}

	public void setPropertyDefinition(List<JPropertyDefinitionImpl<?>> propertyDefinition) {
		Map<String, JPropertyDefinitionImpl<?>> propDef = new HashMap<>();
		propertyDefinition.forEach((k) -> {
			propDef.put(k.getId(), k);
		});
		this.propertyDefinition = propDef;
	}

	@SuppressWarnings("rawtypes")
	private Map<String, JPropertyDefinitionImpl<?>> convertJDOPropertyDefinition(
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {
		if (propertyDefinition != null) {
			Map<String, JPropertyDefinitionImpl<?>> mongoProperty = new LinkedHashMap<String, JPropertyDefinitionImpl<?>>();
			Set<Entry<String, PropertyDefinitionImpl<?>>> data = propertyDefinition.entrySet();
			for (Entry<String, PropertyDefinitionImpl<?>> propertiesValues : data) {
				String id = propertiesValues.getKey();
				PropertyDefinitionImpl<?> valueName = propertiesValues.getValue();
				JPropertyDefinitionImpl<?> mongo = new JPropertyDefinitionImpl();
				mongo.setId(valueName.getId());
				mongo.setLocalName(valueName.getLocalName());
				mongo.setLocalNamespace(valueName.getLocalNamespace());
				mongo.setDisplayName(valueName.getDisplayName());
				mongo.setQueryName(valueName.getQueryName());
				mongo.setDescription(valueName.getDescription());
				mongo.setPropertyType(valueName.getPropertyType());
				mongo.setCardinality(valueName.getCardinality());
				mongo.setUpdatability(valueName.getUpdatability());
				mongo.setIsInherited(valueName.isInherited());
				mongo.setIsRequired(valueName.isRequired());
				mongo.setIsQueryable(valueName.isQueryable());
				mongo.setIsOrderable(valueName.isOrderable());
				mongo.setIsOpenChoice(valueName.isOpenChoice());
				mongo.setChoice(valueName.getChoices());
				mongoProperty.put(id, mongo);
			}
			return mongoProperty;
		}
		return null;

	}

}
