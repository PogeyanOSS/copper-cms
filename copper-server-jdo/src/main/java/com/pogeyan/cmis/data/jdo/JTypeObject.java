package com.pogeyan.cmis.data.jdo;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Embedded;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.PrimaryKey;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
@Extension(vendorName = "datanucleus", key = "read-write", value = "true")
public class JTypeObject {
	@PrimaryKey
	protected String id;
	protected String parent;
	@Embedded
	protected JTypeDefinition typeDefinition;
	@Embedded
	protected JDocumentTypeObject docTypeDefinition;

	@Embedded
	protected List<JPropertyDefinitionImpl<?>> propertyDefinition;

	public JTypeObject() {
	}

	public JTypeObject(String typeId, String parent, JTypeDefinition typeDefinition,
			JDocumentTypeObject docTypeDefinition, Map<String, PropertyDefinition<?>> map) {
		this.id = typeId;
		this.parent = parent;
		this.typeDefinition = typeDefinition;
		this.docTypeDefinition = docTypeDefinition;
		this.propertyDefinition = convertJDOPropertyDefinition(map);
	}

	@SuppressWarnings("rawtypes")
	private List<JPropertyDefinitionImpl<?>> convertJDOPropertyDefinition(
			Map<String, PropertyDefinition<?>> propertyDefinition) {
		if (propertyDefinition != null) {
			List<JPropertyDefinitionImpl<?>> jdoProperty = new ArrayList<JPropertyDefinitionImpl<?>>();
			Set<Entry<String, PropertyDefinition<?>>> data = propertyDefinition.entrySet();
			for (Entry<String, PropertyDefinition<?>> propertiesValues : data) {
				PropertyDefinition<?> valueName = propertiesValues.getValue();
				JPropertyDefinitionImpl<?> jdoProp = new JPropertyDefinitionImpl();
				jdoProp.setId(valueName.getId());
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
				jdoProp.setChoice(valueName.getChoices());
				jdoProperty.add(jdoProp);
			}
			return jdoProperty;
		}
		return null;

	}

	public JTypeDefinition getTypeDefinition() {
		return typeDefinition;
	}

	public void setTypeDefinition(JTypeDefinition typeDefinition) {
		this.typeDefinition = typeDefinition;
	}

	public List<JPropertyDefinitionImpl<?>> getPropertyDefinition() {
		return propertyDefinition;
	}

	public void setPropertyDefinition(List<JPropertyDefinitionImpl<?>> propertyDefinition) {
		this.propertyDefinition = propertyDefinition;
	}

	public JDocumentTypeObject getDocTypeDefinition() {
		return docTypeDefinition;
	}

	public void setDocTypeDefinition(JDocumentTypeObject docTypeDefinition) {
		this.docTypeDefinition = docTypeDefinition;
	}

	public String getParent() {
		return parent;
	}

	public void setParent(String parent) {
		this.parent = parent;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}
}
