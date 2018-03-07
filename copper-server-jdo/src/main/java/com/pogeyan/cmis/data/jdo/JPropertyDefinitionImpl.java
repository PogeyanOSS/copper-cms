package com.pogeyan.cmis.data.jdo;

import java.util.List;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.definitions.Choice;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
@Extension(vendorName = "datanucleus", key = "read-write", value = "true")
public class JPropertyDefinitionImpl<T> implements PropertyDefinition<T> {
	private static final long serialVersionUID = 1607345349010610692L;
	private String id;
	private String localName;
	private String localNamespace;
	private String displayName;
	private String queryName;
	private String description;
	private PropertyType propertyType;
	private Cardinality cardinality;
	private Updatability updatability;
	private Boolean isInherited;
	private Boolean isRequired;
	private Boolean isQueryable;
	private Boolean isOrderable;
	private Boolean isOpenChoice;
	//private List<Choice<?>> choice;

	public JPropertyDefinitionImpl() {

	}

	public JPropertyDefinitionImpl(String id, String localName, String localNamespace, String displayName,
			String queryName, String description, PropertyType propertyType, Cardinality cardinality,
			Updatability updatability, Boolean isInherited, Boolean isRequired, Boolean isQueryable,
			Boolean isOrderable, Boolean isOpenChoice) {
		super();
		this.id = id;
		this.localName = localName;
		this.localNamespace = localNamespace;
		this.displayName = displayName;
		this.queryName = queryName;
		this.description = description;
		this.propertyType = propertyType;
		this.cardinality = cardinality;
		this.updatability = updatability;
		this.isInherited = isInherited;
		this.isRequired = isRequired;
		this.isQueryable = isQueryable;
		this.isOrderable = isOrderable;
		this.isOpenChoice = isOpenChoice;

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
	public PropertyType getPropertyType() {
		return propertyType;
	}

	@Override
	public Cardinality getCardinality() {
		return cardinality;
	}

	@Override
	public Updatability getUpdatability() {
		return updatability;
	}

	@Override
	public Boolean isInherited() {
		return this.isInherited;
	}

	@Override
	public Boolean isRequired() {
		return this.isRequired;
	}

	@Override
	public Boolean isQueryable() {
		return this.isQueryable;
	}

	@Override
	public Boolean isOrderable() {
		return this.isOrderable;
	}

	@Override
	public Boolean isOpenChoice() {
		return this.isOpenChoice;
	}

	@Override
	public List<T> getDefaultValue() {
		return null;
	}

	@Override
	public List<Choice<T>> getChoices() {
//		List<Choice<T>> choiceList = null;
//		if (choice != null) {
//			choiceList = new ArrayList<>();
//			for (Choice<?> ch : choice) {
//				ChoiceImpl<T> singleChoice = new ChoiceImpl<T>();
//				singleChoice.setDisplayName(ch.getDisplayName());
//				singleChoice.setValue((List<T>) ch.getValue());
//				List<?> childChoice = ch.getChoice();
//				singleChoice.setChoice((List<Choice<T>>) childChoice);
//				choiceList.add(singleChoice);
//			}
//		}
//
//		return choiceList;
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

	public void setPropertyType(PropertyType propertyType) {
		this.propertyType = propertyType;
	}

	public void setCardinality(Cardinality cardinality) {
		this.cardinality = cardinality;
	}

	public void setUpdatability(Updatability updatability) {
		this.updatability = updatability;
	}

	public void setIsInherited(Boolean isInherited) {
		this.isInherited = isInherited;
	}

	public void setIsRequired(Boolean isRequired) {
		this.isRequired = isRequired;
	}

	public void setIsQueryable(Boolean isQueryable) {
		this.isQueryable = isQueryable;
	}

	public void setIsOrderable(Boolean isOrderable) {
		this.isOrderable = isOrderable;
	}

	public void setIsOpenChoice(Boolean isOpenChoice) {
		this.isOpenChoice = isOpenChoice;
	}

	@SuppressWarnings({ "unchecked" })
	public void setChoice(List<?> list) {
		// this.choice = (List<Choice<?>>) list;
		//this.choice = null;
	}
}
