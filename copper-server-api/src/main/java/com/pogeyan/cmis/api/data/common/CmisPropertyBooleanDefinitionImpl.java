/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.api.data.common;

import java.util.ArrayList;
import java.util.List;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.definitions.Choice;
import org.apache.chemistry.opencmis.commons.definitions.PropertyBooleanDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChoiceImpl;

public class CmisPropertyBooleanDefinitionImpl<T> implements PropertyBooleanDefinition {

	private static final long serialVersionUID = 1L;
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
	private List<?> choice;

	public CmisPropertyBooleanDefinitionImpl() {
		super();
	}

	public CmisPropertyBooleanDefinitionImpl(PropertyDefinition<?> type) {
		super();
		this.id = type.getId();
		this.localName = type.getLocalName();
		this.localNamespace = type.getLocalNamespace();
		this.displayName = type.getDisplayName();
		this.queryName = type.getQueryName();
		this.description = type.getDescription();
		this.propertyType = type.getPropertyType();
		this.cardinality = type.getCardinality();
		this.updatability = type.getUpdatability();
		this.isInherited = type.isInherited();
		this.isRequired = type.isRequired();
		this.isQueryable = type.isQueryable();
		this.isOrderable = type.isOrderable();
		this.isOpenChoice = type.isOpenChoice();
		this.choice = type.getChoices();
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
		return this.propertyType;
	}

	@Override
	public Cardinality getCardinality() {
		return this.cardinality;
	}

	@Override
	public Updatability getUpdatability() {
		return this.updatability;
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
	public List<Boolean> getDefaultValue() {
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Choice<Boolean>> getChoices() {
		List<Choice<Boolean>> choiceList = null;
		if (choice != null) {
			choiceList = new ArrayList<>();
			for (Object obj : choice) {
				Choice<?> ch = (Choice<?>) obj;
				ChoiceImpl<Boolean> singleChoice = new ChoiceImpl<Boolean>();
				singleChoice.setDisplayName(ch.getDisplayName());
				singleChoice.setValue((List<Boolean>) ch.getValue());
				List<?> childChoice = ch.getChoice();
				singleChoice.setChoice((List<Choice<Boolean>>) childChoice);
				choiceList.add(singleChoice);
			}
		}
		return choiceList;
	}

	@Override
	public List<CmisExtensionElement> getExtensions() {
		return null;
	}

	@Override
	public void setExtensions(List<CmisExtensionElement> extensions) {
	}
}
