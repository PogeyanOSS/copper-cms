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
package com.pogeyan.cmis.data.objects;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.mongodb.morphia.annotations.Embedded;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Field;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Index;
import org.mongodb.morphia.annotations.IndexOptions;
import org.mongodb.morphia.annotations.Indexes;

@Entity(value = "type", noClassnameStored = true)
@Indexes(@Index(fields = { @Field("name") }, options = @IndexOptions(unique = true)))
public class MTypeObject implements TypeDefinition {

	private static final long serialVersionUID = 1L;
	@Id
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
	protected MTypeMutability typeMutability;
	@Embedded
	protected Map<String, MPropertyDefinition<?>> propertyDefinition;

	public MTypeObject() {
	}

	public MTypeObject(String id, String localName, String localNamespace, String displayName, String queryName,
			String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable, Boolean isFileable,
			Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, MTypeMutability typeMutability,
			Map<String, MPropertyDefinition<?>> propertyDefinition) {
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

	@Override
	public Map<String, PropertyDefinition<?>> getPropertyDefinitions() {
		Map<String, PropertyDefinition<?>> map = new LinkedHashMap<String, PropertyDefinition<?>>();
		if (propertyDefinition != null) {
			Set<Entry<String, MPropertyDefinition<?>>> data = propertyDefinition.entrySet();
			for (Map.Entry<String, MPropertyDefinition<?>> propertiesValues : data) {
				String id = propertiesValues.getKey();
				MPropertyDefinition<?> valueName = propertiesValues.getValue();
				String propertyType = valueName.getPropertyType().toString();
				if (propertyType.equalsIgnoreCase("string")) {
					MCmisPropertyStringDefinition propertyValue = new MCmisPropertyStringDefinition(valueName);
					map.put(id, propertyValue);

				} else if (propertyType.equalsIgnoreCase("boolean")) {
					MCmisPropertyBooleanDefinition propertyValue = new MCmisPropertyBooleanDefinition(valueName);
					map.put(id, propertyValue);

				} else if (propertyType.equalsIgnoreCase("id")) {
					MCmisPropertyIdDefinition propertyValue = new MCmisPropertyIdDefinition(valueName);
					map.put(id, propertyValue);

				} else if (propertyType.equalsIgnoreCase("datetime")) {
					MCmisPropertyDateTimeDefinition propertyValue = new MCmisPropertyDateTimeDefinition(valueName);
					map.put(id, propertyValue);

				} else if (propertyType.equalsIgnoreCase("decimal")) {
					MCmisPropertyDecimalDefinition propertyValue = new MCmisPropertyDecimalDefinition(valueName);
					map.put(id, propertyValue);

				} else if (propertyType.equalsIgnoreCase("html")) {
					MCmisPropertyHtmlDefinition propertyValue = new MCmisPropertyHtmlDefinition(valueName);
					map.put(id, propertyValue);

				} else if (propertyType.equalsIgnoreCase("uri")) {
					MCmisPropertyUriDefinition propertyValue = new MCmisPropertyUriDefinition(valueName);
					map.put(id, propertyValue);

				} else if (propertyType.equalsIgnoreCase("integer")) {
					MCmisPropertyIntegerDefinition propertyValue = new MCmisPropertyIntegerDefinition(valueName);
					map.put(id, propertyValue);

				}
			}
		}
		return map;
	}

	@Override
	public MTypeMutability getTypeMutability() {
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

	public void setPropertyDefinition(Map<String, MPropertyDefinition<?>> propertyDefinition) {
		this.propertyDefinition = propertyDefinition;
	}

	public void setTypeMutability(MTypeMutability typeMutability) {
		this.typeMutability = typeMutability;
	}
}
