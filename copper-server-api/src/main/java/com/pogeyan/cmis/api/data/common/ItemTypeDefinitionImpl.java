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

import java.util.Map;

import org.apache.chemistry.opencmis.commons.definitions.ItemTypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

public class ItemTypeDefinitionImpl extends TypeDefinitionImpl implements ItemTypeDefinition {

	private static final long serialVersionUID = -5676305782058634549L;

	public ItemTypeDefinitionImpl() {
	}

	public ItemTypeDefinitionImpl(String id, String localName, String localNamespace, String displayName, String queryName,
			String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable, Boolean isFileable,
			Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutabilityImpl typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {
		super(id, localName, localNamespace, displayName, queryName, description, baseTypeId, parent, isCreatable,
				isFileable, isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery, isControllablePolicy,
				isControllableAcl, typeMutability, propertyDefinition);

	}
}
