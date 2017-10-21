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

import java.util.Map;

import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;

import com.pogeyan.cmis.data.objects.MTypeObject;
import com.pogeyan.cmis.data.objects.MPropertyDefinition;
import com.pogeyan.cmis.data.objects.MTypeMutability;

public class MCmisDocumentTypeDefinition extends MTypeObject implements DocumentTypeDefinition {

	private static final long serialVersionUID = 1L;
	private Boolean isVersion;
	private ContentStreamAllowed contentStream;

	@Override
	public Boolean isVersionable() {
		return this.isVersion;
	}

	@Override
	public ContentStreamAllowed getContentStreamAllowed() {
		return this.contentStream;
	}

	public MCmisDocumentTypeDefinition() {

	}

	public MCmisDocumentTypeDefinition(String id, String localName, String localNamespace, String displayName,
			String queryName, String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable,
			Boolean isFileable, Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, MTypeMutability typeMutability,
			Map<String, MPropertyDefinition<?>> propertyDefinition, Boolean isVersion,
			ContentStreamAllowed contentStream) {
		super(id, localName, localNamespace, displayName, queryName, description, baseTypeId, parent, isCreatable,
				isFileable, isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery, isControllablePolicy,
				isControllableAcl, typeMutability, propertyDefinition);
		this.isVersion = isVersion;
		this.contentStream = contentStream;
	}

}
