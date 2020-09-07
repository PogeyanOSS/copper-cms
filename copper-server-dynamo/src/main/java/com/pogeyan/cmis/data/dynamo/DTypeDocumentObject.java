package com.pogeyan.cmis.data.dynamo;

import java.util.Map;

import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;

import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;


public class DTypeDocumentObject extends DTypeObject implements DocumentTypeDefinition {

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
	
	public DTypeDocumentObject() {

	}

	public DTypeDocumentObject(String id, String localName, String localNamespace, String displayName, String queryName,
			String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable, Boolean isFileable,
			Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutability typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition, Boolean isVersion,
			ContentStreamAllowed contentStream) {
		super(id, localName, localNamespace, displayName, queryName, description, baseTypeId, parent, isCreatable,
				isFileable, isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery, isControllablePolicy,
				isControllableAcl, (TypeMutabilityImpl) typeMutability, propertyDefinition);
		this.isVersion = isVersion;
		this.contentStream = contentStream;
	}
	
	
}
