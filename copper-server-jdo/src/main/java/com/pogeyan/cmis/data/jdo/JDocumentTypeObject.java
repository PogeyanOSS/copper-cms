package com.pogeyan.cmis.data.jdo;

import java.util.Map;
import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.Inheritance;
import javax.jdo.annotations.InheritanceStrategy;

import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
@Inheritance(strategy = InheritanceStrategy.NEW_TABLE)
public class JDocumentTypeObject extends JTypeDefinition implements DocumentTypeDefinition {
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

	public JDocumentTypeObject() {

	}

	public JDocumentTypeObject(String id, String localName, String localNamespace, String displayName, String queryName,
			String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable, Boolean isFileable,
			Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutabilityImpl typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition, Boolean isVersion,
			ContentStreamAllowed contentStream) {
		super(id, localName, localNamespace, displayName, queryName, description, baseTypeId, parent, isCreatable,
				isFileable, isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery, isControllablePolicy,
				isControllableAcl, (TypeMutabilityImpl) typeMutability, propertyDefinition);
		this.isVersion = isVersion;
		this.contentStream = contentStream;
	}

	public void setIsVersion(Boolean isVersion) {
		this.isVersion = isVersion;
	}

	public void setContentStream(ContentStreamAllowed contentStream) {
		this.contentStream = contentStream;
	}
}
