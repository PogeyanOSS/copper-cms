package com.pogeyan.cmis.api.data.services;

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;

import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;

public interface DTypeManagerDAO {
	/**
	 * Returns DTypeObject values depending on TypeId
	 */
	public List<? extends TypeDefinition> getById(List<?> typeId, String[] fieldAccess);

	/**
	 * Remove DTypeObject values depending on TypeId
	 */
	public void delete(String typeId);

	/**
	 * Returns list of DTypeObject children depending on TypeId
	 */
	public List<? extends TypeDefinition> getChildrenIds(String parentId, int maxItems, int skipCount);

	public <T extends TypeDefinition> void commit(T entity);

	public TypeDefinition createObjectFacade(String id, String localName, String localNamespace, String displayName,
			String queryName, String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable,
			Boolean isFileable, Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutability typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition, Boolean isVersion,
			ContentStreamAllowed contentStream);
}