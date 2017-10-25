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
package com.pogeyan.cmis.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.FolderTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.ItemTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PolicyTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.RelationshipTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.SecondaryTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionContainer;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConstraintException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractPropertyDefinition;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractTypeDefinition;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.DocumentTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.FolderTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ItemTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PolicyTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RelationshipTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.SecondaryTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.TypeMutabilityImpl;
import org.apache.chemistry.opencmis.server.support.TypeManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;

/**
 * A helper class doing some consistency checks when new type definitions are
 * added to the system.
 */
public final class TypeValidators {
	private static final Logger LOG = LoggerFactory.getLogger(TypeValidators.class);
	private static final Object CMIS_USER = "cmis:user";

	public static class impl {
		public static void checkType(TypeManager tm, TypeDefinition td) {

			if (null == td) {
				LOG.error("Cannot add type, because the type defintion is null.");
				throw new CmisInvalidArgumentException("Cannot add type, because the type defintion is null.");
			}

			if (null == tm.getTypeById(td.getParentTypeId())) {
				LOG.error("Cannot add type, because parent with id ", td.getParentTypeId(), " does not exist.");
				throw new CmisInvalidArgumentException(
						"Cannot add type, because parent with id " + td.getParentTypeId(), " does not exist.");
			}

			if (null != tm.getTypeById(td.getId())) {
				LOG.error("Cannot add type, because type with id " + td.getId(), " already exists.");
				throw new CmisInvalidArgumentException("Cannot add type, because type with id " + td.getId(),
						" already exists.");
			}

			checkTypeId(tm, td.getId());
			checkTypeQueryName(tm, td.getQueryName());
			checkTypeLocalName(tm, td.getLocalName());
			checkBaseAndParentType(td);

			if (null != td.getPropertyDefinitions()) {
				TypeValidators.impl.checkProperties(tm, td.getPropertyDefinitions().values());
			}
		}

		public static AbstractTypeDefinition completeType(TypeDefinition type) {
			if (type instanceof DocumentTypeDefinition) {
				return completeTypeDoc((DocumentTypeDefinition) type);
			} else if (type instanceof FolderTypeDefinition) {
				return completeTypeFolder((FolderTypeDefinition) type);
			} else if (type instanceof PolicyTypeDefinition) {
				return completeTypePolicy((PolicyTypeDefinition) type);
			} else if (type instanceof ItemTypeDefinition) {
				return completeTypeItem((ItemTypeDefinition) type);
			} else if (type instanceof RelationshipTypeDefinition) {
				return completeTypeRelationship((RelationshipTypeDefinition) type);
			} else if (type instanceof SecondaryTypeDefinition) {
				return completeTypeSecondary((SecondaryTypeDefinition) type);
			} else {
				return null;
			}
		}

		public static void adjustTypeNamesAndId(AbstractTypeDefinition typeDef) {
			if (null == typeDef.getId()) {
				typeDef.setId(UUID.randomUUID().toString());
			} else {
				if (!NameValidator.impl.isValidId(typeDef.getId())) {
					// if there are illegal characters adjust them
					String newId = replaceInvalidCharacters(typeDef.getId());
					typeDef.setId(newId);
				}
			}
			if (!NameValidator.impl.isValidQueryName(typeDef.getQueryName())) {
				typeDef.setQueryName(typeDef.getId());
			}
			if (!NameValidator.impl.isValidLocalName(typeDef.getLocalName())) {
				typeDef.setLocalName(typeDef.getId());
			}
		}

		private static void completeAbstractTypeDefinition(AbstractTypeDefinition td) {
			if (td.isControllableAcl() == null) {
				td.setIsControllableAcl(true);
			}
			if (td.isControllablePolicy() == null) {
				td.setIsControllablePolicy(false);
			}
			if (td.isCreatable() == null) {
				td.setIsCreatable(true);
			}
			if (td.isFileable() == null) {
				td.setIsFileable(true);
			}
			td.setIsFulltextIndexed(false);
			td.setIsIncludedInSupertypeQuery(false);
			if (td.isQueryable() == null) {
				td.setIsQueryable(true);
			}
			td.setParentTypeId(td.getParentTypeId());
			TypeMutabilityImpl tm = new TypeMutabilityImpl();
			tm.setCanCreate(true);
			tm.setCanDelete(true);
			tm.setCanUpdate(true);
			td.setTypeMutability(tm);
			td.setExtensions(td.getExtensions());

			Map<String, PropertyDefinition<?>> propDefsNew = new LinkedHashMap<String, PropertyDefinition<?>>();
			if (null != td.getPropertyDefinitions()) {
				Map<String, PropertyDefinition<?>> propDefs = td.getPropertyDefinitions();
				for (PropertyDefinition<?> pd : propDefs.values()) {
					AbstractPropertyDefinition<?> pdNew = completePropertyDef(pd);
					adjustPropertyNamesAndId(pdNew);
					propDefsNew.put(pdNew.getId(), pd);
				}
			}
			td.setPropertyDefinitions(propDefsNew);
		}

		private static void checkProperties(TypeManager tm, Collection<PropertyDefinition<?>> pds) {

			Collection<TypeDefinitionContainer> tdl = tm.getTypeDefinitionList();
			for (PropertyDefinition<?> pd2 : pds) {
				// check id syntax
				if (null == pd2.getId()) {
					LOG.error("property id cannot be null.");
					throw new CmisInvalidArgumentException("property id cannot be null.");
				}
				if (!NameValidator.impl.isValidId(pd2.getId())) {
					LOG.error(NameValidator.ERROR_ILLEGAL_NAME);
					throw new CmisInvalidArgumentException(NameValidator.ERROR_ILLEGAL_NAME);
				}

				// check query name syntax
				if (null == pd2.getQueryName()) {
					LOG.error("property query name cannot be null.");
					throw new CmisInvalidArgumentException("property query name cannot be null.");
				}
				if (!NameValidator.impl.isValidQueryName(pd2.getQueryName())) {
					LOG.error(NameValidator.ERROR_ILLEGAL_NAME);
					throw new CmisInvalidArgumentException(NameValidator.ERROR_ILLEGAL_NAME);
				}

				// check local name syntax
				if (null == pd2.getLocalName()) {
					LOG.error("property local name cannot be null.");
					throw new CmisInvalidArgumentException("property local name cannot be null.");
				}
				if (!NameValidator.impl.isValidLocalName(pd2.getLocalName())) {
					LOG.error(NameValidator.ERROR_ILLEGAL_NAME);
					throw new CmisInvalidArgumentException(NameValidator.ERROR_ILLEGAL_NAME);
				}

				for (TypeDefinitionContainer tdc : tdl) {
					TypeDefinition td = tdc.getTypeDefinition();
					if (null != td.getPropertyDefinitions()) {
						for (PropertyDefinition<?> pd1 : td.getPropertyDefinitions().values()) {
							// check if id is used
							if (pd1.getId().equals(pd2.getId())) {
								LOG.error("Property id " + pd2.getId(), " already in use in type ", td.getId());
								throw new CmisConstraintException("Property id " + pd2.getId(),
										" already in use in type " + td.getId());
							}
							// check if query name is used
							if (pd1.getQueryName().equals(pd2.getQueryName())) {
								LOG.error("Property query name " + pd2.getQueryName(), " already in use in type ",
										td.getQueryName());
								throw new CmisConstraintException("Property query name " + pd2.getQueryName(),
										" already in use in type " + td.getQueryName());
							}
							// check if local name is used
							if (pd1.getLocalName().equals(pd2.getLocalName())) {
								LOG.error("Property local name ", pd2.getLocalName(),
										" already in use in type " + td.getId());
								throw new CmisConstraintException("Property local name " + pd2.getLocalName()
										+ " already in use in type " + td.getId());
							}
						}
					}
				}
			}
		}

		private static void adjustPropertyNamesAndId(AbstractPropertyDefinition<?> propDef) {
			if (null == propDef.getId()) {
				propDef.setId(UUID.randomUUID().toString());
			} else {
				if (!NameValidator.impl.isValidId(propDef.getId())) {
					String newId = replaceInvalidCharacters(propDef.getId());
					propDef.setId(newId);
				}
			}
			if (!NameValidator.impl.isValidQueryName(propDef.getQueryName())) {
				propDef.setQueryName(propDef.getId());
			}
			if (!NameValidator.impl.isValidLocalName(propDef.getLocalName())) {
				propDef.setLocalName(propDef.getId());
			}
		}

		private static String replaceInvalidCharacters(String id) {
			// if there are illegal characters adjust them
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < id.length(); i++) {
				if (NameValidator.impl.isValidId(id.substring(i, i + 1))) {
					sb.append(id.charAt(i));
				} else {
					sb.append('_');
				}
			}
			return sb.toString();
		}

		private static DocumentTypeDefinitionImpl completeTypeDoc(DocumentTypeDefinition type) {
			DocumentTypeDefinitionImpl td = TypeUtil.impl.cloneTypeDoc(type);
			completeAbstractTypeDefinition(td);
			td.setIsVersionable(type.isVersionable());
			td.setContentStreamAllowed(type.getContentStreamAllowed());
			if (td.isVersionable() == null) {
				td.setIsVersionable(false);
			}
			if (td.getContentStreamAllowed() == null) {
				td.setContentStreamAllowed(ContentStreamAllowed.ALLOWED);
			}
			return td;
		}

		private static FolderTypeDefinitionImpl completeTypeFolder(FolderTypeDefinition type) {
			FolderTypeDefinitionImpl td = TypeUtil.impl.cloneTypeFolder(type);
			completeAbstractTypeDefinition(td);
			return td;
		}

		private static RelationshipTypeDefinitionImpl completeTypeRelationship(RelationshipTypeDefinition type) {
			RelationshipTypeDefinitionImpl td = TypeUtil.impl.cloneTypeRelationship(type);
			completeAbstractTypeDefinition(td);
			td.setAllowedSourceTypes(type.getAllowedSourceTypeIds());
			td.setAllowedTargetTypes(type.getAllowedTargetTypeIds());
			return td;
		}

		private static ItemTypeDefinitionImpl completeTypeItem(ItemTypeDefinition type) {
			ItemTypeDefinitionImpl td = TypeUtil.impl.cloneTypeItem(type);
			td.initialize(type);
			completeAbstractTypeDefinition(td);
			return td;
		}

		private static SecondaryTypeDefinitionImpl completeTypeSecondary(SecondaryTypeDefinition type) {
			SecondaryTypeDefinitionImpl td = TypeUtil.impl.cloneTypeSecondary(type);
			completeAbstractTypeDefinition(td);
			return td;
		}

		private static PolicyTypeDefinitionImpl completeTypePolicy(PolicyTypeDefinition type) {
			PolicyTypeDefinitionImpl td = TypeUtil.impl.cloneTypePolicy(type);
			completeAbstractTypeDefinition(td);
			return null;
		}

		// When creating types PropertyDefinitions may only be partially filled,
		// fill all fields
		// to make a complete definition
		private static AbstractPropertyDefinition<?> completePropertyDef(PropertyDefinition<?> pdSrc) {
			AbstractPropertyDefinition<?> newPropDef = TypeUtil.impl.clonePropertyDefinition(pdSrc);

			if (null == newPropDef.getPropertyType()) {
				throw new CmisInvalidArgumentException("Property " + pdSrc.getId() + "has no property type.");
			}
			if (null == newPropDef.getId()) {
				newPropDef.setId(UUID.randomUUID().toString());
			}
			if (null == newPropDef.getQueryName()) {
				newPropDef.setQueryName(pdSrc.getId());
			}
			if (null == newPropDef.getLocalName()) {
				newPropDef.setLocalName(pdSrc.getId());
			}

			if (null == newPropDef.getCardinality()) {
				newPropDef.setCardinality(Cardinality.SINGLE);
			}
			if (null == newPropDef.isOrderable()) {
				newPropDef.setIsOrderable(true);
			}
			if (null == newPropDef.isQueryable()) {
				newPropDef.setIsQueryable(true);
			}
			if (null == newPropDef.isRequired()) {
				newPropDef.setIsRequired(false);
			}
			if (null == newPropDef.getUpdatability()) {
				newPropDef.setUpdatability(Updatability.READWRITE);
			}

			return newPropDef;
		}

		public static AccessControlListImplExt expandAclMakros(String user, Acl acl) {
			boolean mustCopy = false;

			if (user == null || acl == null || acl.getAces() == null) {
				return (AccessControlListImplExt) acl;
			}

			List<Ace> implAce = acl.getAces();

			for (Ace ace : implAce) {
				String principal = ace.getPrincipalId();
				if (principal != null && principal.equals(CMIS_USER)) {
					mustCopy = true;
				}
			}
			if (mustCopy) {
			}

			List<Ace> list = new ArrayList<Ace>(acl.getAces().size());
			for (Ace ace : acl.getAces()) {
				AccessControlEntryImpl aces = new AccessControlEntryImpl(
						new AccessControlPrincipalDataImpl(ace.getPrincipalId()), ace.getPermissions());
				list.add(aces);
			}
			return new AccessControlListImplExt(list, "objectonly", false);
			// if (mustCopy) {
			// AccessControlListImpl result = new AccessControlListImpl();
			// List<Ace> list = new ArrayList<Ace>(acl.getAces().size());
			// for (Ace ace : acl.getAces()) {
			// String principal = ace.getPrincipalId();
			// if (principal != null && principal.equals(CMIS_USER)) {
			// AccessControlEntryImpl ace2 = new AccessControlEntryImpl();
			// ace2.setPermissions(ace.getPermissions());
			// ace2.setExtensions(ace.getExtensions());
			// ace2.setPrincipal(new AccessControlPrincipalDataImpl(user));
			//
			// list.add(ace2);
			// } else {
			// list.add(ace);
			// }
			// }
			// result.setAces(list);
			// return result;
			// } else {
			// AccessControlListImpl result = new AccessControlListImpl();
			// List<Ace> list = new ArrayList<Ace>(acl.getAces().size());
			// for (Ace ace : acl.getAces()) {
			// String principal = ace.getPrincipalId();
			// if (principal != null && principal.equals(CMIS_USER)) {
			// AccessControlEntryImpl ace2 = new AccessControlEntryImpl();
			// ace2.setPermissions(ace.getPermissions());
			// ace2.setExtensions(ace.getExtensions());
			// ace2.setPrincipal(new AccessControlPrincipalDataImpl(user));
			//
			// list.add(ace2);
			// } else {
			// list.add(ace);
			// }
			// }
			// result.setAces(list);
			//
			// return acl;
			// }
		}

		private static void checkTypeId(TypeManager tm, String typeId) {

			if (null == typeId) {
				LOG.error("Unknown TypeId:{}", typeId);
				throw new CmisInvalidArgumentException("Type id cannot be null.");
			}

			// check name syntax
			if (!NameValidator.impl.isValidId(typeId)) {
				LOG.error(NameValidator.ERROR_ILLEGAL_ID);
				throw new CmisInvalidArgumentException(NameValidator.ERROR_ILLEGAL_ID);
			}

			if (null != tm.getTypeById(typeId)) {
				LOG.error("You cannot add type with id :{}" + typeId + " because it already exists.");
				throw new CmisInvalidArgumentException(
						"You cannot add type with id " + typeId + " because it already exists.");
			}
		}

		private static void checkTypeQueryName(TypeManager tm, String queryName) {

			if (null == queryName) {
				LOG.error("Unknown QueryName:{}", queryName);
				throw new CmisInvalidArgumentException("Query name cannot be null.");
			}

			if (!NameValidator.impl.isValidQueryName(queryName)) {
				LOG.error(NameValidator.ERROR_ILLEGAL_NAME);
				throw new CmisInvalidArgumentException(NameValidator.ERROR_ILLEGAL_NAME);
			}

			// check set query name is unique in the type system
			if (null != tm.getTypeByQueryName(queryName)) {
				LOG.error("You cannot add type with query name " + queryName + " because it already exists.");
				throw new CmisInvalidArgumentException(
						"You cannot add type with query name " + queryName + " because it already exists.");
			}
		}

		private static void checkTypeLocalName(TypeManager tm, String localName) {

			if (null == localName) {
				LOG.error("Unknown localName:{}", localName);
				throw new CmisInvalidArgumentException("Local name cannot be null.");
			}

			if (!NameValidator.impl.isValidLocalName(localName)) {
				LOG.error(NameValidator.ERROR_ILLEGAL_NAME);
				throw new CmisInvalidArgumentException(NameValidator.ERROR_ILLEGAL_NAME);
			}

			for (TypeDefinitionContainer tdc : tm.getTypeDefinitionList()) {
				if (tdc.getTypeDefinition().getLocalName().equals(localName)) {
					LOG.error("You cannot add type with local name " + localName + " because it already exists.");
					throw new CmisConstraintException(
							"You cannot add type with local name " + localName + " because it already exists.");
				}
			}
		}

		private static void checkBaseAndParentType(TypeDefinition td) {
			if (null == td.getBaseTypeId()) {
				LOG.error("You cannot create a type without a base type id:{} ", td.getId());
				throw new CmisInvalidArgumentException(
						"You cannot create a type without a base type id: " + td.getId());
			}
			if (null == td.getParentTypeId()) {
				LOG.error("You cannot create a type without a parent type id:{} ", td.getId());
				throw new CmisInvalidArgumentException(
						"You cannot create a type without a parent type id: " + td.getId());
			}

		}
	}

}
