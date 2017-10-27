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
package com.pogeyan.cmis.data.mongo.services;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Query;

import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.data.mongo.MTypeObject;

public class MTypeManagerDAOImpl extends BasicDAO<MTypeObject, ObjectId> implements MTypeManagerDAO {

	public MTypeManagerDAOImpl(Class<MTypeObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public List<MTypeObject> getById(List<?> typeId) {
		if (typeId == null) {
			List<ObjectId> getid = this.findIds();
			if (getid.size() > 0) {
				return new ArrayList<MTypeObject>();
			} else {
				return null;
			}
		} else if (typeId.size() == 1) {
			Query<MTypeObject> query = createQuery().field("id").equal(typeId.get(0));
			return query.asList();
		} else {
			Query<MTypeObject> query = createQuery().field("id").in(typeId);
			return query.asList();
		}
	}

	@Override
	public void delete(String typeId) {
		Query<MTypeObject> query = createQuery().field("id").equal(typeId);
		this.deleteByQuery(query);
	}

	public List<MTypeObject> getChildrenIds(String parentId, int maxItems, int skipCount) {
		Query<MTypeObject> query = createQuery().field("parent").equal(parentId);
		if (maxItems > 0 && skipCount >= 0) {
			query = query.offset(skipCount).limit(maxItems);
		}

		return query.asList();
	}

	@Override
	public Map<String, PropertyDefinition<?>> getAllPropertyById(String propId) {
		Query<MTypeObject> query = createQuery().field("propertyDefinition." + propId + ".id").equal(propId);
		if (query.get() != null) {
			return query.get().getPropertyDefinitions();
		} else {
			return null;
		}
	}

	@Override
	public void commit(TypeDefinition entity) {
		this.save((MTypeObject) entity);
	}

	@Override
	public TypeDefinition createObjectFacade(String id, String localName, String localNamespace, String displayName,
			String queryName, String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable,
			Boolean isFileable, Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutability typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition) {
		return new MTypeObject(id, localName, localNamespace, displayName, queryName, description, baseTypeId, parent,
				isCreatable, isFileable, isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery,
				isControllablePolicy, isControllableAcl, (TypeMutabilityImpl)typeMutability, propertyDefinition);
	}
}
