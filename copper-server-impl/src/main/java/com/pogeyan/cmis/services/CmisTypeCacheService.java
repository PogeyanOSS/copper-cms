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
package com.pogeyan.cmis.services;

import java.util.HashMap;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.impl.TypeCache;
import org.bson.types.ObjectId;

import com.pogeyan.cmis.DBUtils;
import com.pogeyan.cmis.data.DatabaseManager;
import com.pogeyan.cmis.data.dao.MTypeManagerDAO;
import com.pogeyan.cmis.data.objects.MBaseObject;

public class CmisTypeCacheService implements TypeCache {
	private static Map<String, TypeCache> instances = new HashMap<String, TypeCache>();
	private final String repositoryId;

	CmisTypeCacheService(String repositoryId) {
		this.repositoryId = repositoryId;
		CmisTypeCacheService.instances.put(repositoryId, this);
	}

	@Override
	public TypeDefinition getTypeDefinition(String typeId) {
		return CmisTypeServices.Impl.getTypeDefinition(this.repositoryId, typeId, null);
	}

	@Override
	public TypeDefinition reloadTypeDefinition(String typeId) {
		return CmisTypeServices.Impl.getTypeDefinition(this.repositoryId, typeId, null);
	}

	@Override
	public TypeDefinition getTypeDefinitionForObject(String objectId) {
		MBaseObject object = DBUtils.BaseDAO.getByObjectId(repositoryId, new ObjectId(objectId), null);
		return CmisTypeServices.Impl.getTypeDefinition(this.repositoryId, object.getTypeId(), null);
	}

	@Override
	public PropertyDefinition<?> getPropertyDefinition(String propId) {
		MTypeManagerDAO typeMorphiaDAO = DatabaseManager.getInstance(repositoryId).getObjectService(repositoryId,
				MTypeManagerDAO.class);
		Map<String, PropertyDefinition<?>> property = typeMorphiaDAO.getAllPropertyById(propId);
		return property.get(propId);
	}

	public static TypeCache get(String repositoryId) {
		TypeCache instance = null;
		if (!CmisTypeCacheService.instances.containsKey(repositoryId)) {
			instance = new CmisTypeCacheService(repositoryId);
		} else {
			instance = CmisTypeCacheService.instances.get(repositoryId);
		}

		return instance;
	}
}
