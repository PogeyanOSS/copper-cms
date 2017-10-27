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

import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Query;

import com.pogeyan.cmis.api.data.common.CmisDocumentTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;

public class MDocumentTypeManagerDAOImpl extends BasicDAO<CmisDocumentTypeDefinitionImpl, ObjectId>
		implements MDocumentTypeManagerDAO {

	public MDocumentTypeManagerDAOImpl(Class<CmisDocumentTypeDefinitionImpl> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public CmisDocumentTypeDefinitionImpl getByTypeId(String typeId) {
		Query<CmisDocumentTypeDefinitionImpl> query = createQuery().field("id").equal(typeId);
		return query.get();
	}

}
