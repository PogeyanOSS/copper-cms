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
package com.pogeyan.cmis.repo;

import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Query;
import org.mongodb.morphia.query.UpdateOperations;
import org.mongodb.morphia.query.UpdateResults;

import com.pogeyan.cmis.repo.MRepository;
import com.pogeyan.cmis.repo.MRepositoryDAO;

public class MRepositoryDAOImpl extends BasicDAO<MRepository, String> implements MRepositoryDAO {

	public MRepositoryDAOImpl(Class<MRepository> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public MRepository getByRepositoryId(String repositoryId) {
		Query<MRepository> query = createQuery().field("repositoryId").equal(repositoryId);
		return query.get();
	}

	@Override
	public UpdateResults disableRepository(String repositoryId) {
		Query<MRepository> query = createQuery().field("repositoryId").equal(repositoryId);
		UpdateOperations<MRepository> update = createUpdateOperations();
		update = update.set("isDisabled", true);
		UpdateResults results = update(query, update);
		return results;
	}
}
