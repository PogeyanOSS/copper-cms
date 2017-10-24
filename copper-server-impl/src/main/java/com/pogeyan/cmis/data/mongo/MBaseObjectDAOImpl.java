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
package com.pogeyan.cmis.data.mongo;

import java.util.List;
import java.util.Map;

import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Query;
import org.mongodb.morphia.query.UpdateOperations;

import com.pogeyan.cmis.MChangeType;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.data.objects.MBaseObject;
import com.pogeyan.cmis.data.objects.MToken;

public class MBaseObjectDAOImpl extends BasicDAO<MBaseObject, ObjectId> implements MBaseObjectDAO {

	public MBaseObjectDAOImpl(Class<MBaseObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public void delete(ObjectId objectId, boolean forceDelete, MToken token) {
		Query<MBaseObject> query = createQuery().field("id").equal(objectId).field("token.changetype")
				.notEqual(MChangeType.DELETED.value());
		if (forceDelete) {
			this.deleteByQuery(query);
		} else {
			UpdateOperations<MBaseObject> update = createUpdateOperations();
			update = update.set("token", token);
			update = update.unset("properties");
			update(query, update);
		}

	}

	@Override
	public void update(ObjectId objectId, Map<String, Object> updateProps) {
		UpdateOperations<MBaseObject> update = createUpdateOperations();
		Query<MBaseObject> query = createQuery().field("id").equal(objectId).field("token.changetype")
				.notEqual(MChangeType.DELETED.value());
		for (Map.Entry<String, Object> entry : updateProps.entrySet()) {
			update = update.set(entry.getKey(), entry.getValue());
		}
		update(query, update);
	}

	@Override
	public MBaseObject getLatestToken() {
		Query<MBaseObject> query = createQuery().order("-token.time").limit(-1);
		return query.get();
	}

	@Override
	public List<MBaseObject> filter(Map<String, Object> fieldNames, boolean includePagination, int maxItems,
			int skipCount, String[] mappedColumns) {
		Query<MBaseObject> query = createQuery().field("token.changetype").notEqual(MChangeType.DELETED.value());
		for (Map.Entry<String, Object> entry : fieldNames.entrySet()) {
			query = query.field(entry.getKey()).equal(entry.getValue());
		}
		if (includePagination) {
			if (maxItems > 0) {
				query = query.offset(skipCount).limit(maxItems);
			}
		}
		if (mappedColumns != null && mappedColumns.length > 0) {
			query = query.retrievedFields(true, mappedColumns);
		}
		return query.asList();
	}

	@Override
	public void commit(MBaseObject entity) {
		this.save(entity);
	}

}