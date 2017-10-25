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
import java.util.stream.Stream;

import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.Query;
import org.mongodb.morphia.query.UpdateOperations;

import com.pogeyan.cmis.MChangeType;
import com.pogeyan.cmis.api.data.TokenImpl;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.data.objects.MDocumentObject;

public class MDocumentObjectDAOImpl extends BasicDAO<MDocumentObject, ObjectId> implements MDocumentObjectDAO {

	public MDocumentObjectDAOImpl(Class<MDocumentObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public List<MDocumentObject> filter(Map<String, Object> fieldNames, String[] mappedColumns) {
		Query<MDocumentObject> query = createQuery().field("token.changetype").notEqual(MChangeType.DELETED.value());
		for (Map.Entry<String, Object> entry : fieldNames.entrySet()) {
			query = query.field(entry.getKey()).equal(entry.getValue());
		}
		if (mappedColumns != null && mappedColumns.length > 0) {
			query = query.retrievedFields(true, mappedColumns);
		}
		return query.asList();
	}

	@Override
	public void delete(ObjectId objectId, List<String> removeProps, boolean forceDelete, boolean removefields,
			TokenImpl token) {
		Query<MDocumentObject> query = createQuery().field("id").equal(objectId).field("token.changetype")
				.notEqual(MChangeType.DELETED.value());
		if (forceDelete) {
			this.deleteByQuery(query);
		} else {
			UpdateOperations<MDocumentObject> update = createUpdateOperations();
			if (removefields) {
				for (String field : removeProps) {
					update = update.unset(field);
				}
				update = update.set("token", token);
				update(query, update);
			} else {

				update = update.set("token", token);
				update = update.unset("properties");
				update(query, update);
			}

		}

	}

	@Override
	public void update(ObjectId objectId, Map<String, Object> updateProps) {
		UpdateOperations<MDocumentObject> update = createUpdateOperations();
		Query<MDocumentObject> query = createQuery().field("id").equal(objectId).field("token.changetype")
				.notEqual(MChangeType.DELETED.value());
		for (Map.Entry<String, Object> entry : updateProps.entrySet()) {
			update = update.set(entry.getKey(), entry.getValue());
		}
		update(query, update);
	}

	@Override
	public List<MDocumentObject> getCheckOutDocs(String folderId, String[] principalIds, boolean aclPropagation,
			int maxItems, int skipCount, String orderBy) {
		Query<MDocumentObject> query = null;
		if (folderId == null) {
			query = createQuery().field("typeId").equalIgnoreCase("cmis:document").field("isPrivateWorkingCopy")
					.equal(true).field("token.changetype").notEqual(MChangeType.DELETED.value());
		} else {
			query = createQuery().field("parentId").equal(folderId).field("typeId").equalIgnoreCase("cmis:document")
					.field("isPrivateWorkingCopy").equal(true).field("token.changetype")
					.notEqual(MChangeType.DELETED.value());
		}
		if (orderBy != null) {
			query = query.order("-" + orderBy);
		}
		if (maxItems > 0) {
			query = query.offset(skipCount).limit(maxItems);
		}
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
			return query.asList();
		} else {
			return query.asList();
		}
	}

	private Criteria[] getAclCriteria(String[] principalIds, Query<MDocumentObject> query) {
		Criteria[] checkAcl = Stream.of(principalIds).map(t -> query.criteria("acl.ace.principalId").equalIgnoreCase(t))
				.toArray(s -> new Criteria[s]);
		return checkAcl;
	}

	@Override
	public long getCheckOutDocsSize(String folderId, String[] principalIds, boolean aclPropagation) {
		Query<MDocumentObject> query = null;
		if (folderId == null) {
			query = createQuery().field("typeId").equalIgnoreCase("cmis:document").field("isPrivateWorkingCopy")
					.equal(true).field("token.changetype").notEqual(MChangeType.DELETED.value());
		} else {
			query = createQuery().field("parentId").equal(folderId).field("typeId").equalIgnoreCase("cmis:document")
					.field("isPrivateWorkingCopy").equal(true).field("token.changetype")
					.notEqual(MChangeType.DELETED.value());
		}
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
			return query.countAll();
		} else {
			return query.countAll();
		}
	}

	@Override
	public void commit(MDocumentObject entity) {
		this.save(entity);
	}

}