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

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.Query;
import org.mongodb.morphia.query.UpdateOperations;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.data.mongo.MBaseObject;
import com.pogeyan.cmis.data.mongo.MongoAclImpl;
import com.pogeyan.cmis.data.mongo.MongoToken;

public class MBaseObjectDAOImpl extends BasicDAO<MBaseObject, String> implements MBaseObjectDAO {

	public MBaseObjectDAOImpl(Class<MBaseObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public void delete(String objectId, boolean forceDelete, TokenImpl token, String[] options) {
		Query<MBaseObject> query = createQuery().disableValidation().field("id").equal(objectId)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		query.or(getAclCriteria(query));
		if (forceDelete) {
			this.deleteByQuery(query);
		} else {
			UpdateOperations<MBaseObject> update = createUpdateOperations().disableValidation();
			update = update.set("token", MBaseObject.convertMongoToken(token));
			update = update.set("modifiedAt", token.getTime());
			update = update.unset("properties");
			update(query, update);
		}

	}

	@Override
	public void update(String objectId, Map<String, Object> updateProps) {
		UpdateOperations<MBaseObject> update = createUpdateOperations().disableValidation();
		Query<MBaseObject> query = createQuery().disableValidation().field("id").equal(objectId)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (updateProps.get("acl") != null) {
			MongoAclImpl mAcl = MBaseObject.convertMongoAcl((AccessControlListImplExt) updateProps.get("acl"));
			updateProps.remove("acl");
			updateProps.put("acl", mAcl);
		}
		if (updateProps.get("token") != null) {
			MongoToken mToken = MBaseObject.convertMongoToken((TokenImpl) updateProps.get("token"));
			updateProps.remove("token");
			updateProps.put("token", mToken);
		}
		for (Map.Entry<String, Object> entry : updateProps.entrySet()) {
			update = update.set(entry.getKey(), entry.getValue());
		}
		update(query, update);
	}

	@Override
	public MBaseObject getLatestToken() {
		Query<MBaseObject> query = createQuery().disableValidation().order("-token.time").limit(-1);
		return query.get();
	}

	@Override
	public List<MBaseObject> filter(Map<String, Object> fieldNames, boolean includePagination, int maxItems,
			int skipCount, String[] mappedColumns) {
		Query<MBaseObject> query = createQuery().disableValidation().field("token.changeType")
				.notEqual(TokenChangeType.DELETED.value());
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
		query.or(getAclCriteria(query));
		return query.asList();
	}

	@Override
	public void commit(IBaseObject entity) {
		this.save((MBaseObject) entity);
	}

	public IBaseObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId) {
		return new MBaseObject(name, baseId, typeId, fRepositoryId, secondaryTypeIds, description, createdBy,
				modifiedBy, token, internalPath, properties, policies, acl, path, parentId);
	}

	private Criteria[] getAclCriteria(Query<MBaseObject> query) {
		Criteria[] checkAclRepo = new Criteria[] {
				query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.REPOSITORYDETERMINED.toString()),
				query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.OBJECTONLY.toString()),
				query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.PROPAGATE.toString()) };
		return checkAclRepo;
	}
}