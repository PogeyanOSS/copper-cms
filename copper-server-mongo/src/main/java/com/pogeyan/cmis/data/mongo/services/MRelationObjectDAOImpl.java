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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.commons.lang3.ArrayUtils;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.CriteriaContainer;
import org.mongodb.morphia.query.CriteriaContainerImpl;
import org.mongodb.morphia.query.Query;
import org.mongodb.morphia.query.UpdateOperations;

import com.mongodb.DBObject;
import com.pogeyan.cmis.api.data.IRelationObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MRelationObjectDAO;
import com.pogeyan.cmis.data.mongo.MRelationObject;
import com.pogeyan.cmis.data.mongo.MongoAclImpl;
import com.pogeyan.cmis.data.mongo.MongoToken;

public class MRelationObjectDAOImpl extends BasicDAO<MRelationObject, String> implements MRelationObjectDAO {

	public MRelationObjectDAOImpl(Class<MRelationObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public void delete(String repositoryId, String[] principalIds, String objectId, boolean forceDelete,
			boolean aclPropagation, TokenImpl token, String typeId) {
		Query<MRelationObject> query = createQuery().disableValidation().field("id").equal(objectId)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
		}
		if (forceDelete) {
			this.deleteByQuery(query);
		} else {
			UpdateOperations<MRelationObject> update = createUpdateOperations().disableValidation();
			update = update.set("token", MRelationObject.convertMongoToken(token));
			update = update.set("modifiedAt", token.getTime());
			update = update.unset("properties");
			update(query, update);
		}
	}

	@Override
	public void update(String repositoryId, String objectId, Map<String, Object> updateProps, String typeId) {
		UpdateOperations<MRelationObject> update = createUpdateOperations().disableValidation();
		Query<MRelationObject> query = createQuery().disableValidation().field("id").equal(objectId)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (updateProps.get("acl") != null) {
			MongoAclImpl mAcl = MRelationObject.convertMongoAcl((AccessControlListImplExt) updateProps.get("acl"));
			updateProps.remove("acl");
			updateProps.put("acl", mAcl);
		}
		if (updateProps.get("token") != null) {
			MongoToken mToken = MRelationObject.convertMongoToken((TokenImpl) updateProps.get("token"));
			updateProps.remove("token");
			updateProps.put("token", mToken);
		}
		for (Map.Entry<String, Object> entry : updateProps.entrySet()) {
			update = update.set(entry.getKey(), entry.getValue());
		}
		update(query, update);
	}

	@Override
	public MRelationObject getLatestToken() {
		Query<MRelationObject> query = createQuery().disableValidation().order("-token.time").limit(-1);
		return query.get();
	}

	@Override
	public List<MRelationObject> filter(Map<String, Object> fieldNames, String[] principalIds, boolean aclPropagation,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns, String typeId) {
		Query<MRelationObject> query = createQuery().disableValidation().field("token.changeType")
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
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
			return query.asList();
		} else {
			return query.asList();
		}
	}

	@Override
	public List<MRelationObject> getObjects(List<String> objectIds, String[] principalIds, boolean aclPropagation,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns, String typeId) {
		Query<MRelationObject> query = createQuery().disableValidation().field("token.changeType")
				.notEqual(TokenChangeType.DELETED.value());
		query = query.field("id").in(objectIds);
		if (includePagination) {
			if (maxItems > 0) {
				query = query.offset(skipCount).limit(maxItems);
			}
		}
		if (mappedColumns != null && mappedColumns.length > 0) {
			query = query.retrievedFields(true, mappedColumns);
		}
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
			return query.asList();
		} else {
			return query.asList();
		}
	}

	@Override
	public void commit(IRelationObject entity, String typeId) {
		this.save((MRelationObject) entity);
	}

	public IRelationObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String description, String createdBy, String modifiedBy, TokenImpl token,
			Map<String, Object> properties, Acl acl,
			String parentId) {
		return new MRelationObject(name, baseId, typeId, description, createdBy,
				modifiedBy, token, properties, acl, parentId);
	}

	private Criteria[] getAclCriteria(String[] principalIds, Query<MRelationObject> query) {
		Criteria[] checkAcl = new Criteria[] {};
		if (principalIds != null) {

			// if principalIds contain "__" then we use startsWith else we use equals for
			// principalId check
			List<CriteriaContainerImpl> principalId = Stream.of(principalIds).filter(a -> a.contains("__"))
					.map(t -> query.criteria("acl.aces.principal.principalId").startsWithIgnoreCase(t))
					.collect(Collectors.toList());

			principalId.addAll(Stream.of(principalIds).filter(a -> !a.contains("__"))
					.map(t -> query.criteria("acl.aces.principal.principalId").equalIgnoreCase(t))
					.collect(Collectors.toList()));

			checkAcl = principalId.stream().toArray(s -> new Criteria[s]);

		} else {
			checkAcl = new Criteria[] {
					query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.OBJECTONLY.toString()),
					query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.PROPAGATE.toString()) };
		}
		Criteria[] checkAclRepo = new Criteria[] {
				query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.REPOSITORYDETERMINED.toString()) };
		Criteria[] result = ArrayUtils.addAll(checkAclRepo, checkAcl);
		return result;
	}
}