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

import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.CriteriaContainerImpl;
import org.mongodb.morphia.query.Query;
import org.mongodb.morphia.query.UpdateOperations;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.data.mongo.MBaseObject;
import com.pogeyan.cmis.data.mongo.MDocumentObject;
import com.pogeyan.cmis.data.mongo.MongoAclImpl;
import com.pogeyan.cmis.data.mongo.MongoToken;

public class MDocumentObjectDAOImpl extends BasicDAO<MDocumentObject, ObjectId> implements MDocumentObjectDAO {

	public MDocumentObjectDAOImpl(Class<MDocumentObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public List<MDocumentObject> filter(Map<String, Object> fieldNames, String[] mappedColumns) {
		Query<MDocumentObject> query = createQuery().disableValidation().field("token.changeType")
				.notEqual(TokenChangeType.DELETED.value());
		for (Map.Entry<String, Object> entry : fieldNames.entrySet()) {
			query = query.field(entry.getKey()).equal(entry.getValue());
		}
		if (mappedColumns != null && mappedColumns.length > 0) {
			query = query.retrievedFields(true, mappedColumns);
		}
		return query.asList();
	}

	@Override
	public void delete(String objectId, List<String> removeProps, boolean forceDelete, boolean removefields,
			TokenImpl token) {
		Query<MDocumentObject> query = createQuery().disableValidation().field("id").equal(objectId)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (forceDelete) {
			this.deleteByQuery(query);
		} else {
			UpdateOperations<MDocumentObject> update = createUpdateOperations();
			update = update.set("token", MBaseObject.convertMongoToken(token));
			update = update.set("modifiedAt", token.getTime());
			if (removefields) {
				for (String field : removeProps) {
					update = update.unset(field);
				}
				update(query, update);
			} else {
				update = update.unset("properties");
				update(query, update);
			}

		}

	}

	@Override
	public void update(String objectId, Map<String, Object> updateProps) {
		UpdateOperations<MDocumentObject> update = createUpdateOperations();
		Query<MDocumentObject> query = createQuery().disableValidation().field("id").equal(objectId)
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
	public List<MDocumentObject> getCheckOutDocs(String folderId, String[] principalIds, boolean aclPropagation,
			int maxItems, int skipCount, String orderBy) {
		Query<MDocumentObject> query = null;
		if (folderId == null) {
			query = createQuery().disableValidation().field("typeId").equalIgnoreCase("cmis:document")
					.field("isPrivateWorkingCopy").equal(true).field("token.changeType")
					.notEqual(TokenChangeType.DELETED.value());
		} else {
			query = createQuery().disableValidation().field("parentId").equal(folderId).field("typeId")
					.equalIgnoreCase("cmis:document").field("isPrivateWorkingCopy").equal(true)
					.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
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
		return checkAcl;
	}

	@Override
	public long getCheckOutDocsSize(String folderId, String[] principalIds, boolean aclPropagation) {
		Query<MDocumentObject> query = null;
		if (folderId == null) {
			query = createQuery().disableValidation().field("typeId").equalIgnoreCase("cmis:document")
					.field("isPrivateWorkingCopy").equal(true).field("token.changeType")
					.notEqual(TokenChangeType.DELETED.value());
		} else {
			query = createQuery().disableValidation().field("parentId").equal(folderId).field("typeId")
					.equalIgnoreCase("cmis:document").field("isPrivateWorkingCopy").equal(true)
					.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		}
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
			return query.countAll();
		} else {
			return query.countAll();
		}
	}

	@Override
	public void commit(IDocumentObject entity) {
		this.save((MDocumentObject) entity);
	}

	public IDocumentObject createObjectFacade(IBaseObject baseObject, Boolean isImmutable, Boolean isLatestVersion,
			Boolean isMajorVersion, Boolean isLatestMajorVersion, Boolean isPrivateWorkingCopy, String versionLabel,
			String versionSeriesId, String versionReferenceId, Boolean isVersionSeriesCheckedOut,
			String versionSeriesCheckedOutBy, String versionSeriesCheckedOutId, String checkinComment,
			Long contentStreamLength, String contentStreamMimeType, String contentStreamFileName,
			String contentStreamId, String previousVersionObjectId) {
		return new MDocumentObject((MBaseObject) baseObject, isImmutable, isLatestVersion, isMajorVersion,
				isLatestMajorVersion, isPrivateWorkingCopy, versionLabel, versionSeriesId, versionReferenceId,
				isVersionSeriesCheckedOut, versionSeriesCheckedOutBy, versionSeriesCheckedOutId, checkinComment,
				contentStreamLength, contentStreamMimeType, contentStreamFileName, contentStreamId,
				previousVersionObjectId);
	}
}