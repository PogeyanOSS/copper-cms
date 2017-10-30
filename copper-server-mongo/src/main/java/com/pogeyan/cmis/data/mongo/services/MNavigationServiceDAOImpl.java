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
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.Query;

import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.data.mongo.MBaseObject;

public class MNavigationServiceDAOImpl extends BasicDAO<MBaseObject, ObjectId> implements MNavigationServiceDAO {

	public MNavigationServiceDAOImpl(Class<MBaseObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	@Override
	public List<MBaseObject> getChildrenIds(String path, String[] principalIds, boolean aclPropagation, int maxItems,
			int skipCount, String orderBy, String[] mappedColumns) {
		Query<MBaseObject> query = null;
		if (orderBy != null) {
			query = createQuery().disableValidation().filter("internalPath", path).field("token.changeType")
					.notEqual(TokenChangeType.DELETED.value()).order("-" + orderBy);
		} else {
			query = createQuery().disableValidation().filter("internalPath", path).field("token.changeType")
					.notEqual(TokenChangeType.DELETED.value());
		}
		if (maxItems > 0) {
			query = query.offset(skipCount).limit(maxItems);
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
	public long getChildrenSize(String path, String[] principalIds, boolean aclPropagation) {
		Query<MBaseObject> query = createQuery().disableValidation().filter("internalPath", path)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
			return query.countAll();
		} else {
			return query.countAll();
		}
	}

	@Override
	public List<MBaseObject> getDescendants(String path, String[] principalIds, boolean aclPropagation) {
		if (aclPropagation) {
			Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
			Query<MBaseObject> query = createQuery().disableValidation().filter("internalPath =", exp)
					.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
			query.or(getAclCriteria(principalIds, query));
			return query.asList();
		} else {
			Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
			Query<MBaseObject> query = createQuery().disableValidation().filter("internalPath =", exp)
					.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
			return query.asList();
		}
	}

	@Override
	public List<MBaseObject> getFolderTreeIds(String path, String[] principalIds, boolean aclPropagation) {
		if (aclPropagation) {
			Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
			Query<MBaseObject> query = createQuery().disableValidation().filter("internalPath =", exp).field("baseId")
					.equalIgnoreCase("cmis:folder").field("token.changeType").notEqual(TokenChangeType.DELETED.value());
			query.or(getAclCriteria(principalIds, query));
			return query.asList();
		} else {
			Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
			Query<MBaseObject> query = createQuery().disableValidation().filter("internalPath =", exp).field("baseId")
					.equalIgnoreCase("cmis:folder").field("token.changeType").notEqual(TokenChangeType.DELETED.value());
			return query.asList();
		}

	}

	private Criteria[] getAclCriteria(String[] principalIds, Query<MBaseObject> query) {
		Criteria[] checkAcl = Stream.of(principalIds)
				.map(t -> query.criteria("acl.aces.principal.principalId").equalIgnoreCase(t))
				.toArray(s -> new Criteria[s]);
		return checkAcl;
	}

}
