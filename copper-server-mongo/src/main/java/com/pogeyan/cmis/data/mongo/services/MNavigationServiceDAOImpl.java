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

import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.Query;

import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.api.uri.UriParser;
import com.pogeyan.cmis.api.uri.expression.ExceptionVisitExpression;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;
import com.pogeyan.cmis.data.mongo.MBaseObject;
import com.pogeyan.cmis.data.mongo.MongoExpressionVisitor;
import com.pogeyan.cmis.impl.uri.expression.ExpressionParserInternalError;

public class MNavigationServiceDAOImpl extends BasicDAO<MBaseObject, ObjectId> implements MNavigationServiceDAO {

	public MNavigationServiceDAOImpl(Class<MBaseObject> entityClass, Datastore ds) {
		super(entityClass, ds);
	}

	/*
	 * (non-Javadoc) filterExpression supports eq, ne, ge, gt, le, lt, startswith,
	 * endswith. example filter:
	 * "properties.orderId eq 100 and name eq pogeyan or startswith (name::'a')"
	 * "*,modifiedAt le 123456789 and typeId eq cmis:folder" -->* represents to get
	 * all properties data in that object
	 * "properties.isRead eq false and typeId ne cmis:folder"
	 * "properties.orderId gt 100 properties.purchaseOrder ge 100"
	 * "startswith (name::'a') and properties.orderId lt 100"
	 * "properties.orderId le 100"
	 * 
	 * example order: "name asc, repositoryId" "name desc"
	 * 
	 * @see
	 * com.pogeyan.cmis.api.data.services.MNavigationServiceDAO#getChildren(java
	 * .lang.String, java.lang.String[], boolean, int, int, java.lang.String,
	 * java.lang.String[], java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<MBaseObject> getChildren(String path, String[] principalIds, boolean aclPropagation, int maxItems,
			int skipCount, String orderBy, String[] mappedColumns, String filterExpression) {
		Query<MBaseObject> query = createQuery().disableValidation().filter("internalPath", path)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (!StringUtils.isEmpty(orderBy)) {
			if (this.isOrderByParsable(orderBy)) {
				try {
					OrderByExpression orderByExpression = UriParser.parseOrderBy(orderBy);
					query = (Query<MBaseObject>) orderByExpression
							.accept(new MongoExpressionVisitor<MBaseObject>(query));
				} catch (ExpressionParserException | ExceptionVisitExpression e) {
				}
			} else {
				query = query.order(orderBy);
			}
		}
		if (!StringUtils.isEmpty(filterExpression)) {
			try {
				FilterExpression expression = UriParser.parseFilter(filterExpression);
				query = (Query<MBaseObject>) expression.accept(new MongoExpressionVisitor<MBaseObject>(query));
			} catch (ExpressionParserException | ExceptionVisitExpression e) {
			}
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

	@SuppressWarnings("unused")
	private boolean isOrderByParsable(String orderByExpressionQuery) {
		try {
			OrderByExpression orderByExpression = UriParser.parseOrderBy(orderByExpressionQuery);
			return true;
		} catch (ExpressionParserException e) {
			return false;
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

	/*
	 * (non-Javadoc) filterExpression supports eq, ne, ge, gt, le, lt, startswith,
	 * endswith. example filter:
	 * "properties.orderId eq 100 and name eq pogeyan or startswith (name::'a')"
	 * "*,modifiedAt le 123456789 and typeId eq cmis:folder" -->* represents to get
	 * all properties data in that object
	 * "properties.isRead eq false and typeId ne cmis:folder"
	 * "properties.orderId gt 100 properties.purchaseOrder ge 100"
	 * "startswith (name::'a') and properties.orderId lt 100"
	 * "properties.orderId le 100"
	 * 
	 * example order: "name asc, repositoryId", "name desc"
	 * 
	 * @see com.pogeyan.cmis.api.data.services.MNavigationServiceDAO#getDescendants(
	 * java .lang.String, java.lang.String[], boolean,java.lang.String[],
	 * java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public List<MBaseObject> getDescendants(String path, String[] principalIds, boolean aclPropagation,
			String[] mappedColumns, String filterExpression) {
		Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
		Query<MBaseObject> query = createQuery().disableValidation().filter("internalPath =", exp)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (!StringUtils.isEmpty(filterExpression)) {
			try {
				FilterExpression expression = UriParser.parseFilter(filterExpression);
				query = (Query<MBaseObject>) expression.accept(new MongoExpressionVisitor<MBaseObject>(query));
			} catch (ExpressionParserException | ExceptionVisitExpression e) {
			}
		}
		if (mappedColumns != null && mappedColumns.length > 0) {
			query = query.retrievedFields(true, mappedColumns);
		}
		if (aclPropagation) {
			query.or(getAclCriteria(principalIds, query));
		}
		return query.asList();

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
		Criteria[] checkAclRepo = new Criteria[] {
				query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.REPOSITORYDETERMINED.toString()) };
		Criteria[] result = ArrayUtils.addAll(checkAclRepo, checkAcl);
		return result;
	}

}
