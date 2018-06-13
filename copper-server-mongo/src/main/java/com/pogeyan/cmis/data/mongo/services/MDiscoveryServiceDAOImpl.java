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

import org.apache.commons.lang3.StringUtils;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Query;

import com.pogeyan.cmis.api.data.services.MDiscoveryServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.uri.UriParser;
import com.pogeyan.cmis.api.uri.expression.ExceptionVisitExpression;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;
import com.pogeyan.cmis.data.mongo.MBaseObject;
import com.pogeyan.cmis.data.mongo.MongoExpressionVisitor;

public class MDiscoveryServiceDAOImpl extends BasicDAO<MBaseObject, ObjectId> implements MDiscoveryServiceDAO {

	public MDiscoveryServiceDAOImpl(Class<MBaseObject> entityClass, Datastore ds) {
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
	 * 
	 * **Input Format** Double type properties.dummy eq 1528589317128l + d here if
	 * we pass double append d in the last place of value.
	 * 
	 * Long type properties.dummy eq 1528589317128l + l here if we pass double
	 * append l in the last place of value.
	 * 
	 * Decimal type properties.dummy eq 1528589317128l + m here if we pass double
	 * append m in the last place of value.
	 * 
	 */
	@SuppressWarnings({ "unchecked", "deprecation" })
	@Override
	public List<MBaseObject> getLatestChanges(long changeLogToken, int maxItems, String[] mappedColumns, String orderBy,
			String filterExpression, MTypeManagerDAO typeManager) {
		Query<MBaseObject> query = createQuery().disableValidation().filter("token.time >", changeLogToken);
		if (!StringUtils.isEmpty(orderBy)) {
			if (this.isOrderByParsable(orderBy)) {
				try {
					OrderByExpression orderByExpression = UriParser.parseOrderBy(orderBy);
					query = (Query<MBaseObject>) orderByExpression
							.accept(new MongoExpressionVisitor<MBaseObject>(query, typeManager));
				} catch (ExpressionParserException | ExceptionVisitExpression e) {
				}
			} else {
				query = query.order(orderBy);
			}
		}
		if (!StringUtils.isEmpty(filterExpression)) {
			try {
				FilterExpression expression = UriParser.parseFilter(filterExpression);
				query = (Query<MBaseObject>) expression
						.accept(new MongoExpressionVisitor<MBaseObject>(query, typeManager));
			} catch (ExpressionParserException | ExceptionVisitExpression e) {
			}
		}
		if (maxItems > 0) {
			query = query.limit(maxItems);
		}
		if (mappedColumns != null && mappedColumns.length > 0) {
			query = query.retrievedFields(true, mappedColumns);
		}
		return query.asList();
	}

	@SuppressWarnings({ "deprecation", "unchecked" })
	@Override
	public long getLatestTokenChildrenSize(long latestChangeToken, String filterExpression,
			MTypeManagerDAO typeManager) {
		Query<MBaseObject> query = createQuery().disableValidation().filter("token.time >", latestChangeToken);
		if (!StringUtils.isEmpty(filterExpression)) {
			try {
				FilterExpression expression = UriParser.parseFilter(filterExpression);
				query = (Query<MBaseObject>) expression
						.accept(new MongoExpressionVisitor<MBaseObject>(query, typeManager));
			} catch (ExpressionParserException | ExceptionVisitExpression e) {
			}
		}
		return query.countAll();
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

}
