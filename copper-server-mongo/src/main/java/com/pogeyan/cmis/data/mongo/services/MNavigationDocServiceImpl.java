package com.pogeyan.cmis.data.mongo.services;

import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.CriteriaContainerImpl;
import org.mongodb.morphia.query.Query;

import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.services.MNavigationDocServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.uri.UriParser;
import com.pogeyan.cmis.api.uri.expression.ExceptionVisitExpression;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;
import com.pogeyan.cmis.data.mongo.MDocumentObject;
import com.pogeyan.cmis.data.mongo.MongoExpressionVisitor;

public class MNavigationDocServiceImpl extends BasicDAO<MDocumentObject, ObjectId> implements MNavigationDocServiceDAO {
	public MNavigationDocServiceImpl(Class<MDocumentObject> entityClass, Datastore ds) {
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
	@SuppressWarnings("unchecked")
	@Override
	public List<MDocumentObject> getChildren(String path, String[] principalIds, boolean aclPropagation, int maxItems,
			int skipCount, String orderBy, String[] mappedColumns, String filterExpression, MTypeManagerDAO typeManager,
			String repositoryId, String typeId) {
		Query<MDocumentObject> query = createQuery().disableValidation().filter("internalPath", path)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (!StringUtils.isEmpty(orderBy)) {
			if (this.isOrderByParsable(orderBy)) {
				try {
					OrderByExpression orderByExpression = UriParser.parseOrderBy(orderBy);
					query = (Query<MDocumentObject>) orderByExpression
							.accept(new MongoExpressionVisitor<MDocumentObject>(query, typeManager));
				} catch (ExpressionParserException | ExceptionVisitExpression e) {
				}
			} else {
				query = query.order(orderBy);
			}
		}
		if (!StringUtils.isEmpty(filterExpression)) {
			try {
				FilterExpression expression = UriParser.parseFilter(filterExpression);
				query = (Query<MDocumentObject>) expression
						.accept(new MongoExpressionVisitor<MDocumentObject>(query, typeManager));
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

	@SuppressWarnings("unchecked")
	@Override
	public long getChildrenSize(String path, String[] principalIds, boolean aclPropagation, String repositoryId,
			String typeId, String filterExpression, MTypeManagerDAO typeManager) {
		Query<MDocumentObject> query = createQuery().disableValidation().filter("internalPath", path)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (!StringUtils.isEmpty(filterExpression)) {
			try {
				FilterExpression expression = UriParser.parseFilter(filterExpression);
				query = (Query<MDocumentObject>) expression
						.accept(new MongoExpressionVisitor<MDocumentObject>(query, typeManager));
			} catch (ExpressionParserException | ExceptionVisitExpression e) {
			}
		}
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
	public List<MDocumentObject> getDescendants(String path, String[] principalIds, boolean aclPropagation,
			String[] mappedColumns, String filterExpression, MTypeManagerDAO typeManager) {
		Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
		Query<MDocumentObject> query = createQuery().disableValidation().filter("internalPath =", exp)
				.field("token.changeType").notEqual(TokenChangeType.DELETED.value());
		if (!StringUtils.isEmpty(filterExpression)) {
			try {
				FilterExpression expression = UriParser.parseFilter(filterExpression);
				query = (Query<MDocumentObject>) expression
						.accept(new MongoExpressionVisitor<MDocumentObject>(query, typeManager));
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
	public List<MDocumentObject> getFolderTreeIds(String path, String[] principalIds, boolean aclPropagation) {
		if (aclPropagation) {
			Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
			Query<MDocumentObject> query = createQuery().disableValidation().filter("internalPath =", exp)
					.field("baseId").equalIgnoreCase("cmis:folder").field("token.changeType")
					.notEqual(TokenChangeType.DELETED.value());
			query.or(getAclCriteria(principalIds, query));
			return query.asList();
		} else {
			Pattern exp = Pattern.compile(path, Pattern.CASE_INSENSITIVE);
			Query<MDocumentObject> query = createQuery().disableValidation().filter("internalPath =", exp)
					.field("baseId").equalIgnoreCase("cmis:folder").field("token.changeType")
					.notEqual(TokenChangeType.DELETED.value());
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
		Criteria[] checkAclRepo = new Criteria[] {
				query.criteria("acl.aclPropagation").equalIgnoreCase(AclPropagation.REPOSITORYDETERMINED.toString()) };
		Criteria[] result = ArrayUtils.addAll(checkAclRepo, checkAcl);
		return result;
	}

	@Override
	public List<MDocumentObject> getObjects(List<String> objectIds, String[] mappedColumns, String[] principalIds,
			boolean aclPropagation, String repositoryId, String typeId) {
		Query<MDocumentObject> query = createQuery().disableValidation().field("token.changeType")
				.notEqual(TokenChangeType.DELETED.value());
		query = query.field("id").in(objectIds);

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
}
