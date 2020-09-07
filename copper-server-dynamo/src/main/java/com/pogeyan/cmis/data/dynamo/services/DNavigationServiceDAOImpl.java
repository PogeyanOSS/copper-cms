package com.pogeyan.cmis.data.dynamo.services;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Index;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.QueryOutcome;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.uri.UriParser;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;
import com.pogeyan.cmis.data.dynamo.DBaseObject;
import com.pogeyan.cmis.data.dynamo.DynamoRepo;

public class DNavigationServiceDAOImpl implements MNavigationServiceDAO {

	private DynamoDB dynamo;
	private String tableName;
	private Table table;

	public DNavigationServiceDAOImpl(Class<DBaseObject> entityClass, DynamoRepo ds) {
		tableName = ds.getRepoId() + "_objectData";
		dynamo = ds.getDynamoDb();
		table = dynamo.getTable(tableName);
	}

	@Override
	public List<? extends IBaseObject> getChildren(String path, String[] principalIds, boolean aclPropagation,
			int maxItems, int skipCount, String orderBy, String[] mappedColumns, String filterExpression,
			MTypeManagerDAO typeManager) {
		List<DBaseObject> list = new ArrayList<DBaseObject>();

		QuerySpec spec = new QuerySpec().withFilterExpression("internalPath = :p")
				.withFilterExpression("attributes.changeType <> :val ")
				.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()).withString(":p", path))
				.withScanIndexForward(true);

		if (!StringUtils.isEmpty(filterExpression)) {

		}
		
		if (maxItems > 0) {
			spec.withMaxResultSize(maxItems);
		}
		
		if (mappedColumns != null && mappedColumns.length > 0) {
			for (String col : mappedColumns) {
				if (col.equals("typeId")) {
					spec = spec.withProjectionExpression("PK");
				} else if (col.equals("id")) {
					spec = spec.withProjectionExpression("SK");
				} else if (col.equals("token")) {
					spec = spec.withProjectionExpression("time");
					spec = spec.withProjectionExpression("changeType");

				} else if ((col.equals("name")) || (col.equals("path")) || (col.equals("internalPath"))) {
					spec = spec.withProjectionExpression(col);
				} else {
					spec = spec.withProjectionExpression("attributes." + col);
				}

			}
		}
		if (aclPropagation) {
			// todo
		}

		Index index = table.getIndex(orderBy + "Index");

		ItemCollection<QueryOutcome> items = table.query(spec);
		if (items.getAccumulatedItemCount() != 0) {
			for (Item item : items) {
				DBaseObject dBaseObject = new DBaseObject(item.getString("PK"), item.getString("SK"),
						item.getString("time"), item.getString("name"), item.getString("path"),
						item.getString("internalPath"), (Map<String, Object>) item.get("attributes"));
				list.add(dBaseObject);
			}
		}

		return list;
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
		QuerySpec spec = new QuerySpec().withFilterExpression("internalPath = :p")
				.withFilterExpression("attributes.changeType <> :val ")
				.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()).withString(":p", path))
				.withScanIndexForward(true);
		
		ItemCollection<QueryOutcome> items = table.query(spec);
		if (aclPropagation) {
			//todo
		} 
		return items.getAccumulatedItemCount();
	}

	@Override
	public List<? extends IBaseObject> getDescendants(String path, String[] principalIds, boolean aclPropagation,
			String[] mappedColumns, String filterExpression, MTypeManagerDAO typeManager) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<? extends IBaseObject> getFolderTreeIds(String path, String[] principalIds, boolean aclPropagation) {
		// TODO Auto-generated method stub
		return null;
	}

}
