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
import com.amazonaws.services.dynamodbv2.model.ConditionalOperator;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MNavigationDocServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.uri.UriParser;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;
import com.pogeyan.cmis.data.dynamo.DBaseObject;
import com.pogeyan.cmis.data.dynamo.DDocumentObject;
import com.pogeyan.cmis.data.dynamo.DynamoRepo;

public class DNavigationDocServiceImpl implements MNavigationDocServiceDAO {

	private DynamoDB dynamo;
	private String tableName;
	private Table table;

	public DNavigationDocServiceImpl(Class<DDocumentObject> entityClass, DynamoRepo ds) {
		tableName = ds.getRepoId() + "_types";
		dynamo = ds.getDynamoDb();
		table = dynamo.getTable(tableName);
	}

	@Override
	public List<? extends IDocumentObject> getChildren(String path, String[] principalIds, boolean aclPropagation,
			int maxItems, int skipCount, String orderBy, String[] mappedColumns, String filterExpression,
			MTypeManagerDAO typeManager, String repositoryId, String typeId) {

		List<DDocumentObject> list = new ArrayList<DDocumentObject>();

		QuerySpec spec = new QuerySpec().withFilterExpression("internalPath = :p")
				.withFilterExpression("attributes.changeType <> :val ")
				.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()).withString(":p", path))
				.withScanIndexForward(true);

		if (!StringUtils.isEmpty(orderBy)) {
			if (this.isOrderByParsable(orderBy)) {
				// dont know what to do
			}

			Index index = table.getIndex(orderBy + "Index");

			ItemCollection<QueryOutcome> items = index.query(spec);
			for (Item item : items) {
				DBaseObject baseObject = new DBaseObject(item.getString("PK"), item.getString("SK"),
						item.getString("time"), item.getString("name"), item.getString("path"),
						item.getString("internalPath"), (Map<String, Object>) item.get("attributes"));

				DDocumentObject dDocumentObject = new DDocumentObject(baseObject, (Boolean) item.get("isImmutable"),
						(Boolean) item.get("isLatestVersion"), (Boolean) item.get("isMajorVersion"),
						(Boolean) item.get("isLatestMajorVersion"), (Boolean) item.get("isPrivateWorkingCopy"),
						(String) item.get("versionLabel"), (String) item.get("versionSeriesId"),
						(String) item.get("versionReferenceId"), (Boolean) item.get("isVersionSeriesCheckedOut"),
						(String) item.get("versionSeriesCheckedOutBy"), (String) item.get("versionSeriesCheckedOutId"),
						(String) item.get("checkinComment"), (Long) item.get("contentStreamLength"),
						(String) item.get("contentStreamMimeType"), (String) item.get("contentStreamFileName"),
						(String) item.get("contentStreamId"), (String) item.get("previousVersionObjectId"));
				list.add(dDocumentObject);
			}

		}

		return list;
	}

	private boolean isOrderByParsable(String orderByExpressionQuery) {
		try {
			OrderByExpression orderByExpression = UriParser.parseOrderBy(orderByExpressionQuery);
			return true;
		} catch (ExpressionParserException e) {
			return false;
		}
	}

	@Override
	public long getChildrenSize(String path, String[] principalIds, boolean aclPropagation, String repositoryId,
			String typeId, String filterExpression, MTypeManagerDAO typeManager) {
		
		
		
		QuerySpec spec = new QuerySpec().withFilterExpression("internalPath = :p")
				.withFilterExpression("attributes.changeType <> :val ")
				.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()).withString(":p", path))
				.withScanIndexForward(true);
		
		if (!StringUtils.isEmpty(filterExpression)) {
			
		}
		
		ItemCollection<QueryOutcome> items = table.query(spec);
		if (aclPropagation) {
			//todo
		} 
		
		
		
		return items.getAccumulatedItemCount();
	}
	

	@Override
	public List<? extends IDocumentObject> getDescendants(String path, String[] principalIds, boolean aclPropagation,
			String[] mappedColumns, String filterExpression, MTypeManagerDAO typeManager) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<? extends IDocumentObject> getFolderTreeIds(String path, String[] principalIds,
			boolean aclPropagation) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<? extends IDocumentObject> getObjects(List<String> objectIds, String[] mappedColumns,
			String[] principalIds, boolean aclPropagation, String repositoryId, String typeId) {
		
		ArrayList<DDocumentObject> list = new ArrayList<DDocumentObject>();

		QuerySpec spec = new QuerySpec().withFilterExpression("attributes.changeType <> :val")
				.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()));
		
		

		//pk required
		
		for(String objectId : objectIds) {
			spec = spec.withKeyConditionExpression("SK = :sk")
					.withConditionalOperator(ConditionalOperator.OR)
					.withValueMap(new ValueMap().withString(":sk", objectId));
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

		//so that can querry on SK
		Index index = table.getIndex("timeIndex");
		ItemCollection<QueryOutcome> items = index.query(spec);
		if (items.getAccumulatedItemCount() != 0) {
			for (Item item : items) {
				DBaseObject baseObject = new DBaseObject(item.getString("PK"), item.getString("SK"),
						item.getString("time"), item.getString("name"), item.getString("path"),
						item.getString("internalPath"), (Map<String, Object>) item.get("attributes"));

				DDocumentObject dDocumentObject = new DDocumentObject(baseObject, (Boolean) item.get("isImmutable"),
						(Boolean) item.get("isLatestVersion"), (Boolean) item.get("isMajorVersion"),
						(Boolean) item.get("isLatestMajorVersion"), (Boolean) item.get("isPrivateWorkingCopy"),
						(String) item.get("versionLabel"), (String) item.get("versionSeriesId"),
						(String) item.get("versionReferenceId"), (Boolean) item.get("isVersionSeriesCheckedOut"),
						(String) item.get("versionSeriesCheckedOutBy"), (String) item.get("versionSeriesCheckedOutId"),
						(String) item.get("checkinComment"), (Long) item.get("contentStreamLength"),
						(String) item.get("contentStreamMimeType"), (String) item.get("contentStreamFileName"),
						(String) item.get("contentStreamId"), (String) item.get("previousVersionObjectId"));
				list.add(dDocumentObject);
			}
		}

		return list;

	}

}
