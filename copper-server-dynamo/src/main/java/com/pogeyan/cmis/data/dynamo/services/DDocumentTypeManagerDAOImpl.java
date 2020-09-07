package com.pogeyan.cmis.data.dynamo.services;

import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.ScanOutcome;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.data.dynamo.DTypeDocumentObject;
import com.pogeyan.cmis.data.dynamo.DynamoRepo;

public class DDocumentTypeManagerDAOImpl implements MDocumentTypeManagerDAO {

	private DynamoDB dynamo;
	private String tableName;
	private Table table;

	public DDocumentTypeManagerDAOImpl(Class<DTypeDocumentObject> entityClass, DynamoRepo ds) {
		tableName = ds.getRepoId() + "_types";
		dynamo = ds.getDynamoDb();
		table = dynamo.getTable(tableName);
	}

	public DTypeDocumentObject getByTypeId(String typeId, String[] fieldAccess) {
		ScanSpec scanSpec = new ScanSpec();
		if (fieldAccess != null) {
			for (String field : fieldAccess) {
				scanSpec.withProjectionExpression(field);
			}
		}
		scanSpec.withFilterExpression("PK = :pk").withValueMap(new
				  ValueMap().withString(":pk", typeId));
		ItemCollection<ScanOutcome> items = table.scan(scanSpec);

		if (items.getAccumulatedItemCount() == 0) {
			return null;
		}
		for (Item item : items) {
//			DTypeDocumentObject dTypeObject = new DTypeDocumentObject(item.getString("PK"), item.getString("SK"),
//					(Map<String, Object>) item.get("attributes"));
		}
		return null;

	  
	}
}
