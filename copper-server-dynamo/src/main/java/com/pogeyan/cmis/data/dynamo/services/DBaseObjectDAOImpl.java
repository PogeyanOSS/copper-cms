package com.pogeyan.cmis.data.dynamo.services;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Index;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.PrimaryKey;
import com.amazonaws.services.dynamodbv2.document.QueryFilter;
import com.amazonaws.services.dynamodbv2.document.QueryOutcome;
import com.amazonaws.services.dynamodbv2.document.ScanOutcome;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.UpdateItemOutcome;
import com.amazonaws.services.dynamodbv2.document.spec.DeleteItemSpec;
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.document.spec.UpdateItemSpec;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.amazonaws.services.dynamodbv2.model.ConditionalOperator;
import com.amazonaws.services.dynamodbv2.model.ReturnValue;
import com.amazonaws.services.dynamodbv2.model.ScanRequest;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.data.dynamo.DBaseObject;
import com.pogeyan.cmis.data.dynamo.DynamoRepo;

public class DBaseObjectDAOImpl implements MBaseObjectDAO {

	private DynamoDB dynamo;
	private String tableName;
	private Table table;

	public DBaseObjectDAOImpl(Class<DBaseObject> entityClass, DynamoRepo ds) {
		tableName = ds.getRepoId() + "_objectData";
		dynamo = ds.getDynamoDb();
		table = dynamo.getTable(tableName);
	}

	public void delete(String repositoryId, String[] principalIds, String objectId, boolean forceDelete,
			boolean aclPropagation, TokenImpl token, String typeId) {

		if (aclPropagation) {
			// to do
		}
		if (forceDelete) {

			DeleteItemSpec deleteItemSpec = new DeleteItemSpec()
					.withPrimaryKey(new PrimaryKey("PK", typeId, "SK", objectId))
					.withConditionExpression("attributes.changeType = :val")
					.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()));

			try {
				table.deleteItem(deleteItemSpec);
			} catch (Exception e) {
				System.err.println("Unable to delete item: " + typeId + " " + objectId);
				System.err.println(e.getMessage());
			}

		} else {
			UpdateItemSpec updateItemSpec = new UpdateItemSpec()
					.withPrimaryKey(new PrimaryKey("PK", typeId, "SK", objectId))
					.withUpdateExpression(
							"set attributes.changetType = :valCt , time = :valT , attributes.modifiedAt = :valT")
					.withValueMap(new ValueMap().withInt(":valCt", token.getChangeType().value()).withString(":valT",
							token.getTime().toString()))
					.withReturnValues(ReturnValue.UPDATED_NEW);

			try {
				System.out.println("Updating the item...");
				UpdateItemOutcome outcome = table.updateItem(updateItemSpec);
				System.out.println("UpdateItem succeeded:\n" + outcome.getItem().toJSONPretty());

			} catch (Exception e) {
				System.err.println("Unable to update item: " + typeId + " " + objectId);
				System.err.println(e.getMessage());
			}

		}

	}

	// done
	public void update(String repositoryId, String objectId, Map<String, Object> updateProps, String typeId) {
		if (updateProps.get("acl") != null) {
			Map<String, Object> dAcl = DBaseObject.convertDynamoAcl((AccessControlListImplExt) updateProps.get("acl"));
			updateProps.remove("acl");
			updateProps.put("acl", dAcl);
		}

		if (updateProps.get("token") != null) {
			TokenImpl token = (TokenImpl) updateProps.get("token");

			updateProps.remove("token");
			updateProps.put("time", token.getTime().toString());
			updateProps.put("changeType", token.getChangeType().value());

		}

		for (Map.Entry<String, Object> entry : updateProps.entrySet()) {

			String property = null;

			if (entry.getKey().toString().equals("typeId")) {
				property = "PK";
			} else if (entry.getKey().toString().equals("id")) {
				property = "SK";
			} else if ((entry.getKey().toString().equals("time")) || (entry.getKey().toString().equals("name"))
					|| (entry.getKey().toString().equals("path"))
					|| (entry.getKey().toString().equals("internalPath"))) {
				property = entry.getKey().toString();
			} else {
				property = "attributes." + entry.getKey().toString();
			}

			UpdateItemSpec updateItemSpec = new UpdateItemSpec().withPrimaryKey("PK", typeId, "SK", objectId)
					.withConditionExpression("attributes.token.changeType != :val")
					.withUpdateExpression("set :p = :val")
					.withValueMap(new ValueMap().withStringSet(":p", property).with(":val", entry.getValue()));

			table.updateItem(updateItemSpec);
		}
	}

	// done?
	public DBaseObject getLatestToken() {
		ScanSpec scanSpec = new ScanSpec();
		QuerySpec spec = new QuerySpec().withScanIndexForward(false);

		Index index = table.getIndex("timeIndex");

		ItemCollection<ScanOutcome> items = index.scan(scanSpec);

		Iterator<Item> iterator = items.iterator();

		Item item = iterator.next();

		DBaseObject dItem = new DBaseObject(item.getString("PK"), item.getString("SK"), item.getString("time"),
				item.getString("name"), item.getString("path"), item.getString("internalPath"),
				(Map<String, Object>) item.get("atrributes"));

		return dItem;
	}

	// done
	public List<DBaseObject> filter(Map<String, Object> fieldNames, String[] principalIds, boolean aclPropagation,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns, String typeId) {

		List<DBaseObject> list = new ArrayList<DBaseObject>();
		QuerySpec spec = new QuerySpec().withFilterExpression("attributes.changeType <> :val")
				.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()));

		for (Map.Entry<String, Object> entry : fieldNames.entrySet()) {

			// fieldName
			String property = null;
			Boolean token = false;
			// check if PK, SK or attributes
			
			if (entry.getKey().toString().equals("typeId")) {
				property = "PK";
			} else if (entry.getKey().toString().equals("id")) {
				property = "SK";
			} else if (entry.getKey().toString().equals("token")) {
				token = true;
			} else if ((entry.getKey().toString().equals("name")) || (entry.getKey().toString().equals("path"))
					|| (entry.getKey().toString().equals("internalPath"))) {
				property = entry.getKey().toString();
			} else {
				property = "attributes." + entry.getKey().toString();
			}

			// filterExpression
			if (token) {
				String time = ((TokenImpl) entry.getValue()).getTime().toString();
				int changeType = ((TokenImpl) entry.getValue()).getChangeType().value();
				spec = spec.withFilterExpression("time = " + time + ", attributes.changeType = :val")
						.withValueMap(new ValueMap().withInt(":val", changeType));
			} else {
				spec = spec.withFilterExpression(":p = :val")
						.withValueMap(new ValueMap().withStringSet(":p", property).with(":val", entry.getValue()));
			}

		}
		if (includePagination) {
			if (maxItems > 0) {
				spec.withMaxResultSize(maxItems);
			}
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

	// done
	public void commit(IBaseObject entity, String typeId) {
		DBaseObject dEntity = (DBaseObject) entity;
		Item item = new Item().withPrimaryKey("PK", dEntity.getPk(), "SK", dEntity.getSk())
				.withString("time", dEntity.getTime()).withString("name", dEntity.getName())
				.withString("path", dEntity.getPath()).withString("internalPath", dEntity.getInternalPath())
				.with("atrributes", dEntity.getAttributes());
		int count = 0;
		while(count <= 10) {
			try {
				if (!table.describe().getTableStatus().equals("ACTIVE")) {
					Thread.sleep(1000);
					count ++;
				} else {
					break;
				}
			} catch (InterruptedException e) {
				
			}
		}
		table.putItem(item);
	}

	// done
	public IBaseObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId) {
		return new DBaseObject(name, baseId, typeId, fRepositoryId, secondaryTypeIds, description, createdBy,
				modifiedBy, token, internalPath, properties, policies, acl, path, parentId);
	}

	@Override
	public List<? extends IBaseObject> getObjects(List<String> objectIds, String[] principalIds, boolean aclPropagation,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns, String typeId) {
		List<DBaseObject> list = new ArrayList<DBaseObject>();
		QueryFilter queryFilter = new QueryFilter("SK").in(objectIds);
		QuerySpec spec = new QuerySpec().withFilterExpression("attributes.changeType <> :val ")
				.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value()))
				.withQueryFilters(queryFilter);
		if (includePagination) {
			if (maxItems > 0) {
				spec = spec.withMaxResultSize(maxItems);
			}
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
			spec.withConditionalOperator(ConditionalOperator.OR);
		}

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

}
