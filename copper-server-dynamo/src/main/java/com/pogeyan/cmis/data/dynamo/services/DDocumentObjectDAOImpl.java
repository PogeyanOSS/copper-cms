package com.pogeyan.cmis.data.dynamo.services;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.PrimaryKey;
import com.amazonaws.services.dynamodbv2.document.QueryOutcome;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.UpdateItemOutcome;
import com.amazonaws.services.dynamodbv2.document.spec.DeleteItemSpec;
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.spec.UpdateItemSpec;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.amazonaws.services.dynamodbv2.model.ReturnValue;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.data.dynamo.DBaseObject;
import com.pogeyan.cmis.data.dynamo.DDocumentObject;
import com.pogeyan.cmis.data.dynamo.DynamoRepo;

public class DDocumentObjectDAOImpl implements MDocumentObjectDAO {

	private DynamoDB dynamo;
	private String tableName;
	private Table table;

	public DDocumentObjectDAOImpl(Class<DDocumentObject> entityClass, DynamoRepo ds) {
		tableName = ds.getRepoId() + "_objectData";
		dynamo = ds.getDynamoDb();
		table = dynamo.getTable(tableName);
	}

	// must override
	public void delete(String objectId, List<String> removeProps, boolean forceDelete, boolean removefields,
			TokenImpl token, String typeId) {

		if (forceDelete) {
//need both PK and SK to perform delete operation
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

	public void update(String objectId, Map<String, Object> updateProps, String typeId) {
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

	@Override
	public List<? extends IDocumentObject> getCheckOutDocs(String folderId, String[] principalIds,
			boolean aclPropagation, int maxItems, int skipCount, String orderBy) {
	
		ArrayList<DDocumentObject> list = new ArrayList<DDocumentObject>();
		QuerySpec spec;

		if (folderId == null) {
			spec = new QuerySpec().withKeyConditionExpression("PK = :pk")
					.withFilterExpression("attributes.changeType <> :val")
					.withFilterExpression("isPrivateWorkingCopy = :working")
					.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value())
							.withString(":pk", "cmis:document").withBoolean(":working", true));
		} else {
			spec = new QuerySpec().withKeyConditionExpression("PK = :pk")
					.withFilterExpression("attributes.changeType <> :val").withFilterExpression("parentId = :folderId")
					.withFilterExpression("isPrivateWorkingCopy = :working")
					.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value())
							.withString(":pk", "cmis:document").withBoolean(":working", true)
							.withString(":folderId", folderId));
		}
		if (orderBy != null) {
			//todo
			//query = query.order("-" + orderBy);
		}
		if (maxItems > 0) {
			spec = spec.withMaxResultSize(maxItems);
		}
		if (aclPropagation) {
			//todo
			
		} 
		
		ItemCollection<QueryOutcome> items = table.query(spec);
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

	@Override
	public long getCheckOutDocsSize(String folderId, String[] principalIds, boolean aclPropagation) {
		ArrayList<DDocumentObject> list = new ArrayList<DDocumentObject>();
		QuerySpec spec;

		if (folderId == null) {
			spec = new QuerySpec().withKeyConditionExpression("PK = :pk")
					.withFilterExpression("attributes.changeType <> :val")
					.withFilterExpression("isPrivateWorkingCopy = :working")
					.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value())
							.withString(":pk", "cmis:document").withBoolean(":working", true));
		} else {
			spec = new QuerySpec().withKeyConditionExpression("PK = :pk")
					.withFilterExpression("attributes.changeType <> :val").withFilterExpression("parentId = :folderId")
					.withFilterExpression("isPrivateWorkingCopy = :working")
					.withValueMap(new ValueMap().withInt(":val", TokenChangeType.DELETED.value())
							.withString(":pk", "cmis:document").withBoolean(":working", true)
							.withString(":folderId", folderId));
		}
		if (aclPropagation) {
			//todo
			
		} 
		
		ItemCollection<QueryOutcome> items = table.query(spec);
		return items.getAccumulatedItemCount();
	}

	@Override
	public List<DDocumentObject> filter(Map<String, Object> fieldNames, String[] mappedColumns) {

		ArrayList<DDocumentObject> list = new ArrayList<DDocumentObject>();
		
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
				String expression = property + " = " + entry.getValue().toString();
				spec = spec.withFilterExpression(":p = :val")
						.withValueMap(new ValueMap().withStringSet(":p", property).with(":val", entry.getValue()));
			}

		}

		// can I remove attributes
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
		ItemCollection<QueryOutcome> items = table.query(spec);
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

	@Override
	public void commit(IDocumentObject entity) {
		DDocumentObject dEntity = (DDocumentObject) entity;
		Item item = new Item().withPrimaryKey("PK", dEntity.getPk(), "SK", dEntity.getSk())
				.withString("time", dEntity.getTime()).withString("name", dEntity.getName())
				.withString("path", dEntity.getPath()).withString("internalPath", dEntity.getInternalPath())
				.with("atrributes", dEntity.getAttributes()).withBoolean("isImmutable", dEntity.getIsImmutable())
				.withBoolean("isLatestVersion", dEntity.getIsLatestVersion())
				.withBoolean("isMajorVersion", dEntity.getIsMajorVersion())
				.withBoolean("isLatestMajorVersion", dEntity.getIsLatestMajorVersion())
				.withBoolean("isPrivateWorkingCopy", dEntity.getIsPrivateWorkingCopy())
				.withString("versionLabel", dEntity.getVersionLabel())
				.withString("versionSeriesId", dEntity.getVersionSeriesId())
				.withString("versionReferenceId", dEntity.getVersionReferenceId())
				.withBoolean("isVersionSeriesCheckedOut", dEntity.getIsVersionSeriesCheckedOut())
				.withString("versionSeriesCheckedOutBy", dEntity.getVersionSeriesCheckedOutBy())
				.withString("versionSeriesCheckedOutId", dEntity.getVersionSeriesCheckedOutId())
				.withString("checkinComment", dEntity.getCheckinComment())
				.withLong("contentStreamLength", dEntity.getContentStreamLength())
				.withString("contentStreamMimeType", dEntity.getContentStreamMimeType())
				.withString("contentStreamFileName", dEntity.getContentStreamFileName())
				.withString("contentStreamId", dEntity.getContentStreamId())
				.withString("previousVersionObjectId", dEntity.getPreviousVersionObjectId());

	}

	@Override
	public IDocumentObject createObjectFacade(IBaseObject baseObject, Boolean isImmutable, Boolean isLatestVersion,
			Boolean isMajorVersion, Boolean isLatestMajorVersion, Boolean isPrivateWorkingCopy, String versionLabel,
			String versionSeriesId, String versionReferenceId, Boolean isVersionSeriesCheckedOut,
			String versionSeriesCheckedOutBy, String versionSeriesCheckedOutId, String checkinComment,
			Long contentStreamLength, String contentStreamMimeType, String contentStreamFileName,
			String contentStreamId, String previousVersionObjectId) {

		DDocumentObject dDocumentObject = new DDocumentObject((DBaseObject) baseObject, isImmutable, isLatestVersion,
				isMajorVersion, isLatestMajorVersion, isPrivateWorkingCopy, versionLabel, versionSeriesId,
				versionReferenceId, isVersionSeriesCheckedOut, versionSeriesCheckedOutBy, versionSeriesCheckedOutId,
				checkinComment, contentStreamLength, contentStreamMimeType, contentStreamFileName, contentStreamId,
				previousVersionObjectId);

		return dDocumentObject;
	}

	
	//these methods have to be changed on the actor level
	@Override
	public void delete(String objectId, List<String> removeProps, boolean forceDelete, boolean removefields,
			TokenImpl token) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void update(String objectId, Map<String, Object> updateProps) {
		// TODO Auto-generated method stub
		
	}

}
