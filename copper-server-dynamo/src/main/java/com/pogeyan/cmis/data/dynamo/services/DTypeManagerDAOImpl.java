package com.pogeyan.cmis.data.dynamo.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;

import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.QueryOutcome;
import com.amazonaws.services.dynamodbv2.document.ScanOutcome;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.spec.DeleteItemSpec;
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.document.utils.NameMap;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.amazonaws.services.dynamodbv2.model.ConditionalOperator;
import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.data.dynamo.DTypeDocumentObject;
import com.pogeyan.cmis.data.dynamo.DTypeObject;
import com.pogeyan.cmis.data.dynamo.DynamoRepo;

public class DTypeManagerDAOImpl implements MTypeManagerDAO {

	// hardcoded should be a part of clinet factroy

	private DynamoDB dynamo;
	private String tableName;
	private Table table;

	public DTypeManagerDAOImpl(Class<DTypeObject> entityClass, DynamoRepo ds) {
		tableName = ds.getRepoId() + "_type";
		dynamo = ds.getDynamoDb();
		table = dynamo.getTable(tableName);
	}

	// done
	public List<DTypeObject> getById(List<?> typeId, String[] fieldAccess) {
		List<DTypeObject> list = new ArrayList<DTypeObject>();
		QuerySpec query = new QuerySpec();
		if (typeId == null) {
			ScanSpec scanSpec = new ScanSpec();
			if (fieldAccess != null) {
				for (String field : fieldAccess) {
					scanSpec.withProjectionExpression(field);
				}
			}

			ItemCollection<ScanOutcome> items = table.scan(scanSpec);

			if (items.getAccumulatedItemCount() == 0) {
				return null;
			}
			for (Item item : items) {
				DTypeObject dTypeObject = new DTypeObject(item.getString("PK"), item.getString("SK"),
						(Map<String, Object>) item.get("attributes"));
				list.add(dTypeObject);
			}

			return list;
		} else if (typeId.size() == 1) {
			query.withKeyConditionExpression("#n1 = :id").withNameMap(new NameMap().with("#n1", "PK")).withValueMap(new ValueMap().withString(":id", typeId.get(0).toString()));
		} else {
			query.withConditionalOperator(ConditionalOperator.OR);
			for (Object id : typeId) {
				query.withKeyConditionExpression("PK = :id").withValueMap(new ValueMap().withString(":id", id.toString()));
			}
		}

		if (fieldAccess != null) {
			for (String field : fieldAccess) {
				query.withProjectionExpression(field);
			}
		}

		ItemCollection<QueryOutcome> items = table.query(query);
		if (items.getAccumulatedItemCount() == 0) {
			return list;
		}
		for (Item item : items) {
			DTypeObject dTypeObject = new DTypeObject(item.getString("PK"), item.getString("SK"),
					(Map<String, Object>) item.get("attributes"));
			list.add(dTypeObject);
		}

		return list;
	}

	// done
	public void delete(String typeId) {
		DeleteItemSpec deleteItemSpec = new DeleteItemSpec().withPrimaryKey("PK", typeId);
		table.deleteItem(deleteItemSpec);
	}

	// done
	@SuppressWarnings("deprecation")
	public List<DTypeObject> getChildrenIds(String parentId, int maxItems, int skipCount) {

		int count = 0;

		List<DTypeObject> list = new ArrayList<DTypeObject>();

		QuerySpec spec = new QuerySpec().withFilterExpression("atrributes.parent = :val")
				.withValueMap(new ValueMap().withString(":val", parentId));

		ItemCollection<QueryOutcome> items = table.query(spec);

		for (Item item : items) {
			if (count < maxItems) {
				DTypeObject dTypeObject = new DTypeObject(item.getString("PK"), item.getString("SK"),
						(Map<String, Object>) item.get("attributes"));
				list.add(dTypeObject);
				count++;
			}
		}

		return list;
	}

	public void commit(TypeDefinition entity) {
		DTypeObject dEntity = (DTypeObject) entity;
		Item item = new Item().withPrimaryKey("PK", dEntity.getPk(), "SK", dEntity.getSk()).withMap("attributes",
				dEntity.getAttributes());
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

	public TypeDefinition createObjectFacade(String id, String localName, String localNamespace, String displayName,
			String queryName, String description, BaseTypeId baseTypeId, String parent, Boolean isCreatable,
			Boolean isFileable, Boolean isQueryable, Boolean isFulltextIndexed, Boolean isIncludedInSupertypeQuery,
			Boolean isControllablePolicy, Boolean isControllableAcl, TypeMutability typeMutability,
			Map<String, PropertyDefinitionImpl<?>> propertyDefinition, Boolean isVersion,
			ContentStreamAllowed contentStream) {
		if (isVersion != null || contentStream != null) {
			return new DTypeDocumentObject(id, localName, localNamespace, displayName, queryName, description,
					baseTypeId, parent, isCreatable, isFileable, isQueryable, isFulltextIndexed,
					isIncludedInSupertypeQuery, isControllablePolicy, isControllableAcl,
					(TypeMutabilityImpl) typeMutability, propertyDefinition, isVersion, contentStream);
		} else {
			return new DTypeObject(id, localName, localNamespace, displayName, queryName, description, baseTypeId,
					parent, isCreatable, isFileable, isQueryable, isFulltextIndexed, isIncludedInSupertypeQuery,
					isControllablePolicy, isControllableAcl, (TypeMutabilityImpl) typeMutability, propertyDefinition);
		}

	}

	@Override
	public PropertyDefinition<?> getAllPropertyById(String propId, String[] fieldAccess) {
		QuerySpec query = new QuerySpec();
		String propDef = "propertyDefinition." + propId + ".id";
		if (fieldAccess != null) {
			if (Arrays.asList(fieldAccess).contains(propId)) {
				query = query.withFilterExpression(propDef + " = :propId")
						.withValueMap(new ValueMap().withString(":propId", propId));
			}
		} else {
			query = query.withFilterExpression(propDef + " = :propId")
					.withValueMap(new ValueMap().withString(":propId", propId));
		}

//		if (query != null && query.get() != null) {
//			return query.get().getPropertyDefinitions().get(propId);
//		} else {
//			return null;
//		}

		return null;
	}
}
