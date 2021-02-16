package com.pogeyan.cmis.data.mongo.services;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.bson.Document;
import org.bson.types.ObjectId;
import org.json.JSONArray;
import org.json.JSONObject;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoDatabase;
import com.pogeyan.cmis.api.CustomTypeId;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MQueryDAO;
import com.pogeyan.cmis.data.mongo.MBaseObject;
import com.pogeyan.cmis.data.query.FilterQueryRequest;
import com.pogeyan.cmis.data.query.IQueryResponse;
import com.pogeyan.cmis.data.query.QueryRequest;
import com.pogeyan.cmis.data.query.SortQueryRequest;
import com.pogeyan.cmis.impl.utils.DBUtils.Variables;

public class MQueryDAOImpl extends BasicDAO<MBaseObject, ObjectId> implements MQueryDAO {

	private static final Logger LOG = LoggerFactory.getLogger(MQueryDAOImpl.class);

	public MQueryDAOImpl(Class<MBaseObject> class1, Datastore ds) {
		super(class1, ds);
	}

	@SuppressWarnings({ "deprecation" })
	@Override
	public List<IQueryResponse> query(QueryRequest request, String[] principalIds, MBaseObjectDAO objectMorphiaDAO) {
		List<Document> document = new ArrayList<Document>();
		List<FilterQueryRequest> filterRequest = request.getFilter();
		List<SortQueryRequest> sortRequest = request.getSort();
		Document projections = new Document();
		Document projectionDocument = new Document();
		String sourceTypeId = null;
		String targetTypeId = null;
		Integer relationshipType = 0;
		String direction = null;
		Map<String, QueryRequest> fieldsRequestMap = request.getFields();
		if (!fieldsRequestMap.isEmpty()) {
			for (Entry<String, QueryRequest> fieldsRequest : fieldsRequestMap.entrySet()) {
				if (!fieldsRequest.getKey().contains(".")) {
					String key = fieldsRequest.getKey();
					IBaseObject baseObject = getRelationshipMd(principalIds, objectMorphiaDAO, key);
					if (baseObject != null) {
						sourceTypeId = (String) baseObject.getProperties().get(QueryAggregationConstants.SOURCE_TABLE);
						targetTypeId = (String) baseObject.getProperties().get(QueryAggregationConstants.TARGET_TABLE);
						relationshipType = (Integer) baseObject.getProperties()
								.get(QueryAggregationConstants.RELATION_TYPE);
						direction = fieldsRequest.getValue().getDirection();
					} else {
						sourceTypeId = fieldsRequest.getKey();
					}
					projectionDocument = getDefaultProjectDocument(fieldsRequest, projectionDocument, sourceTypeId,
							targetTypeId, direction);
					document = getLookupQuery(fieldsRequest, document, principalIds, sourceTypeId, targetTypeId,
							key, projectionDocument, objectMorphiaDAO);
				} else {
					String key = fieldsRequest.getKey();
					if (key.contains(".")) {
						String[] keys = key.split("\\.");
						String label = keys[0];
						String field = keys[1];
						String fieldKey = field;
						String fieldName = "$" + label + "." + getQueryName(field);
						projectionDocument.append(fieldKey, fieldName);
					} else {
						String fieldName = "$" + getQueryName(key);
						projectionDocument.append(key, fieldName);
					}
				}
			}
		}

		document = getQueryAggsPipeline(request, filterRequest, sortRequest, document, false, principalIds);

		if (projectionDocument != null && !projectionDocument.isEmpty()) {
			projections.append(QueryAggregationConstants.PROJECT, projectionDocument);
			document.add(projections);
		}
		List<IQueryResponse> result = new ArrayList<IQueryResponse>();
		String dBName = this.ds.getDB().getName();
		MongoDatabase db = this.ds.getMongo().getDatabase(dBName);
		MongoCursor<Document> iterator = db.getCollection(QueryAggregationConstants.COLLECTION_NAME).aggregate(document)
				.iterator();
		int totalObjects = getObjectCount(document);
		LOG.debug("Get Dynamic Relationship Query Result of iterator has next : {} ", iterator.hasNext());
		List<Document> list = new ArrayList<Document>();
		if (iterator.hasNext()) {
			iterator.forEachRemaining(list::add);
		}

		for (Document docs : list) {
			IQueryResponse respose = new IQueryResponse();
			for (Entry<String, Object> doc : docs.entrySet()) {
				respose.put(doc.getKey(), doc.getValue());
			}
			result.add(respose);
		}
		IQueryResponse response = new IQueryResponse();
		response.put("TotalObjects", totalObjects);
		result.add(response);
		LOG.debug("Get Response Dynamic for RelationShip Query Result : {} ", result);

		if (result != null && !result.isEmpty() && relationshipType.equals(1)) {
			List<IQueryResponse> finalResult = getGroupbyResponse(result);
			return finalResult;
		}
		return result;
	}

	private int getObjectCount(List<Document> document) {
		List<Document> countdoc = new ArrayList<Document>();
		for (Document doc : document) {
			if (doc.containsKey(QueryAggregationConstants.LIMIT)) {
				doc.remove(QueryAggregationConstants.LIMIT);
			} else if (doc.containsKey(QueryAggregationConstants.SKIP)) {
				doc.remove(QueryAggregationConstants.SKIP);
			} else {
				countdoc.add(doc);
			}
		}
		String dBName = this.ds.getDB().getName();
		MongoDatabase db = this.ds.getMongo().getDatabase(dBName);
		MongoCursor<Document> totalObjects = db.getCollection(QueryAggregationConstants.COLLECTION_NAME)
				.aggregate(countdoc).iterator();
		int count = 0;
		while (totalObjects.hasNext()) {
			totalObjects.next();
			count++;
		}
		return count;
	}

	private List<IQueryResponse> getGroupbyResponse(List<IQueryResponse> result) {
		List<IQueryResponse> finalResponse = new ArrayList<IQueryResponse>();
		for (IQueryResponse res : result) {
			boolean isGroup = false;
			IQueryResponse response = new IQueryResponse();
			for (Entry<String, Object> eMap : res.entrySet()) {
				String key = eMap.getKey();
				if (key.equals(PropertyIds.OBJECT_ID)) {
					for (IQueryResponse iQueryResponse : finalResponse) {
						if (iQueryResponse.get(key) != null && iQueryResponse.get(key).equals(eMap.getValue())) {
							isGroup = true;
							getNestedArray(key, res, iQueryResponse);
							break;
						}
					}
				}
				if (!isGroup) {
					if (eMap.getValue() instanceof Map) {
						Map<String, Object> map = getSubGroup(key, eMap.getValue(), null);
						List<Map<String, Object>> childArray = new ArrayList<Map<String, Object>>();
						childArray.add(map);
						response.put(key, childArray);

					} else {
						response.put(eMap.getKey(), eMap.getValue());
					}
				}
			}
			if (!isGroup) {
				finalResponse.add(response);
			}
		}
		return finalResponse;
	}

	@SuppressWarnings("unchecked")
	private Map<String, Object> getSubGroup(String parentKey, Object value, IQueryResponse finalResponse) {
		Map<String, Object> map = new HashMap<String, Object>();
		Object parentValue = getParentKeyValue(value);
		boolean isGroup = false;
		for (Entry<String, Object> sub : ((Document) value).entrySet()) {
			if (sub.getValue() instanceof Map) {
				Map<String, Object> cmap = getSubGroup(parentKey, sub.getValue(), finalResponse);
				Map<String, Object> parentObject = getParentObject(parentKey, parentValue, finalResponse);
				if (parentObject != null && parentObject.size() > 0) {
					List<Map<String, Object>> innerArray = (List<Map<String, Object>>) parentObject.get(sub.getKey());
					innerArray.add(cmap);
					isGroup = true;
					break;
				}
				if (!isGroup) {
					List<Map<String, Object>> innerArray = new ArrayList<Map<String, Object>>();
					innerArray.add(cmap);
					map.put(sub.getKey(), innerArray);
				}
			} else {
				map.put(sub.getKey(), sub.getValue());
			}
		}
		return map;
	}

	private Map<String, Object> getParentObject(String parentKey, Object parentValue, IQueryResponse finalResponse) {
		if (finalResponse != null && finalResponse.size() > 0) {
			for (Entry<String, Object> eMap : finalResponse.entrySet()) {
				if (eMap.getValue() instanceof List) {
					;
					List<Map<String, Object>> parentArray = ((List<Map<String, Object>>) finalResponse.get(parentKey));
					for (int i = 0; i < ((List<?>) eMap.getValue()).size(); i++) {
						String keyValue = (String) parentArray.get(i).get(PropertyIds.OBJECT_ID);
						if (keyValue.equals(parentValue)) {
							return (Map<String, Object>) parentArray.get(i);
						}
					}

				}
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	private void getNestedArray(String key, IQueryResponse res, IQueryResponse iQueryResponse) {
		for (Entry<String, Object> ent : res.entrySet()) {
			String entKey = ent.getKey();
			Object value = ent.getValue();
			boolean isGroup = false;
			if (value instanceof Map) {
				Map<String, Object> cmap = getSubGroup(entKey, value, iQueryResponse);
				String parentValue = getParentKeyValue(value);
				Map<String, Object> parentObject = getParentObject(entKey, parentValue, iQueryResponse);
				if (parentObject != null && parentObject.size() > 0) {
					isGroup = true;
					break;
				}
				if (!isGroup) {
					List<Map<String, Object>> parentArray = ((List<Map<String, Object>>) iQueryResponse.get(entKey));
					parentArray.add(cmap);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private String getParentKeyValue(Object Value) {
		Map<String, Object> subVlaue = (Map<String, Object>) Value;
		if (subVlaue.get(PropertyIds.OBJECT_ID) != null) {
			for (Entry<String, Object> sub : subVlaue.entrySet()) {
				if (sub.getKey().equals(PropertyIds.OBJECT_ID)) {
					return (String) sub.getValue();
				}
			}
		}
		return null;
	}

	@SuppressWarnings("serial")
	private IBaseObject getRelationshipMd(String[] principalIds, MBaseObjectDAO objectMorphiaDAO, String key) {
		Map<String, Object> fieldsNamesAndValues = new HashMap<String, Object>() {
			{
				put(Variables.NAME, key);
				put(Variables.TYPEID, CustomTypeId.CMIS_EXT_RELATIONMD.value());
			}
		};
		List<? extends IBaseObject> objects = objectMorphiaDAO.filter(fieldsNamesAndValues, principalIds, false, false,
				0, 0, null, null);
		if (objects.size() > 0) {
			return objects.get(0);
		}
		return null;
	}

	private Document getDefaultProjectDocument(Entry<String, QueryRequest> fieldsQuery, Document projectionDocument,
			String sourceTypeId, String targetTypeId, String direction) {
		String label = sourceTypeId;
		String objectId = "$" + label + "." + getQueryName(PropertyIds.OBJECT_ID);
		String objectTypeId = "$" + label + "." + getQueryName(PropertyIds.OBJECT_TYPE_ID);
		String objectName = "$" + label + "." + getQueryName(PropertyIds.NAME);
		if (direction != null && direction.equals(QueryAggregationConstants.TARGET)) {
			label = targetTypeId;
			objectId = "$" + label + "." + getQueryName(PropertyIds.OBJECT_ID);
			objectTypeId = "$" + label + "." + getQueryName(PropertyIds.OBJECT_TYPE_ID);
			objectName = "$" + label + "." + getQueryName(PropertyIds.NAME);
		}
		projectionDocument.append(PropertyIds.OBJECT_ID, objectId);
		projectionDocument.append(PropertyIds.OBJECT_TYPE_ID, objectTypeId);
		projectionDocument.append(PropertyIds.NAME, objectName);
		return projectionDocument;
	}

	private List<Document> getQueryAggsPipeline(QueryRequest request, List<FilterQueryRequest> filterRequest,
			List<SortQueryRequest> sortRequest, List<Document> document, boolean aclPropagation,
			String[] principalIds) {
		int limit = request.getSize();
		int pagination = request.getStep();
		Document filterQuery = new Document();
		Document sortQuery = new Document();
		Document limitQuery = new Document();
		Document offSetQuery = new Document();

		if (filterRequest != null && filterRequest.size() > 0 && !filterRequest.isEmpty()) {
			filterQuery = getFilterQuery(filterRequest, aclPropagation, principalIds);
		}

		if (sortRequest != null && sortRequest.size() > 0 && !sortRequest.isEmpty()) {
			sortQuery = getSortQuery(sortRequest);
		}

		if (filterQuery != null && !filterQuery.isEmpty()) {
			document.add(filterQuery);
		}

		if (sortQuery != null && !sortQuery.isEmpty()) {
			document.add(sortQuery);
		}

		if (limit != 0) {
			limitQuery.append(QueryAggregationConstants.LIMIT, limit);
			document.add(limitQuery);
		}

		if (pagination != 0) {
			offSetQuery.append(QueryAggregationConstants.SKIP, pagination);
			document.add(offSetQuery);
		}
		return document;
	}

	private List<Document> getLookupQuery(Entry<String, QueryRequest> fieldsQuery, List<Document> document,
			String[] principalIds, String sourceTypeId, String targetTypeId, String key, Document projectionDocument,
			MBaseObjectDAO objectMorphiaDAO) {
		Document rootProjection = new Document();
		Document filterDoc = new Document();
		Document operatorDoc = new Document();
		if (fieldsQuery.getValue() != null && (fieldsQuery.getValue().getDirection() != null
				&& fieldsQuery.getValue().getDirection().equals(QueryAggregationConstants.TARGET))) {
			rootProjection = new Document(QueryAggregationConstants.PROJECT,
					rootProjection.append(targetTypeId, QueryAggregationConstants.ROOT));
			document.add(rootProjection);
			operatorDoc = new Document(targetTypeId + "." + "token.changeType",
					new Document("$" + QueryAggregationConstants.NOTEQUAL, 2));
			filterDoc = new Document(QueryAggregationConstants.MATCH, operatorDoc);
			document.add(filterDoc);
		} else {
			rootProjection = new Document(QueryAggregationConstants.PROJECT,
					rootProjection.append(sourceTypeId, QueryAggregationConstants.ROOT));
			document.add(rootProjection);
			if (targetTypeId != null) {
			operatorDoc = new Document(sourceTypeId + "." + "token.changeType",
					new Document("$" + QueryAggregationConstants.NOTEQUAL, 2));
			} else {
				Document sourceLookupDoc = new Document();
				sourceLookupDoc.append(sourceTypeId + "." + "token.changeType",
						new Document("$" + QueryAggregationConstants.NOTEQUAL, 2));
				sourceLookupDoc.append(sourceTypeId + "." + "typeId", sourceTypeId);
				operatorDoc = sourceLookupDoc;
			}
			filterDoc = new Document(QueryAggregationConstants.MATCH, operatorDoc);
			document.add(filterDoc);
		}
		if (targetTypeId != null) {
			getNestedRelationShip(fieldsQuery, document, sourceTypeId, targetTypeId, projectionDocument, principalIds,
					objectMorphiaDAO);
		}

		document = getNestedQuery(fieldsQuery, document, principalIds);
		return document;
	}

	private List<Document> getNestedQuery(Entry<String, QueryRequest> fieldsQuery, List<Document> document,
			String[] principalIds) {
		if (fieldsQuery.getValue() != null && (fieldsQuery.getValue().getFields() != null
				|| fieldsQuery.getValue().getFilter() != null || fieldsQuery.getValue().getSort() != null)) {
			QueryRequest aggsRequest = fieldsQuery.getValue();
			List<FilterQueryRequest> filter = aggsRequest.getFilter();
			List<SortQueryRequest> sort = aggsRequest.getSort();
			document = getQueryAggsPipeline(aggsRequest, filter, sort, document, false, principalIds);
			if (fieldsQuery.getValue().getFields() != null) {
				for (Entry<String, QueryRequest> fields : fieldsQuery.getValue().getFields().entrySet()) {
					if (fields.getValue() != null && (fields.getValue().getFields() != null
							|| fields.getValue().getFilter() != null || fields.getValue().getSort() != null)) {
						getNestedQuery(fields, document, principalIds);
					}
				}
			}
		}
		return document;
	}

	private void getNestedRelationShip(Entry<String, QueryRequest> fieldsQuery, List<Document> document,
			String sourceTypeId, String targetTypeId, Document projectionDocument, String[] principalIds,
			MBaseObjectDAO objectMorphiaDAO) {
		Document nestedProject = new Document();
		if (fieldsQuery.getValue() != null && (fieldsQuery.getValue().getDirection() != null
				&& fieldsQuery.getValue().getDirection().equals(QueryAggregationConstants.TARGET))) {
			getLookupByDirection(document, targetTypeId, sourceTypeId, QueryAggregationConstants.TARGET, fieldsQuery,
					principalIds, objectMorphiaDAO);
			nestedProject = getProjectDocumentOnLookup(targetTypeId, sourceTypeId, QueryAggregationConstants.SOURCE,
					fieldsQuery, principalIds, objectMorphiaDAO);
			projectionDocument.append(sourceTypeId, nestedProject);
		} else {
			getLookupByDirection(document, sourceTypeId, targetTypeId, QueryAggregationConstants.SOURCE, fieldsQuery,
					principalIds, objectMorphiaDAO);
			nestedProject = getProjectDocumentOnLookup(targetTypeId, sourceTypeId, QueryAggregationConstants.TARGET,
					fieldsQuery, principalIds, objectMorphiaDAO);
			projectionDocument.append(targetTypeId, nestedProject);
		}

	}

	private Document getProjectDocumentOnLookup(String targetTypeId, String sourceTypeId, String direction,
			Entry<String, QueryRequest> fieldsQuery, String[] principalIds, MBaseObjectDAO objectMorphiaDAO) {
		Document projection = new Document();
		Document parent = new Document();
		if (fieldsQuery.getValue().getFields() != null) {
			for (Entry<String, QueryRequest> fields : fieldsQuery.getValue().getFields().entrySet()) {
				String key = fields.getKey();
				if (key.contains(".")) {
					String[] keys = key.split("\\.");
					String label = keys[0];
					String field = keys[1];
					String fieldKey = field;
					String fieldName = "$" + label + "." + getQueryName(field);
					projection.append(fieldKey, fieldName);
				}
			}

		}
		projection = getDefaultProjectDocument(fieldsQuery, projection, sourceTypeId, targetTypeId, direction);
		parent.putAll(projection);
		if (fieldsQuery.getValue() != null && fieldsQuery.getValue().getFields() != null) {
			for (Entry<String, QueryRequest> fields : fieldsQuery.getValue().getFields().entrySet()) {
				if (!fields.getKey().contains(".") && fields.getKey().contains("_")) {
					String key = fields.getKey();
					IBaseObject baseObject = getRelationshipMd(principalIds, objectMorphiaDAO, key);
					if (baseObject != null) {
						sourceTypeId = (String) baseObject.getProperties().get(QueryAggregationConstants.SOURCE_TABLE);
						targetTypeId = (String) baseObject.getProperties().get(QueryAggregationConstants.TARGET_TABLE);
						String order = fields.getValue().getDirection();

						if (order != null && order.equals(QueryAggregationConstants.TARGET)) {
							projection = getProjectDocumentOnLookup(targetTypeId, sourceTypeId,
									QueryAggregationConstants.SOURCE, fields, principalIds, objectMorphiaDAO);
							parent.append(sourceTypeId, projection);
						} else {
							projection = getProjectDocumentOnLookup(targetTypeId, sourceTypeId,
									QueryAggregationConstants.TARGET, fields, principalIds, objectMorphiaDAO);
							parent.append(targetTypeId, projection);
						}
					}
				}
			}
		}
		return parent;
	}

	private void getLookupByDirection(List<Document> document, String sourceTypeId, String targetTypeId,
			String direction, Entry<String, QueryRequest> fieldsQuery, String[] principalIds,
			MBaseObjectDAO objectMorphiaDAO) {
		String relationshipAlias = sourceTypeId + QueryAggregationConstants.RELATIONSHIP;
		if (direction != null && direction.equals(QueryAggregationConstants.TARGET)) {
			getLookupDocument(QueryAggregationConstants.RELATION_COLLECTION_NAME, sourceTypeId + "._id",
					getQueryName(QueryAggregationConstants.TARGET_ID), relationshipAlias, document);
			getLookupDocument(QueryAggregationConstants.COLLECTION_NAME,
					relationshipAlias + "." + getQueryName(QueryAggregationConstants.SOURCE_ID), "_id", targetTypeId,
					document);
		} else {
			getLookupDocument(QueryAggregationConstants.RELATION_COLLECTION_NAME, sourceTypeId + "._id",
					getQueryName(QueryAggregationConstants.SOURCE_ID), relationshipAlias, document);
			getLookupDocument(QueryAggregationConstants.COLLECTION_NAME,
					relationshipAlias + "." + getQueryName(QueryAggregationConstants.TARGET_ID), "_id", targetTypeId,
					document);
		}
		if (fieldsQuery.getValue() != null && fieldsQuery.getValue().getFields() != null) {
			for (Entry<String, QueryRequest> fields : fieldsQuery.getValue().getFields().entrySet()) {
				if (!fields.getKey().contains(".") && fields.getKey().contains("_")) {
					String key = fields.getKey();
					IBaseObject baseObject = getRelationshipMd(principalIds, objectMorphiaDAO, key);
					if (baseObject != null) {
						sourceTypeId = (String) baseObject.getProperties().get(QueryAggregationConstants.SOURCE_TABLE);
						targetTypeId = (String) baseObject.getProperties().get(QueryAggregationConstants.TARGET_TABLE);
						String order = fields.getValue().getDirection();
						if (order != null && order.equals(QueryAggregationConstants.TARGET)) {
							getLookupByDirection(document, targetTypeId, sourceTypeId, order, fields, principalIds,
									objectMorphiaDAO);
						} else {
							getLookupByDirection(document, sourceTypeId, targetTypeId, order, fields, principalIds,
									objectMorphiaDAO);
						}
					}
				}
			}
		}
	}

	private void getLookupDocument(String collectionName, String localField, String foreignField, String alias,
			List<Document> document) {
		Document lookupDocument = new Document();
		Document lookupObject = new Document();
		Document unwindObject = new Document();
		Document filterDoc = new Document();
		Document operatorDoc = new Document();
		lookupObject.append(QueryAggregationConstants.FROM, collectionName);
		lookupObject.append(QueryAggregationConstants.LOCAL_FIELD, localField);
		lookupObject.append(QueryAggregationConstants.FOREIGN_FIELD, foreignField);
		lookupObject.append(QueryAggregationConstants.ALIAS, alias);
		unwindObject.append(QueryAggregationConstants.UNWIND,
				new Document(QueryAggregationConstants.UNWIND_PATH, "$" + alias));
		lookupDocument.append(QueryAggregationConstants.LOOKUP, lookupObject);
		document.add(lookupDocument);
		document.add(unwindObject);
		// operatorDoc.append(alias + "." + "token.changeType", new Document("$" +
		// QueryAggregationConstants.NOTEQUAL, 2));
		if (localField.contains(".") && (localField.contains(QueryAggregationConstants.TARGET_ID)
				|| localField.contains(QueryAggregationConstants.SOURCE_ID))) {
			String local = localField.split("[.]")[0];
			String source = local.split(QueryAggregationConstants.RELATIONSHIP)[0];
			if (localField.contains(QueryAggregationConstants.TARGET_ID)) {
				operatorDoc.append(local + ".properties." + "relation_name", source + "_" + alias);
			} else {
				operatorDoc.append(local + ".properties." + "relation_name", alias + "_" + source);
			}
			filterDoc.append(QueryAggregationConstants.MATCH, operatorDoc);
			document.add(filterDoc);
		}
	}

	private Document getSortQuery(List<SortQueryRequest> sortRequest) {
		Document sortAggs = new Document();
		Document sortDoc = new Document();
		for (SortQueryRequest sortQuery : sortRequest) {
			String field = sortQuery.getField();
			String operator = sortQuery.getOperator();
			if (field.contains(".")) {
				String[] fieldArr = field.split("\\.");
				String fieldKey = fieldArr[0];
				String fieldName = fieldArr[1];
				field = fieldKey + "." + getQueryName(fieldName);
			} else {
				field = getQueryName(field);
			}
			if (operator.equals(QueryAggregationConstants.DESCENDING)) {
				sortDoc.append(field, -1);
			} else {
				sortDoc.append(field, 1);
			}
		}
		sortAggs.append(QueryAggregationConstants.SORT, sortDoc);
		return sortAggs;
	}

	@SuppressWarnings("unchecked")
	private Document getFilterQuery(List<FilterQueryRequest> filterRequest, boolean aclPropagation,
			String[] principalIds) {
		Document filterDoc = new Document();
		Document operatorDoc = new Document();
		List<Document> aclList = new ArrayList<Document>();
		for (FilterQueryRequest filter : filterRequest) {
			String operatorName = filter.getOperator();
			String field = filter.getField();
			Object value = filter.getValue();
			String type = filter.getType();
			if (field.contains(".")) {
				String[] fieldArr = field.split("\\.");
				String fieldKey = fieldArr[0];
				String fieldName = fieldArr[1];
				field = fieldKey + "." + getQueryName(fieldName);
			} else {
				field = getQueryName(field);
			}
			operatorDoc = getOperatorQuery(field, operatorName, value, operatorDoc, type);
		}
		if (aclPropagation) {
			List<Document> acl = new ArrayList<Document>();
			aclList = getAclCriteria(principalIds, aclList);
			if (operatorDoc.containsKey(QueryAggregationConstants.OR)) {
				acl = (ArrayList<Document>) operatorDoc.get(QueryAggregationConstants.OR);
				operatorDoc.remove(QueryAggregationConstants.OR);
				aclList.addAll(acl);
			}
			operatorDoc.append(QueryAggregationConstants.OR, aclList);
		}
		filterDoc.append(QueryAggregationConstants.MATCH, operatorDoc);
		return filterDoc;
	}

	private List<Document> getAclCriteria(String[] principalIds, List<Document> aclList) {
		for (String principalId : principalIds) {
			Document aclPrincipalId = new Document();
			Document acl = new Document();
			aclPrincipalId.append("$regex", "^" + principalId);
			aclPrincipalId.append("$options", "i");
			acl.append("acl.aces.principal.principalId", aclPrincipalId);
			aclList.add(acl);
		}
		return aclList;
	}

	private Document getOperatorQuery(String field, String operatorName, Object value, Document operatorDoc,
			String type) {

		if (value instanceof List) {
			value = (List<?>) value;
		} else if (value instanceof Integer) {
			value = (Integer) value;
		} else if (value instanceof Long) {
			value = (Long) value;
		} else if (value instanceof Boolean) {
			value = (Boolean) value;
		} else {
			value = (String) value;
		}

		switch (operatorName) {

		case QueryAggregationConstants.EQUAL:
			operatorDoc.append(field, value);
			break;

		case QueryAggregationConstants.NOTEQUAL:
			operatorDoc.append(field, new Document("$" + QueryAggregationConstants.NOTEQUAL, value));
			break;

		case QueryAggregationConstants.GREATERTHANEQUAL:
			operatorDoc.append(field, new Document("$" + QueryAggregationConstants.GREATERTHANEQUAL, value));
			break;

		case QueryAggregationConstants.LESSTHANEQUAL:
			operatorDoc.append(field, new Document("$" + QueryAggregationConstants.LESSTHANEQUAL, value));
			break;

		case QueryAggregationConstants.GREATERTHAN:
			operatorDoc.append(field, new Document("$" + QueryAggregationConstants.GREATERTHAN, value));
			break;

		case QueryAggregationConstants.LESSTHAN:
			operatorDoc.append(field, new Document("$" + QueryAggregationConstants.LESSTHAN, value));
			break;
			
		case QueryAggregationConstants.CONTAINS:
			Document stringPattern = new Document();
			stringPattern.append("$" + QueryAggregationConstants.REGEX, value);
			stringPattern.append("$options", "i");
			operatorDoc.append(field, stringPattern);
			break;

		default:
			break;
		}
		//supporting and/or conditions in filter
		if (type != null) {
			List<Document> filterList = new ArrayList<Document>();
			Document fileterDoc = new Document();
			if (operatorDoc.containsKey("$"+type)) {
				filterList = (ArrayList<Document>) operatorDoc.get("$"+type);
				operatorDoc.remove("$"+type);
			}
			filterList.add(operatorDoc);
			fileterDoc.append("$"+type, filterList);
			return fileterDoc;
		}
		return operatorDoc;
	}

	private static String getQueryName(String name) {
		if (name.equalsIgnoreCase(PropertyIds.PATH) || name.equalsIgnoreCase(PropertyIds.DESCRIPTION)
				|| name.equalsIgnoreCase(PropertyIds.PARENT_ID)
				|| name.equalsIgnoreCase(PropertyIds.CONTENT_STREAM_LENGTH)
				|| name.equalsIgnoreCase(PropertyIds.CONTENT_STREAM_FILE_NAME)
				|| name.equalsIgnoreCase(PropertyIds.CONTENT_STREAM_MIME_TYPE)
				|| name.equalsIgnoreCase(PropertyIds.CHECKIN_COMMENT)
				|| name.equalsIgnoreCase(PropertyIds.VERSION_LABEL)
				|| name.equalsIgnoreCase(PropertyIds.IS_MAJOR_VERSION)
				|| name.equalsIgnoreCase(PropertyIds.IS_LATEST_VERSION)
				|| name.equalsIgnoreCase(PropertyIds.IS_LATEST_MAJOR_VERSION) || name.equalsIgnoreCase(PropertyIds.NAME)
				|| name.equalsIgnoreCase(PropertyIds.IS_PRIVATE_WORKING_COPY)
				|| name.equalsIgnoreCase(PropertyIds.CREATED_BY) || name.equalsIgnoreCase(PropertyIds.CONTENT_STREAM_ID)
				|| name.equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_ID)
				|| name.equalsIgnoreCase(PropertyIds.VERSION_SERIES_ID)
				|| name.equalsIgnoreCase(PropertyIds.IS_VERSION_SERIES_CHECKED_OUT)
				|| name.equalsIgnoreCase(PropertyIds.IS_IMMUTABLE) || name.equalsIgnoreCase("cmis:modifiedBy")
				|| name.equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_BY)) {
			return getFieldName(name);
		} else if (name.equalsIgnoreCase(PropertyIds.OBJECT_ID)) {
			return "_id";
		} else if (name.equalsIgnoreCase(PropertyIds.SECONDARY_OBJECT_TYPE_IDS)) {
			return "secondaryTypeIds";
		} else if (name.equalsIgnoreCase(PropertyIds.OBJECT_TYPE_ID)) {
			return "typeId";
		} else if (name.equalsIgnoreCase(PropertyIds.LAST_MODIFIED_BY)) {
			return "modifiedBy";
		} else if (name.equalsIgnoreCase(PropertyIds.CREATION_DATE)) {
			return "createdAt";
		} else if (name.equalsIgnoreCase(PropertyIds.CHANGE_TOKEN)) {
			return "token";
		} else if (name.equalsIgnoreCase("cmis:changeType")) {
			return "token.changeType";
		} else if (name.equalsIgnoreCase(PropertyIds.LAST_MODIFICATION_DATE)) {
			return "modifiedAt";
		} else if (name.equalsIgnoreCase(PropertyIds.BASE_TYPE_ID)) {
			return "baseId";
		} else if (name.equalsIgnoreCase("id")) {
			return "id";
		} else if (name.equalsIgnoreCase("operator")) {
			return "operator";
		} else {
			return "properties." + name;
		}
	}

	private static String getFieldName(Object value) {
		String valueString = value.toString();
		String[] values = valueString.split(":");
		String stringValue = values[1].replaceAll("[-+.^:',{}]", "");
		return stringValue;
	}
}
