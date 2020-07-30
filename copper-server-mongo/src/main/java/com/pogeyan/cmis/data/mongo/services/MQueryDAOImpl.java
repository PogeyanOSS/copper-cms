package com.pogeyan.cmis.data.mongo.services;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.bson.Document;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.dao.BasicDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoDatabase;
import com.pogeyan.cmis.api.data.services.MQueryDAO;
import com.pogeyan.cmis.data.mongo.MBaseObject;
import com.pogeyan.cmis.data.query.FilterQueryRequest;
import com.pogeyan.cmis.data.query.IQueryResponse;
import com.pogeyan.cmis.data.query.QueryRequest;
import com.pogeyan.cmis.data.query.SortQueryRequest;

public class MQueryDAOImpl extends BasicDAO<MBaseObject, ObjectId> implements MQueryDAO {

	private static final Logger LOG = LoggerFactory.getLogger(MQueryDAOImpl.class);

	public MQueryDAOImpl(Class<MBaseObject> class1, Datastore ds) {
		super(class1, ds);
	}

	@SuppressWarnings("deprecation")
	@Override
	public List<IQueryResponse> query(QueryRequest request, String[] principalIds) {

		List<Document> document = new ArrayList<Document>();
		List<FilterQueryRequest> filterRequest = request.getFilter();
		List<SortQueryRequest> sortRequest = request.getSort();
		Document projections = new Document();
		Document projectionDocument = new Document();
		String sourceTypeId = null;
		String targetTypeId = null;
		Map<String, QueryRequest> fieldsRequestMap = request.getFields();
		if (!fieldsRequestMap.isEmpty()) {
			for (Entry<String, QueryRequest> fieldsRequest : fieldsRequestMap.entrySet()) {
				if (fieldsRequest.getKey().contains("_")) {
					String key = fieldsRequest.getKey();
					String[] keySplit = key.split("_");
					String direction = fieldsRequest.getValue().getDirection();
					sourceTypeId = keySplit[0];
					targetTypeId = keySplit[1];
					projectionDocument = getDefaultProjectDocument(fieldsRequest, projectionDocument, sourceTypeId,
							targetTypeId, direction);
					document = getLookupQuery(fieldsRequest, document, principalIds, sourceTypeId, targetTypeId, key,
							projectionDocument);
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

		document = getQueryAggsPipeline(request, filterRequest, sortRequest, document, true, principalIds);
		if (projectionDocument != null && !projectionDocument.isEmpty()) {
			projections.append("$project", projectionDocument);
			document.add(projections);
		}
		List<IQueryResponse> result = new ArrayList<IQueryResponse>();
		String dBName = this.ds.getDB().getName();
		MongoDatabase db = this.ds.getMongo().getDatabase(dBName);
		MongoCursor<Document> iterator = db.getCollection("objectData").aggregate(document).iterator();
		LOG.error("Get Dynamic Relationship Query Result of iterator has next : {} ", iterator.hasNext());
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

		LOG.error("Get Response Dynamic for RelationShip Query Result : {} ", list);
		return result;
	}

	private Document getDefaultProjectDocument(Entry<String, QueryRequest> fieldsQuery, Document projectionDocument,
			String sourceTypeId, String targetTypeId, String direction) {
		String label = sourceTypeId;
		String objectId = "$" + label + "." + getQueryName("cmis:objectId");
		String objectTypeId = "$" + label + "." + getQueryName("cmis:objectTypeId");
		String objectName = "$" + label + "." + getQueryName("cmis:name");
		if (direction != null && direction.equals("target")) {
			label = targetTypeId;
			objectId = "$" + label + "." + getQueryName("cmis:objectId");
			objectTypeId = "$" + label + "." + getQueryName("cmis:objectTypeId");
			objectName = "$" + label + "." + getQueryName("cmis:name");
		}
		projectionDocument.append("cmis:objectId", objectId);
		projectionDocument.append("cmis:objectTypeId", objectTypeId);
		projectionDocument.append("cmis:name", objectName);
		return projectionDocument;
	}

	private List<Document> getQueryAggsPipeline(QueryRequest request, List<FilterQueryRequest> filterRequest,
			List<SortQueryRequest> sortRequest, List<Document> document, boolean aclPropagation,
			String[] principalIds) {
		int limit = request.getSize();
		Document filterQuery = new Document();
		Document sortQuery = new Document();
		Document limitQuery = new Document();

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
			limitQuery.append("$limit", limit);
			document.add(limitQuery);
		}
		return document;
	}

	private List<Document> getLookupQuery(Entry<String, QueryRequest> fieldsQuery, List<Document> document,
			String[] principalIds, String sourceTypeId, String targetTypeId, String key, Document projectionDocument) {
		Document rootProjection = new Document();
		Document projectionDoc = new Document(); 
		if (fieldsQuery.getValue() != null && (fieldsQuery.getValue().getDirection() != null
				&& fieldsQuery.getValue().getDirection().equals("target"))) {
			getLookupByDirection(document, rootProjection, targetTypeId, sourceTypeId, "target");
			projectionDoc = getProjectDocumentOnLookup(targetTypeId, sourceTypeId, "source" , fieldsQuery);
			projectionDocument.append(sourceTypeId, projectionDoc);
		} else {
			getLookupByDirection(document, rootProjection, sourceTypeId, targetTypeId, "source");
			projectionDoc = getProjectDocumentOnLookup(targetTypeId, sourceTypeId, "target" , fieldsQuery);
			projectionDocument.append(targetTypeId, projectionDoc);
		}

		if (fieldsQuery.getValue() != null && (fieldsQuery.getValue().getFields() != null
				|| fieldsQuery.getValue().getFilter() != null || fieldsQuery.getValue().getSort() != null))
		{
			QueryRequest aggsRequest = fieldsQuery.getValue();
			List<FilterQueryRequest> filter = aggsRequest.getFilter();
			List<SortQueryRequest> sort = aggsRequest.getSort();
			document = getQueryAggsPipeline(aggsRequest, filter, sort, document, false, principalIds);
		}
		return document;
	}

	private Document getProjectDocumentOnLookup(String targetTypeId, String sourceTypeId,
			String direction, Entry<String, QueryRequest> fieldsQuery) {
		Document projection = new Document();
		if (fieldsQuery.getValue().getFields() != null) {
			String key = fieldsQuery.getKey();
			if (key.contains(".")) {
				String[] keys = key.split("\\.");
				String label = keys[0];
				String field = keys[1];
				String fieldKey = field;
				String fieldName = "$" + label + "." + getQueryName(field);
				projection.append(fieldKey, fieldName);
			} else {
				String fieldName = "$" + getQueryName(key);
				projection.append(key, fieldName);
			}
		}
		projection = getDefaultProjectDocument(fieldsQuery, projection, sourceTypeId, targetTypeId,
				direction);
		
		return projection;
	}

	private void getLookupByDirection(List<Document> document, Document rootProjection, String sourceTypeId,
			String targetTypeId, String direction) {
		Document filterDoc = new Document();
		Document operatorDoc = new Document();
		rootProjection = new Document("$project", rootProjection.append(sourceTypeId, "$$ROOT"));
		document.add(rootProjection);
		operatorDoc = new Document(sourceTypeId + "." + "token.changeType",
				new Document("$" + QueryAggregationConstants.NOTEQUAL, 2));
		filterDoc = new Document("$match", operatorDoc);
		document.add(filterDoc);
		String relationshipAlias = sourceTypeId + "Relationship";
		if (direction.equals("target")) {
			getLookupDocument("objectData", sourceTypeId + "._id", getQueryName("cmis:targetId"), relationshipAlias,
					document);
			getLookupDocument("objectData", relationshipAlias + "." + getQueryName("cmis:sourceId"), "_id",
					targetTypeId, document);
		} else {
			getLookupDocument("objectData", sourceTypeId + "._id", getQueryName("cmis:sourceId"), relationshipAlias,
					document);
			getLookupDocument("objectData", relationshipAlias + "." + getQueryName("cmis:targetId"), "_id",
					targetTypeId, document);
		}
	}

	private void getLookupDocument(String collectionName, String localField, String foreignField, String alias,
			List<Document> document) {
		Document lookupDocument = new Document();
		Document lookupObject = new Document();
		Document unwindObject = new Document();
		Document filterDoc = new Document();
		Document operatorDoc = new Document();
		lookupObject.append("from", collectionName);
		lookupObject.append("localField", localField);
		lookupObject.append("foreignField", foreignField);
		lookupObject.append("as", alias);
		unwindObject.append("$unwind", new Document("path", "$" + alias));
		lookupDocument.append("$lookup", lookupObject);
		document.add(lookupDocument);
		document.add(unwindObject);
		operatorDoc.append(alias + "." + "token.changeType", new Document("$" + QueryAggregationConstants.NOTEQUAL, 2));
		filterDoc.append("$match", operatorDoc);
		document.add(filterDoc);
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
		sortAggs.append("$sort", sortDoc);
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
			if (operatorDoc.containsKey("$or")) {
				acl = (ArrayList<Document>) operatorDoc.get("$or");
				operatorDoc.remove("$or");
				aclList.addAll(acl);
			}
			operatorDoc.append("$or", aclList);
		}
		filterDoc.append("$match", operatorDoc);
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

		default:
			break;
		}

		if (type != null && type.equals("or")) {
			List<Document> filterList = new ArrayList<Document>();
			Document fileterDoc = new Document();
			filterList.add(operatorDoc);
			fileterDoc.append("$or", filterList);
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
