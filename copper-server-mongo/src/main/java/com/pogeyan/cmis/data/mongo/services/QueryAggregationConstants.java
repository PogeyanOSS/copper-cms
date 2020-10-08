package com.pogeyan.cmis.data.mongo.services;

public class QueryAggregationConstants {
	
	public static final String EQUAL = "eq";
	public static final String NOTEQUAL = "ne";
	public static final String GREATERTHAN = "gt";
	public static final String LESSTHAN = "lt";
	public static final String GREATERTHANEQUAL = "gte";
	public static final String LESSTHANEQUAL = "lte";
	public static final String ASCENDING = "asc";
	public static final String DESCENDING = "dsc";
	public static final String SOURCE = "source";
	public static final String TARGET = "target";
	public static final String PROJECT = "$project";
	public static final String MATCH = "$match";
	public static final String GROUP = "$group";
	public static final String SORT = "$sort";
	public static final String LIMIT = "$limit";
	public static final String SKIP = "$skip";
	public static final String LOOKUP = "$lookup";
	public static final String UNWIND = "$unwind";
	public static final String OR = "$or";
	public static final String ROOT = "$$ROOT";
	public static final String FROM = "from";
	public static final String LOCAL_FIELD = "localField";
	public static final String FOREIGN_FIELD = "foreignField";
	public static final String RELATIONSHIP = "Relationship";
	public static final String ALIAS = "as";
	public static final String UNWIND_PATH = "path";
	public static final String SOURCE_ID = "cmis:sourceId";
	public static final String TARGET_ID = "cmis:targetId";
	public static final String COLLECTION_NAME = "objectData";
	public static final String SOURCE_TABLE = "source_table";
	public static final String TARGET_TABLE = "target_table";
	public static final String RELATION_TYPE = "cmis:relationType";
	public static final String RELATION_COLLECTION_NAME= "relationData";
	
}
