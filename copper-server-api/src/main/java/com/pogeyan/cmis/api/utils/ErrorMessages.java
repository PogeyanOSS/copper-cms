package com.pogeyan.cmis.api.utils;

public class ErrorMessages {

	public static String INVALID_EXCEPTION = "CmisInvalidArgumentException";
	public static String RUNTIME_EXCEPTION = "CmisRuntimeException";
	public static String UNKNOWN_OBJECT = "Unknown object id: %s, TraceId: %s";
	public static String NOT_AUTHORISED = "%s is not authorized to applyAcl, TraceId: %s";
	public static String TOKEN_VALUE_NULL = "change log token value should not be null, TraceId: %s";
	public static String ACL_NULL = "object acl is null, TraceId: %s";
	public static String RELATIONSHIP_NULL = "Relationships are null, TraceId: %s";
	public static String EXCEPTION = "exception: %s, TraceId: %s";
	public static String OBJECT_NULL = "Object is null, TraceId: %s";
	public static String POLICY_NOT_APPLIED = "Policy id: %s, cannot be removed because it is not applied to object: %s, TraceId: %s";
	public static String UNKNOWN_POLICY_ID = "Unknown policy id: %s, TraceId: %s";
	public static String POLICY_NOT_ADDED = "Policy id: %s, cannot be added because it is already applied to object: %s, TraceId: %s";
	public static String TYPE_TREE_NULL = "Type tree is null, TraceId: %s";
	public static String TYPE_DEFINITION_MISSING = "%s, Type definition missing, TraceId: %s";
	public static String JSON_ERROR = "%s, JSON Parser error, TraceId: %s";
	public static String INVALID_TYPE_DEFINITION = "%s, Invalid type definition, TraceId: %s";
	public static String MONGO_OBJECT_NULL = "MongoObject shouldnot be null, TraceId: %s";
	public static String TYPE_MUST_BE_SET = "Type must be set!, TraceId: %s";
	public static String NOT_VALID_ID = "Type must have a valid id, TraceId: %s";
	public static String PARENT_NOT_VALID = "Type must have a valid parent id, TraceId: %s";
	public static String TYPE_ID_PRESENT = "%s, id already present, TraceId: %s";
	public static String CREATE_PERMISSION_DENIED = "Create type permission denied for this userId: %s, TraceId: %s";
	public static String UNKNOWN_TYPE_ID = "Unknown TypeId: %s, TraceId: %s";
	public static String UPDATE_PERMISSION_DENIED = "update type permission denied for this userId: %s, TraceId: %s";
	public static String DELETE_PERMISSION_DENIED = "delete type permission denied for this userId: %s, TraceId: %s";
	public static String ZERO_DEPTH = "A zero depth is not allowed for getDescendants, TraceId: %s";
	public static String CANNOT_GET_PARENT = "Cannot get parent of a root folder, TraceId: %s";

}
