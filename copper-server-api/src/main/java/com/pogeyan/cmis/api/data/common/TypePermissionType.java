package com.pogeyan.cmis.api.data.common;

public enum TypePermissionType {

	READ("read"), WRITE("write"), DELETE("delete");

	private final String value;

	TypePermissionType(String v) {
		value = v;
	}

	public String value() {
		return value;
	}

	public static TypePermissionType fromValue(String v) {
		for (TypePermissionType c : TypePermissionType.values()) {
			if (c.value.equals(v)) {
				return c;
			}
		}
		throw new IllegalArgumentException(v);
	}
}
