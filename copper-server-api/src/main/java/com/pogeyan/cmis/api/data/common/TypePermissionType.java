package com.pogeyan.cmis.api.data.common;

public enum TypePermissionType {

	CREATE("create"), READ("read"), UPDATE("update"), DELETE("delete"), SHARE("share"), VIEW_ONLY("viewonly"), APPROVE(
			"approve");

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
