package com.pogeyan.cmis.api.data.common;

public enum PermissionType {

	CREATE("create"), READ("read"), UPDATE("update"), DELETE("delete"), SHARE("share"), VIEW_ONLY("viewonly"), APPROVE(
			"approve");

	private final String value;

	PermissionType(String v) {
		value = v;
	}

	public String value() {
		return value;
	}

	public static PermissionType fromValue(String v) {
		for (PermissionType c : PermissionType.values()) {
			if (c.value.equals(v)) {
				return c;
			}
		}
		throw new IllegalArgumentException(v);
	}
}
