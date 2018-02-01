package com.pogeyan.cmis.api.data.common;

public enum ObjectFlowType {
	CREATED(0), UPDATED(1), DELETED(2);
	private final int value;

	ObjectFlowType(int v) {
		value = v;
	}

	public int value() {
		return value;
	}
}
