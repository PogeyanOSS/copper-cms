package com.pogeyan.cmis.api.data.common;

public enum EncryptType {
	ENCRYPT(0), DECRYPT(1);
	private final int value;

	EncryptType(int v) {
		value = v;
	}

	public int value() {
		return value;
	}
}
