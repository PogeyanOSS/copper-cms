package com.pogeyan.cmis.data.services;

public enum FileNameType {
	BASEOBJECT(0), DOCUMENTOBJECT(1), PROPERTYOBJECT(2), NONE(3);
	private final int value;

	FileNameType(int v) {
		value = v;
	}

	public int value() {
		return value;
	}

	public static FileNameType getFileNameValue(int v) {
		for (FileNameType c : FileNameType.values()) {
			if (c.value == v) {
				return c;
			}
		}
		throw new IllegalArgumentException("Illegal value");
	}
}
