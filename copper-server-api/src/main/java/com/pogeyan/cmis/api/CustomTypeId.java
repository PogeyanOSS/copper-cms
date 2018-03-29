package com.pogeyan.cmis.api;

public enum CustomTypeId {

	CMIS_EXT_RELATIONMD("cmis_ext:relationmd"), //

	CMIS_EXT_RELATIONSHIP("cmis_ext:relationship"), //

	CMIS_EXT_CONFIG("cmis_ext:config"); //

	private final String value;

	CustomTypeId(String v) {
		value = v;
	}

	public String value() {
		return value;
	}

	public static CustomTypeId fromValue(String v) {
		for (CustomTypeId c : CustomTypeId.values()) {
			if (c.value.equals(v)) {
				return c;
			}
		}
		throw new IllegalArgumentException(v);
	}
}
