package com.pogeyan.cmis.api.utils;

import java.util.HashMap;
import java.util.Map;

public class TracingErrorMessage extends TracingMessage {
	public static String ERROR = "Error";

	public TracingErrorMessage() {
		super();
	}

	TracingErrorMessage(String message, String description, String repoId, boolean isError) {
		super(message, description, repoId, isError);
	}

	public static TracingMessage message(String message, String description, String repoId, boolean isError) {
		return new TracingErrorMessage(message, description, repoId, isError);
	}

	@Override
	public Map<String, Object> getAttrMap() {
		Map<String, Object> attrMap = new HashMap<String, Object>();
		attrMap.put(REPOID, super.getRepoId());
		attrMap.put(ERROR, super.getMessage());
		return attrMap;
	}

}
