package com.pogeyan.cmis.api.utils;

import java.util.HashMap;
import java.util.Map;

import com.pogeyan.cmis.api.data.ITracingMessage;

public class TracingMessage implements ITracingMessage {

	public static String REPOID = "repoId";
	public static String SUCCESS = "Success";

	String message;
	String description;
	String repoId;
	boolean isError;

	TracingMessage() {

	}

	/**
	 * 
	 * @param repoId
	 *            the repositoryId
	 * @param isError
	 *            for error cases
	 * @param description
	 *            description of the annotation
	 * @param attrMap
	 *            additional information that is included in the span
	 */

	TracingMessage(String message, String description, String repoId, boolean isError) {
		super();
		this.message = message;
		this.description = description;
		this.repoId = repoId;
		this.isError = isError;
	}

	public static TracingMessage message(String message, String description, String repoId, boolean isError) {
		return new TracingMessage(message, description, repoId, isError);
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getRepoId() {
		return repoId;
	}

	public void setRepoId(String repoId) {
		this.repoId = repoId;
	}

	public void setIsError(boolean isError) {
		this.isError = isError;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public Map<String, Object> getAttrMap() {
		Map<String, Object> attrMap = new HashMap<String, Object>();
		attrMap.put(REPOID, getRepoId());
		attrMap.put(SUCCESS, getMessage());
		return attrMap;
	}

	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public boolean isError() {
		return isError;
	}

}