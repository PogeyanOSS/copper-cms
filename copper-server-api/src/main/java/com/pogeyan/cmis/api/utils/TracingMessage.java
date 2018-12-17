package com.pogeyan.cmis.api.utils;

import java.util.HashMap;
import java.util.Map;

import com.pogeyan.cmis.api.data.ITracingMessage;

 public class TracingMessage implements ITracingMessage {

	String errorMessage;
	String description;
	String repoId;
	boolean isError;

	private TracingMessage(String errorMessage, String description, String repoId, boolean isError) {
		super();
		this.errorMessage = errorMessage;
		this.description = description;
		this.repoId = repoId;
		this.isError = isError;
	}

	public static TracingMessage message(String errorMessage, String description, String repoId, boolean isError) {
		return new TracingMessage(errorMessage, description, repoId, isError);
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
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
		attrMap.put("repoId", getRepoId());
		attrMap.put("error", getErrorMessage());
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
