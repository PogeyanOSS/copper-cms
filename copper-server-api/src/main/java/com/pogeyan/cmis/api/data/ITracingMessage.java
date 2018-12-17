package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ITracingMessage {

	public Map<String, Object> getAttrMap();

	public String getDescription();
	
	public boolean isError();
}
