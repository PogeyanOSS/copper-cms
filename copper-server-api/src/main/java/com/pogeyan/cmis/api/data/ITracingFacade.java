package com.pogeyan.cmis.api.data;

import java.util.HashMap;
import java.util.Map;

public interface ITracingFacade {
	public static Map<String, Object> traceContextMap = new HashMap<>();

//	public ISpan startSpan(ISpan span, String name, Map<String, String> headers);

	public void endSpan(ISpan span);

	public void updateSpan(ISpan span, boolean isError, String description, Map<String, Object> map);
	
	public ISpan startSpan(String baseMessageId,String name, Map<String, String> headers);
}
