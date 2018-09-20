package com.pogeyan.cmis.api.data;

import java.util.HashMap;
import java.util.Map;

public interface ITracingFacade {

	public static Map<String, ISpan> traceContextMap = new HashMap<>();

	public void endSpan(String requestMessageId, ISpan spanC);

	public void updateSpan(ISpan span, boolean isError, String description, Map<String, Object> map);

	public ISpan startSpan(String baseMessageId, String name, Map<String, String> headers);
}
