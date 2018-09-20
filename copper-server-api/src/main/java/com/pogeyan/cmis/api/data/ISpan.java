package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ISpan {

	void setChildSpan(String baseMessageId, String name, Map<String, String> headers);

	ISpan setSpanwithParent(ISpan parentSpan, String baseMessageId, String name);

	void updateSpan(boolean isError, String description, Map<String, Object> map);

	void close(boolean forRootSpan);

	ISpan getParentSpan();
}