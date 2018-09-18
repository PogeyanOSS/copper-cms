package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ISpan {

	void setSpan(String baseMessageId, String name, Map<String, String> headers);

	void updateSpan(boolean isError, String description, Map<String, Object> map);

	void close();
}