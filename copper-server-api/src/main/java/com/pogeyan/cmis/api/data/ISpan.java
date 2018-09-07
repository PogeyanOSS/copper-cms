package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ISpan {

	void setSpan(String name);

	void updateSpan(boolean isError, String description, Map<String, Object> map);

	ISpan setSpanWithParent(String name);

	void close();
}