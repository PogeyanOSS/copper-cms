package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ISpan {
	void setSpan(String name);
	ISpan setSpanWithParent(String name);
	void close();
	void setScope();
	void handleSpanErrors(String errorDescription, String repoId);
	void handleSpanAnnotations(String description, Map<String,Object> map);
}