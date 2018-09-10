package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ITracingFacade {

	public ISpan startSpan(ISpan span, String name, Map<String, String> headers);

	public void endSpan(ISpan span);

	public void updateSpan(ISpan span, boolean isError, String description, Map<String, Object> map);
}
