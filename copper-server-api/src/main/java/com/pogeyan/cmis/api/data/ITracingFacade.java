package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ITracingFacade {

	public ISpan startSpan(ISpan span,String name);

	public void endSpan(ISpan span);

	public void spanHandleErrors(ISpan span, String errorDescription, String repoId);

	public void registerJaegarService();

	public ISpan extractCMISHeaders(String spanName, Map<String, String> headers);

	public void addSpanAnnotations(ISpan span, String description, Map<String, Object> map);
}
