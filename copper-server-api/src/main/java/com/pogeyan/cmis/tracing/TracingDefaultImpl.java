package com.pogeyan.cmis.tracing;

import java.util.Map;

import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.ITracingFacade;

public class TracingDefaultImpl implements ITracingFacade {
	ISpan span;

	@Override
	public ISpan startSpan(String baseMessageId, String name, Map<String, String> headers) {
		span = new TracingDefaultSpanImpl();
		span.setChildSpan(baseMessageId, name, headers);
		return span;
	}

	@Override
	public void endSpan(String requestMessageId, ISpan spanC) {
		span.close(true);
	}

	@Override
	public void updateSpan(ISpan span, boolean isError, String description, Map<String, Object> map) {
	}
}
