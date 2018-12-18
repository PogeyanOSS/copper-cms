package com.pogeyan.cmis.tracing;

import java.util.Map;

import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.ITracingMessage;
import com.pogeyan.cmis.api.data.ITracingService;

public class TracingDefaultImpl implements ITracingService {
	ISpan span;

	@Override
	public ISpan startSpan(String tracingId, ISpan parentSpan, String name, Map<String, String> headers) {
		span = new TracingDefaultSpanImpl();
		span.setChildSpan(tracingId, name, headers);
		return span;
	}

	@Override
	public void endSpan(String tracingId, ISpan spanC, boolean isError) {
		spanC.close(isError);
	}

	@Override
	public void updateSpan(ISpan span, ITracingMessage tracingMessage) {
	}
}
