package com.pogeyan.cmis.tracing;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.ITracingFacade;

public class TracingDefaultImpl implements ITracingFacade {
	private static final Logger LOG = LoggerFactory.getLogger(TracingDefaultImpl.class);
	ISpan span;

	@Override
	public ISpan startSpan(ISpan span, String name, Map<String, String> headers) {
		span = new TracingDefaultSpanImpl();
		span.setSpan(name);
		return span;
	}

	@Override
	public void endSpan(ISpan span) {
		span.close();
	}

	@Override
	public void registerJaegarService() {
		LOG.warn("Jaeger exporter and Zpages not initialized");
	}

	@Override
	public void updateSpan(ISpan span, boolean isError, String description, Map<String, Object> map) {

	}
}
