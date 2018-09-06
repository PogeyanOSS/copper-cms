package com.pogeyan.cmis.tracing;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.ITracingFacade;

public class TracingDefaultImpl implements ITracingFacade {
	private static final Logger LOG = LoggerFactory.getLogger(TracingDefaultImpl.class);

	@Override
	public ISpan startSpan(ISpan span, String name) {
		LOG.debug("StartTime: {}", System.currentTimeMillis());
		return null;
	}

	@Override
	public void endSpan(ISpan span) {
		LOG.debug("EndTime: {}", System.currentTimeMillis());
	}

	@Override
	public void spanHandleErrors(ISpan span, String errorDescription, String repoId) {
	}

	@Override
	public void registerJaegarService() {
		LOG.warn("Jaeger exporter and Zpages not initialized");
	}

	@Override
	public ISpan extractCMISHeaders(String spanName, Map<String, String> headers) {
		return null;
	}

	@Override
	public void addSpanAnnotations(ISpan span, String description, Map<String, Object> map) {		
	}
}
