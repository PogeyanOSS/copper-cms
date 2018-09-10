package com.pogeyan.cmis.tracing;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.ISpan;

public class TracingDefaultSpanImpl implements ISpan {
	private static final Logger LOG = LoggerFactory.getLogger(TracingDefaultSpanImpl.class);

	long startTime;
	String name;

	@Override
	public void setSpan(String name) {
		this.name = name;
		this.startTime = System.currentTimeMillis();
		LOG.debug("Method Name: {}, StartTime: {} ms", name, startTime);
	}

	@Override
	public ISpan setSpanWithParent(String name) {
		return null;
	}

	@Override
	public void close() {
		Long totalTime = System.currentTimeMillis() - startTime;
		LOG.debug("Method Name: {}, TotalTime: {} ms", name, totalTime);
	}

	@Override
	public void updateSpan(boolean isError, String description, Map<String, Object> map) {
	}
}