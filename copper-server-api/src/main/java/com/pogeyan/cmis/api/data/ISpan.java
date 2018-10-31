package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ISpan {
	/**
	 * 
	 * @param name
	 *            name of the span
	 * @param headers
	 *            list of tracing headers from which parent context will be
	 *            formed
	 */
	void setChildSpan(String tracingId, String name, Map<String, String> headers);

	/**
	 * 
	 * @param parentSpan
	 *            The parent of a child span
	 * @param name
	 *            name of the child span
	 * @return ISpan which has parent and child span
	 */
	ISpan setSpanwithParent(ISpan parentSpan, String name);

	/**
	 * 
	 * @param isError
	 *            for error cases
	 * @param description
	 *            description of the annotation
	 * @param attrMap
	 *            additional information that is included in the span
	 */
	void updateSpan(boolean isError, String description, Map<String, Object> attrMap);

	/**
	 * ends the span
	 */
	void close();

	/**
	 * 
	 * @return returns the parentSpan
	 */
	ISpan getParentSpan();

	String getTraceId();
}