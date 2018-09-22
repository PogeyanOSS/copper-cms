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
	void setChildSpan(String name, Map<String, String> headers);

	/**
	 * 
	 * @param parentSpan
	 *            To the parent of a child span
	 * @param namename
	 *            of the child span
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
	 * 
	 * @param isRootSpan
	 *            If true, then parentSpan will also end , else ends only the
	 *            child span
	 */
	void close(boolean isRootSpan);

	/**
	 * 
	 * @return ISpan which gets the parent span
	 */
	ISpan getParentSpan();
}