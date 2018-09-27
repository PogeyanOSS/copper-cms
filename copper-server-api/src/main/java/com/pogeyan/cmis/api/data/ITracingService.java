package com.pogeyan.cmis.api.data;

import java.util.Map;

public interface ITracingService {

	/**
	 * 
	 * @param tracingId
	 *            uniqueId for each request
	 * @param name
	 *            unique name for each span
	 * @param headers
	 *            tracing headers
	 * @return ISpan which has instance of a span
	 */
	public ISpan startSpan(String tracingId, ISpan parentSpan, String name, Map<String, String> headers);

	/**
	 * 
	 * @param tracingId
	 *            uniqueId for each request
	 * @param spanC
	 *            span to end
	 */
	public void endSpan(String tracingId, ISpan spanC);

	/**
	 * 
	 * @param span
	 *            span to update
	 * @param isError
	 *            for error cases
	 * @param description
	 *            description of the annotation
	 * @param attrMap
	 *            additional information that is included in the span
	 */
	public void updateSpan(ISpan span, boolean isError, String description, Map<String, Object> attrMap);

}
