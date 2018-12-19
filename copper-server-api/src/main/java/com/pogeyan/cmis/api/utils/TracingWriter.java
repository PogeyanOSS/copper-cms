package com.pogeyan.cmis.api.utils;

import com.pogeyan.cmis.api.data.ISpan;

public class TracingWriter {

	public static String log(String message, ISpan span) {
		if (span != null) {
			return span.getTraceId() + ", " + message;

		}
		return message;
	}
}