package com.pogeyan.cmis.api.utils;

public class TracingWriter {

	public static String log(String message, String traceId) {

		return  traceId + ", " + message;

	}

}
