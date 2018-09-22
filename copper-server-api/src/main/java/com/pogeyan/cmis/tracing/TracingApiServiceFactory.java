package com.pogeyan.cmis.tracing;

import com.pogeyan.cmis.api.data.ITracingService;

public class TracingApiServiceFactory {

	static ITracingService apiServiceClass;

	public static void add(ITracingService apiService) {
		apiServiceClass = apiService;
	}

	public static ITracingService getApiService() {
		return apiServiceClass;
	}
}
