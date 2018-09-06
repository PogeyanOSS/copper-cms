package com.pogeyan.cmis.tracing;

import com.pogeyan.cmis.api.data.ITracingFacade;

public class TracingApiServiceFactory {

	static ITracingFacade apiServiceClass;

	public static void add(ITracingFacade apiService) {
		apiServiceClass = apiService;
	}

	public static ITracingFacade getApiService() {
		return apiServiceClass;
	}
}
