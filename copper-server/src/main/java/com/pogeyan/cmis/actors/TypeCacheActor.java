package com.pogeyan.cmis.actors;

import java.util.HashMap;
import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.browser.BrowserConstants;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class TypeCacheActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(TypeCacheActor.class);

	@Override
	public String getName() {
		return "cache";
	}

	public TypeCacheActor() {
		this.registerMessageHandle("resetcache", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.clearCache((QueryGetRequest) t, (HashMap<String, Object>) b))));

	}

	private JSONObject clearCache(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisObjectNotFoundException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"TypeCacheActor::clearCache", null);
		LOG.info("Method name: {}, repositoryId: {}", "clearCache", request.getRepositoryId());
		CacheProviderServiceFactory.getTypeCacheServiceProvider().removeAll(request.getRepositoryId());
		JSONObject j = new JSONObject();
		j.put("status", true);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return j;
	}
}
