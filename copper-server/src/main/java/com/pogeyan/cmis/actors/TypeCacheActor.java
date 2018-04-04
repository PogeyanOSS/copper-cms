package com.pogeyan.cmis.actors;

import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;

public class TypeCacheActor extends BaseClusterActor<BaseRequest, BaseResponse> {

	@Override
	public String getName() {
		return "cache";
	}

	public TypeCacheActor() {
		this.registerMessageHandle("resetcache", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.clearCache((QueryGetRequest) t))));

	}

	private JSONObject clearCache(QueryGetRequest request) throws CmisObjectNotFoundException, CmisRuntimeException {
		CacheProviderServiceFactory.getTypeCacheServiceProvider().removeAll(request.getRepositoryId());
		return null;
	}
}
