package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class ITypeCacheActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "cache";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.TypeCacheActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "resetcache", "resetcachebykey" };
		return selectors;
	}

	@Override
	public boolean isSingletonService() {
		return true;
	}

}
