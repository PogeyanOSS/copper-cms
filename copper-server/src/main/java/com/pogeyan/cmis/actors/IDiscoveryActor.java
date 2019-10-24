package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class IDiscoveryActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "discovery";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.DiscoveryActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "contentChanges", "query" };
		return selectors;
	}

}
