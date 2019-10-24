package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class IPolicyActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "policy";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.PolicyActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "policies", "applyPolicy", "removePolicy" };
		return selectors;
	}

}
