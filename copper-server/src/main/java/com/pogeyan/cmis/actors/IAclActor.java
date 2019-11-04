package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class IAclActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "acl";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.AclActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "acl", "applyACL" };
		return selectors;
	}

	@Override
	public boolean isSingletonService () {
		return false;
	}

}
