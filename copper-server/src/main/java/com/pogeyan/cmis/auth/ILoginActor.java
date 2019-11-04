package com.pogeyan.cmis.auth;

import com.pogeyan.cmis.api.IActorService;

public class ILoginActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "login";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.auth.LoginActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "authenticate" };
		return selectors;
	}

	@Override
	public boolean isSingletonService() {
		return false;
	}

}
