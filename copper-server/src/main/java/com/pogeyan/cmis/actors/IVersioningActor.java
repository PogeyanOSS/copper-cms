package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class IVersioningActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "versioning";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.VersioningActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "checkOut", "cancelCheckOut", "checkIn", "versions",
				"getObjectOfLatestVersion", "getPropertiesOfLatestVersion" };
		return selectors;
	}

	@Override
	public boolean isSingletonService() {
		return false;
	}

}
