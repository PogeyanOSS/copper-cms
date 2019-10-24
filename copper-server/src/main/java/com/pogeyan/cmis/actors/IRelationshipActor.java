package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class IRelationshipActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "relationships";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.RelationshipActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "relationships" };
		return selectors;
	}

}
