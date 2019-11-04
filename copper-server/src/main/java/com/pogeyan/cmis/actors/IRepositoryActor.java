package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class IRepositoryActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "repository";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.RepositoryActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "getrepositories", "repositoryInfo", "typeDefinition", "typeChildren",
				"typeDescendants", "createType", "deleteType", "updateType" };
		return selectors;
	}

	@Override
	public boolean isSingletonService() {
		return false;
	}

}
