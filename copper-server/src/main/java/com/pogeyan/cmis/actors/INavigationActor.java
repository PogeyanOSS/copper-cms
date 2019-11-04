package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class INavigationActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "navigation";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.NavigationActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "children", "descendants", "folderTree", "parent", "parents", "folder",
				"checkedout", "getAllObjects" };
		return selectors;
	}

	@Override
	public boolean isServiceActor() {
		return false;
	}

}
