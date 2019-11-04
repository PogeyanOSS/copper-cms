package com.pogeyan.cmis.actors;

import com.pogeyan.cmis.api.IActorService;

public class IObjectActor implements IActorService {

	@Override
	public String getServiceURL() {
		return "object";
	}

	@Override
	public Class<?> getActorClass() throws ClassNotFoundException {
		return Class.forName("com.pogeyan.cmis.actors.ObjectActor");
	}

	@Override
	public String[] getMethodSelectors() {
		String[] selectors = new String[] { "object", "properties", "allowableActions", "renditions", "createFolder",
				"createDocument", "createDocumentFromSource", "createItem", "createPolicy", "createRelationship",
				"bulkUpdate", "update", "content", "setContent", "appendContent", "deleteContent", "delete",
				"deleteTree", "move" };
		return selectors;
	}

	@Override
	public boolean isServiceActor() {
		return false;
	}

}
