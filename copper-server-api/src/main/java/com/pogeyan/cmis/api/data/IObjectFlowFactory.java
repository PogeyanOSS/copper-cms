package com.pogeyan.cmis.api.data;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectFlowFactory {

	public IObjectFlowStoreSetting getStoreSetting();

	public IObjectFlowService getObjectFlowService(String repositoryId) throws InvalidTargetObjectTypeException;
}
