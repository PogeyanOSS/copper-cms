package com.pogeyan.cmis.api.data;

import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectFlowFactory {

	public IObjectFlowStoreSetting getStoreSetting();

	public IObjectFlowService getObjectFlowService(Map<String, String> parameters)
			throws InvalidTargetObjectTypeException;
}
