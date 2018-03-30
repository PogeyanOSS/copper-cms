package com.pogeyan.cmis.api.data;

import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectFlowService {
	public void setObjectFlowStoreSettings(IObjectFlowStoreSetting dbSettings) throws InvalidTargetObjectTypeException;

	public Boolean afterCreation(IBaseObject resultData);

	public Boolean afterUpdate(IBaseObject resultData, Map<String, Object> updateValues);

	public Boolean afterDeletion(IBaseObject resultData);
}
