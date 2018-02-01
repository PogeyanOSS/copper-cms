package com.pogeyan.cmis.api.data;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectFlowService {
	public void setObjectFlowStoreSettings(IObjectFlowStoreSetting dbSettings) throws InvalidTargetObjectTypeException;

	public Boolean afterCreation(IBaseObject resultData);

	public Boolean afterUpdate(IBaseObject resultData);

	public Boolean afterDeletion(IBaseObject resultData);
}
