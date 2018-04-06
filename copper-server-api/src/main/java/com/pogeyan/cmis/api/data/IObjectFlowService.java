package com.pogeyan.cmis.api.data;

import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;

public interface IObjectFlowService {
	public void setObjectFlowStoreSettings(IObjectFlowStoreSetting dbSettings) throws InvalidTargetObjectTypeException;

	public Boolean beforeCreation(AccessControlListImplExt acl);

	public Boolean beforeUpdate();

	public Boolean beforeDeletion();

	public Boolean afterCreation(IBaseObject resultData);

	public Boolean afterUpdate(IBaseObject resultData, Map<String, Object> updateValues);

	public Boolean afterDeletion(IBaseObject resultData);
}
