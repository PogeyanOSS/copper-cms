package com.pogeyan.cmis.api.data;

import java.util.List;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.Properties;

import com.pogeyan.cmis.api.auth.IUserObject;

public interface IObjectFlowService {
	public void setObjectFlowStoreSettings(IObjectFlowStoreSetting dbSettings) throws InvalidTargetObjectTypeException;

	public boolean beforeCreation(String repositoryId, String objectId, Properties properties, List<String> policies,
			Acl addAces, Acl removeAces, IUserObject userObject);

	public boolean beforeUpdate(String repositoryId, String objectId, Properties properties, Acl acl, String userName);

	public boolean beforeDeletion(String repositoryId, String objectId, boolean allVers, String userName);

	public void afterCreation(IBaseObject resultData);

	public void afterUpdate(IBaseObject resultData, Map<String, Object> updateValues);

	public void afterDeletion(IBaseObject resultData);
}
