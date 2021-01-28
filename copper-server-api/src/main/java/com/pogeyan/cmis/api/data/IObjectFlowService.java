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

	public boolean beforeUpdate(String repositoryId, String objectId, Properties properties, Acl acl,
			IUserObject userObject);

	public boolean beforeDeletion(String repositoryId, String objectId, boolean allVers, IUserObject userObject);

	public void afterCreation(IBaseObject resultData, IUserObject userObject, Map<String, String> extensionObjects);

	public void afterUpdate(IBaseObject resultData, Map<String, Object> updateValues, IUserObject userObject, Map<String, String> extensionObjects);

	public void afterDeletion(IBaseObject resultData, IUserObject userObject, Map<String, String> extensionObjects);
}
