package com.pogeyan.cmis.api.data;

import java.util.List;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.enums.UnfileObject;
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.apache.chemistry.opencmis.commons.spi.Holder;

public interface IObjectFlowService {
	public void setObjectFlowStoreSettings(IObjectFlowStoreSetting dbSettings) throws InvalidTargetObjectTypeException;

	public Boolean beforeCreation(String repositoryId, String folderId, Properties properties, List<String> policies,
			Acl addAces, Acl removeAces, String userName);

	public Boolean beforeUpdate(String repositoryId, Holder<String> objectId, Holder<String> changeToken,
			Properties properties, Acl acl, ObjectInfoHandler objectInfos, String userName);

	public Boolean beforeDeletion(String repositoryId, String folderId, Boolean allVers, UnfileObject unfile,
			Boolean continueOnFail, String userName);

	public Boolean afterCreation(IBaseObject resultData);

	public Boolean afterUpdate(IBaseObject resultData, Map<String, Object> updateValues);

	public Boolean afterDeletion(IBaseObject resultData);
}
