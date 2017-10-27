package com.pogeyan.cmis.api.data;

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenImpl;

public interface IBaseObject {
	public String getId();

	public String getName();

	public BaseTypeId getBaseId();

	public String getTypeId();

	public String getRepositoryId();

	public List<String> getSecondaryTypeIds();

	public String getDescription();

	public String getCreatedBy();

	public String getModifiedBy();

	public Long getCreatedAt();

	public Long getModifiedAt();

	public TokenImpl getChangeToken();

	public String getInternalPath();

	public String getPath();

	public List<String> getPolicies();

	public AccessControlListImplExt getAcl();

	public String getParentId();

	public Map<String, Object> getProperties();
	
	public void setProperties(Map<String, Object> props);
}
