package com.pogeyan.cmis.api.data;

import com.pogeyan.cmis.api.auth.IUserGroupObject;

public interface ITypePermissionfactory {

	public ITypePermissionService getTypePermissionFlowService(String repositoryId, IUserGroupObject[] role);
}
