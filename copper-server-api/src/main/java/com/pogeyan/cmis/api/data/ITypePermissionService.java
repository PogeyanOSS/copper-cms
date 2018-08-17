package com.pogeyan.cmis.api.data;

import java.util.List;

import com.pogeyan.cmis.api.auth.IUserGroupObject;
import com.pogeyan.cmis.api.data.common.TypePermissionType;

public interface ITypePermissionService {
	public Boolean checkTypeAccess(String repositoryId, IUserGroupObject[] role, String typeId);

	public List<String> getFieldAccess(String repositoryId, IUserGroupObject[] role, String typeId);

	public Boolean checkPermissionAccess(String repositoryId, IUserGroupObject[] role, String typeId,
			TypePermissionType accessPermission);
}
