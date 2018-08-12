package com.pogeyan.cmis.impl.TypePermission;

import com.pogeyan.cmis.api.auth.IUserGroupObject;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.ITypePermissionfactory;

public class TypePermissionServiceFlowFactory implements ITypePermissionfactory {

	@Override
	public ITypePermissionService getTypePermissionFlowService(String repositoryId, IUserGroupObject[] role) {
		TypePermissionService typeService = new TypePermissionService();
		typeService.setDetails(repositoryId, role);
		return typeService;
	}

}
