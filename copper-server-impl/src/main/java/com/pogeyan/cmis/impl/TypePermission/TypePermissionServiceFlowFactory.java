package com.pogeyan.cmis.impl.TypePermission;

import com.pogeyan.cmis.api.data.ITypePermissionFactory;
import com.pogeyan.cmis.api.data.ITypePermissionService;

public class TypePermissionServiceFlowFactory implements ITypePermissionFactory {

	@Override
	public ITypePermissionService getTypePermissionFlowService(String repositoryId) {
		TypePermissionService typeService = new TypePermissionService();
		return typeService;
	}

}
