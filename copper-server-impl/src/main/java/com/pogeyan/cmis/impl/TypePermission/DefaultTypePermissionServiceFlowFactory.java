package com.pogeyan.cmis.impl.TypePermission;

import com.pogeyan.cmis.api.data.ITypePermissionFactory;
import com.pogeyan.cmis.api.data.ITypePermissionService;

public class DefaultTypePermissionServiceFlowFactory implements ITypePermissionFactory {

	@Override
	public ITypePermissionService getTypePermissionFlowService(String repositoryId) {
		DefaultTypePermissionService typeService = new DefaultTypePermissionService();
		return typeService;
	}

}
