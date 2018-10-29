package com.pogeyan.cmis.impl.TypePermission;

import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.common.TypePermissionType;

public class DefaultTypePermissionService implements ITypePermissionService {
	@SuppressWarnings("unused")
	private static final Logger LOG = LoggerFactory.getLogger(DefaultTypePermissionService.class);

	@Override
	public Boolean checkTypeAccess(String repositoryId, IUserObject role, String typeId) {
		if (typeId != null) {
			return true;
		}
		return false;

	}

	@Override
	public List<String> getFieldAccess(String repositoryId, IUserObject role, String typeId) {
		if (typeId != null) {
			List<String> fieldAccess = new ArrayList<String>();
			return fieldAccess;
		}
		return null;
	}

	@Override
	public Boolean checkPermissionAccess(String repositoryId, IUserObject role, String typeId,
			TypePermissionType permissionAccess) {
		if (typeId != null) {
			return true;
		}
		return false;
	}
}