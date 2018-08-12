package com.pogeyan.cmis.api.data;

import java.util.List;

public interface ITypePermissionService {
	public Boolean checkTypeAccess(String repositoryId, String typeId);

	public List<String> getFieldAccess(String repositoryId, String typeId);

	public List<String> getTypeIdAccess(String repositoryId);

	public Boolean checkPermissionAccess(String repositoryId, String typeId, String acess);
}
