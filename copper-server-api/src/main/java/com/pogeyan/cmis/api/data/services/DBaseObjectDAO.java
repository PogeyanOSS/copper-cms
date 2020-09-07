package com.pogeyan.cmis.api.data.services;

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;

public interface DBaseObjectDAO {
	public IBaseObject getLatestToken();

	/**
	 * Remove MBaseObject values depending on object
	 */
	public void delete(String repositoryId, String[] principalIds, String objectId, boolean forceDelete,
			boolean aclPropagation, TokenImpl token, String typeId);

	/**
	 * Update Folder type objectIds for an object.
	 * 
	 * @return
	 */
	public void update(String repositoryId, String objectId, Map<String, Object> updateProps, String typeId);

	public List<? extends IBaseObject> filter(Map<String, Object> fieldNames, String[] principalIds,
			boolean aclPropagation, boolean includePagination, int maxItems, int skipCount, String[] mappedColumns,
			String typeId);

	public void commit(IBaseObject entity, String typeId);

	public IBaseObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId);

	public List<? extends IBaseObject> getObjects(List<String> objectIds, String[] principalIds, boolean aclPropagation,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns, String typeId);

}