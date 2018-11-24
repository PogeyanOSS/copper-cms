package com.pogeyan.cmis.api.data.services;

import java.util.List;

import com.pogeyan.cmis.api.data.IDocumentObject;

public interface MNavigationDocServiceDAO {
	/**
	 * Returns List of MBaseObject childrens values depending on
	 * ObjectId,maxItems,skipCount.
	 */
	public List<? extends IDocumentObject> getChildren(String path, String[] principalIds, boolean aclPropagation,
			int maxItems, int skipCount, String orderBy, String[] mappedColumns, String filterExpression,
			MTypeManagerDAO typeManager, String repositoryId, String typeId);

	/**
	 * Get children size.
	 */
	public long getChildrenSize(String path, String[] principalIds, boolean aclPropagation, String repositoryId,
			String typeId, String filterExpression, MTypeManagerDAO typeManager);

	/**
	 * Returns List of MBaseObject childrens values depending on ObjectId
	 */
	public List<? extends IDocumentObject> getDescendants(String path, String[] principalIds, boolean aclPropagation,
			String[] mappedColumns, String filterExpression, MTypeManagerDAO typeManager);

	/**
	 * Returns List of MBaseObject FolderTree values depending on ObjectId
	 * 
	 */
	public List<? extends IDocumentObject> getFolderTreeIds(String path, String[] principalIds, boolean aclPropagation);
}
