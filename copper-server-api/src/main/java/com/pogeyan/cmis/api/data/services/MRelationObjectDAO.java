/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.api.data.services;

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

import com.pogeyan.cmis.api.data.IRelationObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;

public interface MRelationObjectDAO {

	public IRelationObject getLatestToken();

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

	public List<? extends IRelationObject> filter(Map<String, Object> fieldNames, String[] principalIds,
			boolean aclPropagation, boolean includePagination, int maxItems, int skipCount, String[] mappedColumns,
			String typeId);

	public void commit(IRelationObject entity, String typeId);

	public IRelationObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String description,
			String createdBy, String modifiedBy, TokenImpl token, Map<String, Object> properties, Acl acl,
			String parentId);

	public List<? extends IRelationObject> getObjects(List<String> objectIds, String[] principalIds, boolean aclPropagation,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns, String typeId);

}