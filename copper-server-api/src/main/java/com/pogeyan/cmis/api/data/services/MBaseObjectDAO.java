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

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;

public interface MBaseObjectDAO {

	public IBaseObject getLatestToken();

	/**
	 * Remove MBaseObject values depending on object
	 */
	public void delete(String repositoryId, String objectId, boolean forceDelete, TokenImpl token, String typeId);

	/**
	 * Update Folder type objectIds for an object.
	 */
	public void update(String repositoryId, String objectId, Map<String, Object> updateProps);

	public List<? extends IBaseObject> filter(Map<String, Object> fieldNames, boolean includePagination, int maxItems,
			int skipCount, String[] mappedColumns, String typeId);

	public void commit(IBaseObject entity);

	public IBaseObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId);
}