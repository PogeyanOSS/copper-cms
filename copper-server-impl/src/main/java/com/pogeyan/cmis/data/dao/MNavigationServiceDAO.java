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
package com.pogeyan.cmis.data.dao;

import java.util.List;

import org.bson.types.ObjectId;
import org.mongodb.morphia.dao.DAO;

import com.pogeyan.cmis.data.objects.MBaseObject;

public interface MNavigationServiceDAO extends DAO<MBaseObject, ObjectId> {

	/**
	 * Returns List of MBaseObject childrens values depending on
	 * ObjectId,maxItems,skipCount.
	 */
	public List<MBaseObject> getChildrenIds(String path, String[] principalIds, boolean aclPropagation, int maxItems,
			int skipCount, String orderBy, String[] mappedColumns);

	/**
	 * Get children size.
	 */
	public long getChildrenSize(String path, String[] principalIds, boolean aclPropagation);

	/**
	 * Returns List of MBaseObject childrens values depending on ObjectId
	 */
	public List<MBaseObject> getDescendants(String path, String[] principalIds, boolean aclPropagation);

	/**
	 * Returns List of MBaseObject FolderTree values depending on ObjectId
	 * 
	 */
	public List<MBaseObject> getFolderTreeIds(String path, String[] principalIds, boolean aclPropagation);
}
