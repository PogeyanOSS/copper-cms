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

import org.bson.types.ObjectId;
import com.pogeyan.cmis.data.objects.MDocumentObject;
import com.pogeyan.cmis.data.objects.MToken;

public interface MDocumentObjectDAO {

	/**
	 * Remove MBaseObject values depending on object
	 */
	public void delete(ObjectId objectId, List<String> removeProps, boolean forceDelete, boolean removefields,
			MToken token);

	/**
	 * update MDocumentObject with multiple field with in single query depending
	 * on ObjectId
	 */
	public void update(ObjectId objectId, Map<String, Object> updateProps);

	/**
	 * get checked out documents.
	 */
	public List<MDocumentObject> getCheckOutDocs(String folderId, String[] principalIds, boolean aclPropagation,
			int maxItems, int skipCount, String orderBy);

	/**
	 * get checked out documents size.
	 */
	public long getCheckOutDocsSize(String folderId, String[] principalIds, boolean aclPropagation);

	public List<MDocumentObject> filter(Map<String, Object> fieldNames, String[] mappedColumns);

	public void commit(MDocumentObject entity);

}