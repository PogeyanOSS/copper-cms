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

import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.IBaseObject;

public interface MDocumentObjectDAO {

	/**
	 * Remove MBaseObject values depending on object
	 */
	public void delete(String repositoryId, String objectId, String typeId, List<String> removeProps,
			boolean forceDelete, boolean removefields, TokenImpl token);

	/**
	 * update MDocumentObject with multiple field with in single query depending on
	 * ObjectId
	 */
	public void update(String repositoryId, String objectId, String typeId, Map<String, Object> updateProps);

	/**
	 * get checked out documents.
	 */
	public List<? extends IDocumentObject> getCheckOutDocs(String repositoryId, String folderId, String typeId,
			String[] principalIds, boolean aclPropagation, int maxItems, int skipCount, String orderBy);

	/**
	 * get checked out documents size.
	 */
	public long getCheckOutDocsSize(String repositoryId, String folderId, String typeId, String[] principalIds,
			boolean aclPropagation);

	public List<? extends IDocumentObject> filter(String repositoryId, Map<String, Object> fieldNames, String typeId,
			String[] mappedColumns);

	public void commit(String repositoryId, IDocumentObject entity);

	public IDocumentObject createObjectFacade(String objectId, IBaseObject baseObject, Boolean isImmutable,
			Boolean isLatestVersion, Boolean isMajorVersion, Boolean isLatestMajorVersion, Boolean isPrivateWorkingCopy,
			String versionLabel, String versionSeriesId, String versionReferenceId, Boolean isVersionSeriesCheckedOut,
			String versionSeriesCheckedOutBy, String versionSeriesCheckedOutId, String checkinComment,
			Long contentStreamLength, String contentStreamMimeType, String contentStreamFileName,
			String contentStreamId, String previousVersionObjectId);
}