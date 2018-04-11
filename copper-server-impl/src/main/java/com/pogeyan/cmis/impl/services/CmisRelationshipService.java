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
package com.pogeyan.cmis.impl.services;

import java.math.BigInteger;
import java.util.List;
import java.util.Set;

import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
import org.apache.chemistry.opencmis.commons.enums.RelationshipDirection;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectListImpl;
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.impl.utils.DBUtils;
import com.pogeyan.cmis.api.data.IBaseObject;

public class CmisRelationshipService {
	private static final Logger LOG = LoggerFactory.getLogger(CmisNavigationService.class);

	public static class Impl {
		/**
		 * CMIS getObjectRelationships.
		 */
		public static ObjectList getObjectRelationships(String repositoryId, String objectId,
				Boolean includeSubRelationshipTypes, RelationshipDirection relationshipDirection, String typeId,
				String filter, Boolean includeAllowableActions, BigInteger maxItems, BigInteger skipCount,
				ObjectInfoHandler objectInfos, String userName) throws CmisObjectNotFoundException, MongoException {
			LOG.info("getObjectRelationships on object: {} , repository: {},", repositoryId, objectId);
			IBaseObject so = null;
			try {
				so = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null);
			} catch (Exception e) {
				LOG.error("getObjectRelationships Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
				throw new MongoException(e.toString());
			}

			if (so == null) {
				LOG.error("getObjectRelationships Exception: {},{}", "Unknown object id", objectId);
				throw new CmisObjectNotFoundException("Unknown object id: " + objectId);
			}
			int maxItemsInt = maxItems == null ? -1 : maxItems.intValue();
			int skipCountInt = skipCount == null ? 0 : skipCount.intValue();

			String[] filterArray = null;
			// split filter
			Set<String> filterCollection = CmisObjectService.Impl.splitFilter(filter);
			if (filter != null && filterCollection != null && filterCollection.size() > 0) {
				filterArray = Helpers.getFilterArray(filterCollection, so.getBaseId() != BaseTypeId.CMIS_DOCUMENT);
			}
			ObjectListImpl result = new ObjectListImpl();
			List<ObjectData> odList = null;
			List<? extends IBaseObject> totalSize = null;
			if (relationshipDirection == RelationshipDirection.SOURCE) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipBySourceId(repositoryId, so.getId().toString(), -1,
						-1, null);
				odList = CmisObjectService.Impl.getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.SOURCE, so, userName, maxItemsInt, skipCountInt, filterArray);
			} else if (relationshipDirection == RelationshipDirection.TARGET) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipByTargetId(repositoryId, so.getId().toString(), -1,
						-1, null);
				odList = CmisObjectService.Impl.getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.TARGET, so, userName, maxItemsInt, skipCountInt, filterArray);
			} else if (relationshipDirection == RelationshipDirection.EITHER) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipBySourceId(repositoryId, so.getId().toString(), -1,
						-1, null);
				odList = CmisObjectService.Impl.getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.BOTH, so, userName, maxItemsInt, skipCountInt, filterArray);
			}

			result.setObjects(odList);
			result.setNumItems(BigInteger.valueOf(totalSize.size()));
			result.setHasMoreItems(totalSize.size() > maxItemsInt - 1);

			if (LOG.isDebugEnabled()) {
				LOG.debug("ObjectRelationships: {}", result.getObjects());
			}
			return result;

		}
	}
}
