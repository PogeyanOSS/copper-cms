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
import java.util.stream.Stream;

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
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.api.utils.TracingWriter;
import com.pogeyan.cmis.impl.utils.DBUtils;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class CmisRelationshipService {
	private static final Logger LOG = LoggerFactory.getLogger(CmisNavigationService.class);

	public static class Impl {
		/**
		 * CMIS getObjectRelationships.
		 */
		public static ObjectList getObjectRelationships(String repositoryId, String objectId,
				Boolean includeSubRelationshipTypes, RelationshipDirection relationshipDirection, String typeId,
				String filter, Boolean includeAllowableActions, BigInteger maxItems, BigInteger skipCount,
				ObjectInfoHandler objectInfos, IUserObject userObject, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException, MongoException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisRelationshipService::getobjectRelationships", null);
			String[] principalIds = Helpers.getPrincipalIds(userObject);
			String systemAdmin = System.getenv("SYSTEM_ADMIN");
			boolean aclPropagation = Stream.of(userObject.getGroups())
					.anyMatch(a -> a.getGroupDN() != null && a.getGroupDN().equals(systemAdmin)) ? false : true;
			IBaseObject so = null;
			try {
				so = DBUtils.BaseDAO.getByObjectId(repositoryId, principalIds, aclPropagation, objectId, null, typeId);
			} catch (Exception e) {
				LOG.error("Method name: {}, getObjectRelationships Exception: {}, repositoryid: {}, TraceId: {}",
						"getObjectRelationships", ExceptionUtils.getStackTrace(e), repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span),
								ErrorMessages.MONGO_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new MongoException(TracingWriter.log(String.format(ErrorMessages.EXCEPTION, e.toString()), span));
			}

			if (so == null) {
				LOG.error("Method name: {}, getObjectRelationships Exception: {}, {}, repositoryid: {}, traceId: {}",
						"getObjectRelationships", "Unknown object id", objectId, repositoryId,
						span != null ? span.getTraceId() : null);
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span),
								ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisObjectNotFoundException(
						TracingWriter.log(String.format(ErrorMessages.UNKNOWN_OBJECT, objectId), span));
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
				totalSize = DBUtils.RelationshipDAO.getRelationshipBySourceId(repositoryId, so.getId().toString(), true,
						-1, -1, null, typeId);
				odList = CmisObjectService.Impl.getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.SOURCE, so, userObject, maxItemsInt, skipCountInt, filterArray);
			} else if (relationshipDirection == RelationshipDirection.TARGET) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipByTargetId(repositoryId, so.getId().toString(), true,
						-1, -1, null, typeId);
				odList = CmisObjectService.Impl.getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.TARGET, so, userObject, maxItemsInt, skipCountInt, filterArray);
			} else if (relationshipDirection == RelationshipDirection.EITHER) {
				totalSize = DBUtils.RelationshipDAO.getRelationshipBySourceId(repositoryId, so.getId().toString(), true,
						-1, -1, null, typeId);
				odList = CmisObjectService.Impl.getRelationships(repositoryId, includeAllowableActions,
						IncludeRelationships.BOTH, so, userObject, maxItemsInt, skipCountInt, filterArray);
			}

			result.setObjects(odList);
			result.setNumItems(BigInteger.valueOf(totalSize.size()));
			result.setHasMoreItems(totalSize.size() > maxItemsInt - 1);

			if (result != null) {
				LOG.debug("ObjectRelationships result count: {}", result.getNumItems());
			}
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return result;

		}
	}
}
