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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.data.PolicyIdList;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChangeEventInfoDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ObjectListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PolicyIdListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.server.ObjectInfoImpl;
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.services.MDiscoveryServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;
import com.pogeyan.cmis.api.utils.TracingWriter;

public class CmisDiscoveryService {
	private static final Logger LOG = LoggerFactory.getLogger(CmisDiscoveryService.class);

	public static class Impl {
		public static ObjectList getContentChanges(String repositoryId, Holder<String> changeLogToken,
				Boolean includeProperties, String filter, String orderBy, Boolean includePolicyIds, Boolean includeAcl,
				BigInteger maxItems, ObjectInfoHandler objectInfos, IUserObject userObject, String tracingId,
				ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisDiscoveryService::getContentChanges", null);
			MDiscoveryServiceDAO discoveryObjectMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDiscoveryServiceDAO.class);
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			int maxItemsInt = maxItems == null ? 10 : maxItems.intValue();
			if (changeLogToken == null || changeLogToken.getValue() == null) {
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(ErrorMessages.TOKEN_VALUE_NULL, span.getTraceId()),
								ErrorMessages.INVALID_EXCEPTION, repositoryId, true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(ErrorMessages.TOKEN_VALUE_NULL, span.getTraceId()));
			}
			String[] principalIds = com.pogeyan.cmis.api.utils.Helpers.getPrincipalIds(userObject);

			ObjectListImpl objList = new ObjectListImpl();
			long childrenCount = 0;
			// convert ObjectInFolderContainerList to objectList
			List<ObjectData> lod = new ArrayList<ObjectData>();
			// if (maxItemsInt == 0) {
			// maxItemsInt = 10;
			// }
			String[] filterArray = null;
			// split filter
			Set<String> filterCollection = splitFilter(filter);
			if (filter != null && filterCollection != null && filterCollection.size() > 0) {
				filterArray = Helpers.getFilterArray(filterCollection, true);
				filterArray = Arrays.copyOf(filterArray, filterArray.length + 1);
				filterArray[filterArray.length - 1] = "internalPath";
			}
			if (orderBy != null) {
				String[] orderQuery = orderBy.split(",");
				orderBy = Arrays.stream(orderQuery).map(t -> getOrderByName(t)).collect(Collectors.joining(","));
			}

			List<? extends IBaseObject> latestChangesObjects = discoveryObjectMorphiaDAO.getLatestChanges(
					Long.parseLong(changeLogToken.getValue()), maxItemsInt, filterArray, orderBy,
					Helpers.splitFilterQuery(filter), typeManagerDAO, includeAcl, principalIds);
			if (latestChangesObjects.size() > 0) {
				childrenCount = discoveryObjectMorphiaDAO.getLatestTokenChildrenSize(
						Long.parseLong(changeLogToken.getValue()), Helpers.splitFilterQuery(filter), typeManagerDAO,
						includeAcl, principalIds);
				for (IBaseObject object : latestChangesObjects) {
					if (object != null) {
						ObjectDataImpl odImpl = getObjectDataImpl(repositoryId, object, filterCollection,
								includeProperties, includePolicyIds, includeAcl, userObject);
						lod.add(odImpl);
						// Acl Check for propagate or object only

						// if (includeAcl) {
						// List<AccessControlListImplExt> mAcl =
						// CmisNavigationService.Impl.getParentAcl(repositoryId,
						// object.getInternalPath(), object.getAcl());
						// boolean objectOnly = true;
						// for (AccessControlListImplExt acl : mAcl) {
						// if (acl != null) {
						// if (acl.getAclPropagation().equalsIgnoreCase("PROPAGATE")) {
						// List<Ace> listAce = getListAce(acl, principalIds);
						// if (listAce.size() >= 1) {
						// ObjectDataImpl odImpl = getObjectDataImpl(repositoryId, object,
						// filterCollection, includeProperties, includePolicyIds, includeAcl);
						// lod.add(odImpl);
						// objectOnly = false;
						// break;
						// }
						// }
						// }
						// }
						//
						// if (objectOnly) {
						// List<Ace> listAce = getListAce(object.getAcl(), principalIds);
						// if (listAce.size() >= 1) {
						// ObjectDataImpl odImpl = getObjectDataImpl(repositoryId, object,
						// filterCollection,
						// includeProperties, includePolicyIds, includeAcl);
						// lod.add(odImpl);
						// }
						// }
						// } else {
						// }
					}
				}

			}

			objList.setObjects(lod);
			objList.setNumItems(BigInteger.valueOf(childrenCount));
			objList.setHasMoreItems(childrenCount > maxItemsInt);
			LOG.debug("getContentChanges result data count: {}", objList != null ? objList.getNumItems() : objList);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return objList;
		}

		private static ObjectDataImpl getObjectDataImpl(String repositoryId, IBaseObject object,
				Set<String> filterCollection, Boolean includeProperties, Boolean includePolicyIds, Boolean includeAcl,
				IUserObject userObject) {
			ObjectDataImpl odImpl = new ObjectDataImpl();
			if (!includeProperties) {
				Map<String, Object> custom = new HashMap<>();
				custom.put("cmis:objectId", object.getId());
				object.setProperties(null);
				object.setProperties(custom);
			}
			ObjectInfoImpl objectInfo = new ObjectInfoImpl();
			Properties props = null;
			if (object.getChangeToken().getChangeType().equals(TokenChangeType.DELETED)) {
				PropertiesImpl result = new PropertiesImpl();
				PropertyIdImpl pd = new PropertyIdImpl(PropertyIds.OBJECT_ID, (String) object.getId());
				pd.setDisplayName(PropertyIds.OBJECT_ID);
				pd.setQueryName(PropertyIds.OBJECT_ID);
				result.addProperty(pd);
				props = result;
			} else {
				props = CmisObjectService.Impl.compileProperties(repositoryId, object, filterCollection, objectInfo,
						userObject);
			}

			odImpl.setProperties(props);
			ChangeEventInfoDataImpl changeEventInfo = new ChangeEventInfoDataImpl();
			changeEventInfo.setChangeType(TokenChangeType.fromValue(object.getChangeToken().getChangeType()));
			GregorianCalendar eventTimestamp = new GregorianCalendar();
			eventTimestamp.setTimeInMillis(object.getChangeToken().getTime());
			changeEventInfo.setChangeTime(eventTimestamp);
			odImpl.setChangeEventInfo(changeEventInfo);
			if (includePolicyIds != null && includePolicyIds) {
				if (object.getPolicies().size() > 0) {
					PolicyIdListImpl policy = new PolicyIdListImpl();
					policy.setPolicyIds(object.getPolicies());
					odImpl.setPolicyIds(policy);
				} else {
					PolicyIdList policies = new PolicyIdListImpl();
					odImpl.setPolicyIds(policies);
				}

			}
			if (includeAcl != null && includeAcl) {
				odImpl.setAcl(object.getAcl());
			}
			return odImpl;

		}

		/**
		 * Splits a filter statement into a collection of properties. If
		 * <code>filter</code> is <code>null</code>, empty or one of the properties is
		 * '*' , an empty collection will be returned.
		 */
		private static Set<String> splitFilter(String filter) {
			if (filter == null) {
				return null;
			}

			if (filter.trim().length() == 0) {
				return null;
			}

			Set<String> result = new HashSet<String>();
			for (String s : filter.split(",")) {
				s = s.trim();
				if (s.equals("*")) {
					return null;
				} else if (s.length() > 0) {
					result.add(s);
				}
			}

			// set a few base properties
			// query name == id (for base type properties)
			result.add(PropertyIds.OBJECT_ID);
			result.add(PropertyIds.OBJECT_TYPE_ID);
			result.add(PropertyIds.BASE_TYPE_ID);
			result.add(PropertyIds.CHANGE_TOKEN);
			return result;
		}

		private static String getOrderByName(String orderBy) {
			String order[] = orderBy.split("\\s+");
			if (order.length > 0) {
				return com.pogeyan.cmis.api.utils.Helpers.getQueryName(orderBy.split("\\s+")[0]) + " "
						+ orderBy.split("\\s+")[1];
			} else {
				return com.pogeyan.cmis.api.utils.Helpers.getQueryName(orderBy);
			}
		}
	}
}
