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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.ExtensionsData;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.CapabilityAcl;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.impl.utils.DBUtils;
import com.pogeyan.cmis.impl.utils.TypeValidators;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class CmisAclServices {

	private static final Logger LOG = LoggerFactory.getLogger(CmisAclServices.class);

	public static class Impl {
		public static Acl getAcl(String repositoryId, String objectId, Boolean onlyBasicPermissions,
				ExtensionsData extension, ObjectInfoHandler objectInfos, IUserObject userObject, String typeId,
				String tracingId, ISpan parentSpan) throws CmisObjectNotFoundException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisAclService::getAcl", null);
			Map<String, Object> attrMap = new HashMap<String, Object>();
			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, typeId);
			if (data == null) {
				LOG.error("Method name: {}, unknown object id: {}, repository: {}, TraceId: {}", "getAcl", objectId,
						repositoryId, span.getTraceId());
				attrMap.put("error", "Unknown object id:" + objectId + " ,TraceId: " + span.getTraceId());
				TracingApiServiceFactory.getApiService().updateSpan(span, true, "Unknown object id", attrMap);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
				throw new CmisObjectNotFoundException(
						"Unknown object id: " + objectId + " ,TraceId: " + span.getTraceId());
			}
			ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, data, null, true, true,
					false, objectInfos, null, null, userObject, tracingId, span);

			LOG.debug("get acl result data: {}", objectData != null ? objectData.getAcl() : null);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			return objectData.getAcl();
		}

		public static Acl applyAcl(String repositoryId, String objectId, Acl aclAdd, Acl aclRemove,
				AclPropagation aclPropagation, ExtensionsData extension, ObjectInfoHandler objectInfos,
				CapabilityAcl capability, String userName, String typeId, String tracingId, ISpan parentSpan)
				throws CmisObjectNotFoundException {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisAclService::applyAcl", null);
			Map<String, Object> attrMap = new HashMap<String, Object>();
			List<String> id = new ArrayList<String>();
			Acl addAces = TypeValidators.impl.expandAclMakros(userName, aclAdd);
			Acl removeAces = TypeValidators.impl.expandAclMakros(userName, aclRemove);
			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, typeId);
			if (data == null) {
				LOG.error("Method name: {}, unknown object id: {}, repository: {}, TraceId: {}", "applyAcl", objectId,
						repositoryId, span.getTraceId());
				attrMap.put("error", "Unknown object id:" + objectId + " ,TraceId: " + span.getTraceId());
				TracingApiServiceFactory.getApiService().updateSpan(span, true, "Unknown object id", attrMap);
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
				throw new CmisObjectNotFoundException(
						"Unknown object id: " + objectId + " ,TraceId:" + span.getTraceId());
			}

			Long modifiedTime = System.currentTimeMillis();
			TokenImpl token = new TokenImpl(TokenChangeType.SECURITY, modifiedTime);
			switch (aclPropagation) {
			case REPOSITORYDETERMINED: {
				AccessControlListImplExt newData = validateAcl(addAces, removeAces, data, id, aclPropagation.name(),
						tracingId, span);
				DBUtils.BaseDAO.updateAcl(repositoryId, newData, token, objectId, modifiedTime, typeId);
				break;
			}
			case OBJECTONLY:
				AccessControlListImplExt newData = validateAcl(addAces, removeAces, data, id, aclPropagation.name(),
						tracingId, span);
				DBUtils.BaseDAO.updateAcl(repositoryId, newData, token, objectId, modifiedTime, typeId);
				break;
			case PROPAGATE:
				AccessControlListImplExt aclData = validateAcl(addAces, removeAces, data, id, aclPropagation.name(),
						tracingId, span);
				DBUtils.BaseDAO.updateAcl(repositoryId, aclData, token, objectId, modifiedTime, typeId);
				break;
			}
			IBaseObject newData = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, data.getTypeId());
			LOG.debug("After applyAcl new aces: {}", newData != null ? newData.getAcl() : null);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			return newData.getAcl();
		}

		private static AccessControlListImplExt validateAcl(Acl addAces, Acl removeAces, IBaseObject object,
				List<String> id, String aclPropagation, String tracingId, ISpan parentSpan) {
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"CmisAclService::validateAcl", null);
			List<Ace> aces = new ArrayList<Ace>();
			if (addAces != null) {
				if (object.getAcl() != null) {
					for (Ace dataAce : object.getAcl().getAces()) {
						for (Ace ace : addAces.getAces()) {
							if (dataAce.getPrincipalId().equalsIgnoreCase(ace.getPrincipalId())) {
								AccessControlEntryImpl ace1 = new AccessControlEntryImpl(
										new AccessControlPrincipalDataImpl(ace.getPrincipalId()), ace.getPermissions());
								aces.add(ace1);
								id.add(ace.getPrincipalId());
							}
						}
					}
					aces = getAce(object.getAcl().getAces(), aces, id);
				} else {
					for (Ace ace : addAces.getAces()) {
						AccessControlEntryImpl ace1 = new AccessControlEntryImpl(
								new AccessControlPrincipalDataImpl(ace.getPrincipalId()), ace.getPermissions());
						aces.add(ace1);
						id.add(ace.getPrincipalId());
					}
				}
				aces = getAce(addAces.getAces(), aces, id);
			}
			if (removeAces != null) {
				if (object.getAcl() != null) {
					if (aces.isEmpty()) {
						for (Ace ace : object.getAcl().getAces()) {
							AccessControlEntryImpl ace1 = new AccessControlEntryImpl(
									new AccessControlPrincipalDataImpl(ace.getPrincipalId()), ace.getPermissions());
							aces.add(ace1);
							id.add(ace.getPrincipalId());
						}
						aces = getAce(object.getAcl().getAces(), aces, id);
					}
					aces = aces.stream()
							.filter(ace -> removeAces.getAces().stream()
									.filter(remAce -> remAce.getPrincipalId().equals(ace.getPrincipalId())).count() < 1)
							.collect(Collectors.toList());
				}
			}

			if (addAces == null && removeAces == null) {
				aces.addAll(object.getAcl().getAces());
			}

			Set<Ace> removeDuplicate = aces.stream()
					.collect(Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(Ace::getPrincipalId))));
			aces = new ArrayList<Ace>(removeDuplicate);
			AccessControlListImplExt aclimpl = new AccessControlListImplExt(aces, aclPropagation, false);
			LOG.debug("After validatedAces: {}", aclimpl != null ? aclimpl.getAces() : null);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			return aclimpl;
		}

		private static List<Ace> getAce(List<Ace> data, List<Ace> newList, List<String> id) {
			if (id.isEmpty()) {
				for (Ace dataAce : data) {
					AccessControlEntryImpl aces = new AccessControlEntryImpl(
							new AccessControlPrincipalDataImpl(dataAce.getPrincipalId()), dataAce.getPermissions());
					newList.add(aces);
					id.add(dataAce.getPrincipalId());
				}
			} else {
				for (Ace dataAce : data) {
					if (!id.contains(dataAce.getPrincipalId())) {
						AccessControlEntryImpl aces = new AccessControlEntryImpl(
								new AccessControlPrincipalDataImpl(dataAce.getPrincipalId()), dataAce.getPermissions());
						newList.add(aces);
						id.add(dataAce.getPrincipalId());
					}
				}
			}
			return newList;
		}
	}
}
