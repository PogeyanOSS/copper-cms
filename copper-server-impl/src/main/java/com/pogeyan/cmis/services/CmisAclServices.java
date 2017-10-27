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
package com.pogeyan.cmis.services;

import java.util.ArrayList;
import java.util.List;

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

import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.utils.DBUtils;
import com.pogeyan.cmis.utils.TypeValidators;

public class CmisAclServices {

	private static final Logger LOG = LoggerFactory.getLogger(CmisAclServices.class);

	public static class Impl {
		public static Acl getAcl(String repositoryId, String objectId, Boolean onlyBasicPermissions,
				ExtensionsData extension, ObjectInfoHandler objectInfos, String userName)
				throws CmisObjectNotFoundException {
			LOG.info("getAcl on objectId: {} , repository: {}", objectId, repositoryId);
			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null);
			if (data == null) {
				LOG.error("Unknown object id: {}", objectId);
				throw new CmisObjectNotFoundException("Unknown object id: ", objectId);
			}
			ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, data, null, true, true,
					false, objectInfos, null, null, userName);
			if (LOG.isDebugEnabled()) {
				LOG.debug("Acl: {}", objectData.getAcl().getAces());
			}
			return objectData.getAcl();
		}

		public static Acl applyAcl(String repositoryId, String objectId, Acl aclAdd, Acl aclRemove,
				AclPropagation aclPropagation, ExtensionsData extension, ObjectInfoHandler objectInfos,
				CapabilityAcl capability, String userName) throws CmisObjectNotFoundException {
			LOG.info("applyAcl on objectId: {} , repository: {}", objectId, repositoryId);
			List<String> id = new ArrayList<String>();
			Acl addAces = TypeValidators.impl.expandAclMakros(userName, aclAdd);
			Acl removeAces = TypeValidators.impl.expandAclMakros(userName, aclRemove);
			if (LOG.isDebugEnabled() && addAces != null && removeAces != null) {
				LOG.debug("Adding {} , removing {} given ACEs", addAces.getAces(), removeAces.getAces());
			}

			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null);
			if (data == null) {
				LOG.error("Unknown object id: {}", objectId);
				throw new CmisObjectNotFoundException("Unknown object id: ", objectId);
			}
			TokenImpl token = new TokenImpl(TokenChangeType.SECURITY, System.currentTimeMillis());
			switch (aclPropagation) {
			case REPOSITORYDETERMINED: {
				AccessControlListImplExt newData = validateAcl(addAces, removeAces, data, id, aclPropagation.name());
				DBUtils.BaseDAO.updateAcl(repositoryId, newData, token, objectId);
				break;
			}
			case OBJECTONLY:
				AccessControlListImplExt newData = validateAcl(addAces, removeAces, data, id, aclPropagation.name());
				DBUtils.BaseDAO.updateAcl(repositoryId, newData, token, objectId);
				break;
			case PROPAGATE:
				AccessControlListImplExt aclData = validateAcl(addAces, removeAces, data, id, aclPropagation.name());
				DBUtils.BaseDAO.updateAcl(repositoryId, aclData, token, objectId);
				break;
			}
			IBaseObject newData = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null);
			if (LOG.isDebugEnabled()) {
				LOG.debug("newData after applyAcl: {}", newData.getAcl().getAces());
			}
			return newData.getAcl();
		}

		private static AccessControlListImplExt validateAcl(Acl addAces, Acl removeAces, IBaseObject object, List<String> id,
				String aclPropagation) {
			List<Ace> aces = new ArrayList<Ace>();
			if (addAces != null) {
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
				aces = getAce(addAces.getAces(), aces, id);

				if (removeAces != null) {
					for (Ace ace : removeAces.getAces()) {
						int i = 0;
						for (String userId : id) {
							if (userId.equalsIgnoreCase(ace.getPrincipalId())) {
								aces.remove(i);
							}
							i++;
						}
					}
				}
			}

			AccessControlListImplExt aclimpl = new AccessControlListImplExt(aces, aclPropagation, false);
			if (LOG.isDebugEnabled() && aclimpl != null) {
				LOG.debug("ValidatedAcl: {}", aclimpl.getAces());
			}
			
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