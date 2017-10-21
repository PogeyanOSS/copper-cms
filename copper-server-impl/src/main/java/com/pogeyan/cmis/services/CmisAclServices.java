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
import org.apache.chemistry.opencmis.commons.server.ObjectInfoHandler;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.DBUtils;
import com.pogeyan.cmis.MongoTypeValidator;
import com.pogeyan.cmis.data.objects.MAce;
import com.pogeyan.cmis.data.objects.MAclImpl;
import com.pogeyan.cmis.data.objects.MBaseObject;
import com.pogeyan.cmis.data.objects.MToken;

public class CmisAclServices {

	private static final Logger LOG = LoggerFactory.getLogger(CmisAclServices.class);

	public static class Impl {
		public static Acl getAcl(String repositoryId, String objectId, Boolean onlyBasicPermissions,
				ExtensionsData extension, ObjectInfoHandler objectInfos, String userName)
				throws CmisObjectNotFoundException {
			LOG.info("getAcl on objectId: {} , repository: {}", objectId, repositoryId);
			MBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, new ObjectId(objectId), null);
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
			Acl addAces = MongoTypeValidator.impl.expandAclMakros(userName, aclAdd);
			Acl removeAces = MongoTypeValidator.impl.expandAclMakros(userName, aclRemove);
			if (LOG.isDebugEnabled() && addAces != null && removeAces != null) {
				LOG.debug("Adding {} , removing {} given ACEs", addAces.getAces(), removeAces.getAces());
			}

			MBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, new ObjectId(objectId), null);
			if (data == null) {
				LOG.error("Unknown object id: {}", objectId);
				throw new CmisObjectNotFoundException("Unknown object id: ", objectId);
			}
			MToken token = new MToken(4, System.currentTimeMillis());
			switch (aclPropagation) {
			case REPOSITORYDETERMINED: {
				MAclImpl newData = validateAcl(addAces, removeAces, data, id, aclPropagation.name());
				DBUtils.BaseDAO.updateAcl(repositoryId, newData, token, new ObjectId(objectId));
				break;
			}
			case OBJECTONLY:
				MAclImpl newData = validateAcl(addAces, removeAces, data, id, aclPropagation.name());
				DBUtils.BaseDAO.updateAcl(repositoryId, newData, token, new ObjectId(objectId));
				break;
			case PROPAGATE:
				MAclImpl aclData = validateAcl(addAces, removeAces, data, id, aclPropagation.name());
				DBUtils.BaseDAO.updateAcl(repositoryId, aclData, token, new ObjectId(objectId));
				break;
			}
			MBaseObject newData = DBUtils.BaseDAO.getByObjectId(repositoryId, new ObjectId(objectId), null);
			if (LOG.isDebugEnabled()) {
				LOG.debug("newData after applyAcl: {}", newData.getAcl().getAces());
			}
			return newData.getAcl();
		}

		private static MAclImpl validateAcl(Acl addAces, Acl removeAces, MBaseObject object, List<String> id,
				String aclPropagation) {
			List<MAce> acess = new ArrayList<MAce>();
			if (addAces != null) {
				for (Ace dataAce : object.getAcl().getAces()) {
					for (Ace ace : addAces.getAces()) {
						if (dataAce.getPrincipalId().equalsIgnoreCase(ace.getPrincipalId())) {
							MAce aclace = new MAce(ace.getPrincipalId(), ace.getPermissions());
							acess.add(aclace);
							id.add(ace.getPrincipalId());
						}
					}
				}
				acess = getAce(object.getAcl().getAces(), acess, id);
				acess = getAce(addAces.getAces(), acess, id);

				if (removeAces != null) {
					for (Ace ace : removeAces.getAces()) {
						int i = 0;
						for (String userId : id) {
							if (userId.equalsIgnoreCase(ace.getPrincipalId())) {
								acess.remove(i);
							}
							i++;
						}
					}
				}
			}

			MAclImpl aclimpl = new MAclImpl(acess, aclPropagation, false);
			if (LOG.isDebugEnabled() && aclimpl != null) {
				LOG.debug("ValidatedAcl: {}", aclimpl.getAces());
			}
			return aclimpl;
		}

		private static List<MAce> getAce(List<Ace> data, List<MAce> newList, List<String> id) {
			if (id.isEmpty()) {
				for (Ace dataAce : data) {
					MAce aclace = new MAce(dataAce.getPrincipalId(), dataAce.getPermissions());
					newList.add(aclace);
					id.add(dataAce.getPrincipalId());
				}
			} else {
				for (Ace dataAce : data) {
					if (!id.contains(dataAce.getPrincipalId())) {
						MAce aclace = new MAce(dataAce.getPrincipalId(), dataAce.getPermissions());
						newList.add(aclace);
						id.add(dataAce.getPrincipalId());
					}
				}
			}
			return newList;
		}
	}
}