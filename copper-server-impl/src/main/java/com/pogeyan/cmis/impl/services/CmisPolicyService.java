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
import java.util.List;

import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.impl.utils.DBUtils;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;

public class CmisPolicyService {
	private static final Logger LOG = LoggerFactory.getLogger(CmisPolicyService.class);

	public static class Impl {
		/**
		 * getAppliedPolicies() method get the list of applied policies.
		 */
		public static List<ObjectData> getAppliedPolicies(String repositoryId, String objectId, String filter,
				IUserObject userObject, String typeId) throws CmisObjectNotFoundException {
			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, typeId);
			List<ObjectData> res = new ArrayList<ObjectData>();
			if (data == null) {
				LOG.error("Method name: {}, unknown object id: {}, repository: {}", "getAppliedPolicies", objectId,
						repositoryId);
				throw new CmisObjectNotFoundException("Unknown object id: " + objectId);
			}
			List<String> polIds = data.getPolicies();
			if (null != polIds && polIds.size() > 0) {
				for (String polId : polIds) {
					IBaseObject policy = DBUtils.BaseDAO.getByObjectId(repositoryId, polId, null, data.getTypeId());
					ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, policy, null, false,
							false, true, null, null, IncludeRelationships.NONE, userObject, null, null);

					res.add(objectData);
				}
			}
			if (res != null) {
				LOG.debug("Applied policies result count: {}", res.size());
			}
			return res;
		}

		/**
		 * removePolicy() method from cmis object.
		 */
		public static void removePolicy(String repositoryId, String policyId, String objectId, String userName,
				String typeId) throws CmisInvalidArgumentException, CmisObjectNotFoundException {
			List<String> polIds = null;
			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, typeId);
			if (data == null) {
				LOG.error("Method name: {}, unknown object id: {}, repository: {}", "removePolicy", objectId,
						repositoryId);
				throw new CmisObjectNotFoundException("Unknown object id: " + objectId);
			}
			TokenImpl token = new TokenImpl(TokenChangeType.SECURITY, System.currentTimeMillis());
			polIds = data.getPolicies();
			if (null == polIds || !(polIds.contains(policyId))) {
				LOG.error(
						"Method name: {}, policyId: {}, cannot be removed because it is not applied to object: {}, repository: {}",
						"removePolicy", policyId, objectId, repositoryId);
				throw new CmisInvalidArgumentException(
						"Policy id " + policyId + "cannot be removed because it is not applied to object " + objectId);
			}
			polIds.remove(policyId);
			DBUtils.BaseDAO.updatePolicy(repositoryId, polIds, objectId, token, typeId);
			if (polIds != null) {
				LOG.debug("PolicyObject after removing policyids are: {}", polIds);
			}
		}

		/**
		 * applyPolicy() method for an object.
		 */
		public static void applyPolicy(String repositoryId, String policyId, String objectId, String typeId)
				throws CmisObjectNotFoundException, CmisInvalidArgumentException {
			List<String> polIds = null;
			IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null, typeId);
			if (data == null) {
				LOG.error("Method name: {}, unknown object id: {}, repository: {}", "applyPolicy", objectId,
						repositoryId);
				throw new CmisObjectNotFoundException("Unknown object id: " + objectId);
			}
			IBaseObject policy = DBUtils.BaseDAO.getByObjectId(repositoryId, policyId, null, data.getTypeId());
			if (policy == null) {
				LOG.error("Method name: {}, Unknown policy id: {}, repository: {}", "applyPolicy", policyId,
						repositoryId);
				throw new CmisObjectNotFoundException("Unknown policy id: " + policyId);
			}
			TokenImpl token = new TokenImpl(TokenChangeType.SECURITY, System.currentTimeMillis());
			polIds = data.getPolicies();
			if (null != polIds && polIds.contains(policyId)) {
				LOG.error(
						"Method name: {}, policyId: {}, cannot be added because it is already applied to object: {}, repository: {}",
						"applyPolicy", policyId, objectId, repositoryId);
				throw new CmisInvalidArgumentException("Policy id " + policyId
						+ "cannot be added because it is already applied to object " + objectId);
			}
			if (polIds == null) {
				polIds = new ArrayList<String>();
			}
			polIds.add(policyId);
			DBUtils.BaseDAO.updatePolicy(repositoryId, polIds, objectId, token, typeId);
			if (polIds != null) {
				LOG.debug("PolicyObject after adding policyids are: {}", polIds);
			}
		}
	}
}
