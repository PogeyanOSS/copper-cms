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

import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.DBUtils;
import com.pogeyan.cmis.api.data.TokenImpl;
import com.pogeyan.cmis.data.objects.MBaseObject;

public class CmisPolicyService {
	private static final Logger LOG = LoggerFactory.getLogger(CmisPolicyService.class);

	public static class Impl {
		/**
		 * getAppliedPolicies() method get the list of applied policies.
		 */
		public static List<ObjectData> getAppliedPolicies(String repositoryId, String objectId, String filter,
				String username) throws CmisObjectNotFoundException {
			LOG.info("getAppliedPolicies on objectId: {} , repository: {}", objectId, repositoryId);
			MBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, new ObjectId(objectId), null);
			List<ObjectData> res = new ArrayList<ObjectData>();
			if (data == null) {
				LOG.error("Unknown object id:{}", objectId);
				throw new CmisObjectNotFoundException("Unknown object id: " + objectId);
			}
			List<String> polIds = data.getPolicies();
			if (null != polIds && polIds.size() > 0) {
				for (String polId : polIds) {
					MBaseObject policy = DBUtils.BaseDAO.getByObjectId(repositoryId, new ObjectId(polId), null);
					ObjectData objectData = CmisObjectService.Impl.compileObjectData(repositoryId, policy, null, false,
							false, true, null, null, IncludeRelationships.NONE, username);

					res.add(objectData);
				}
			}
			if (LOG.isDebugEnabled() && res != null) {
				LOG.debug("Applied Policies: {}", res.toString());
			}
			return res;
		}

		/**
		 * removePolicy() method from cmis object.
		 */
		public static void removePolicy(String repositoryId, String policyId, ObjectId objectId, String userName)
				throws CmisInvalidArgumentException, CmisObjectNotFoundException {

			LOG.info("removePolicy on objectId: {} , with policyId: {} , repository: {}", objectId, policyId,
					repositoryId);
			List<String> polIds = null;
			MBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null);
			if (data == null) {
				LOG.error("Unknown object id:{}", objectId);
				throw new CmisObjectNotFoundException("Unknown object id: " + objectId);
			}
			TokenImpl token = new TokenImpl(3, System.currentTimeMillis());
			polIds = data.getPolicies();
			if (null == polIds || !(polIds.contains(policyId))) {
				LOG.error("policyId:{},cannot be removed, because it is not applied to object:{} ", policyId, objectId);
				throw new CmisInvalidArgumentException(
						"Policy id " + policyId + "cannot be removed, because it is not applied to object " + objectId);
			}
			polIds.remove(policyId);
			DBUtils.BaseDAO.updatePolicy(repositoryId, polIds, objectId, token);
			if (LOG.isDebugEnabled()) {
				LOG.debug("PolicyObject after removing policy: {} is : {} ", policyId, polIds.toString());
			}
		}

		/**
		 * applyPolicy() method for an object.
		 */
		public static void applyPolicy(String repositoryId, String policyId, ObjectId objectId)
				throws CmisObjectNotFoundException, CmisInvalidArgumentException {

			LOG.info("applyPolicy on objectId: {} , with policyId: {} , repository : {}", objectId, policyId,
					repositoryId);
			List<String> polIds = null;
			MBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, objectId, null);
			if (data == null) {
				LOG.error("Unknown object id:{}", objectId);
				throw new CmisObjectNotFoundException("Unknown object id: " + objectId);
			}
			MBaseObject policy = DBUtils.BaseDAO.getByObjectId(repositoryId, new ObjectId(policyId), null);
			if (policy == null) {
				LOG.error("Unknown policy id:{}", policyId);
				throw new CmisObjectNotFoundException("Unknown policy id: " + policyId);
			}
			TokenImpl token = new TokenImpl(3, System.currentTimeMillis());
			polIds = data.getPolicies();
			if (null != polIds && polIds.contains(policyId)) {
				LOG.error("policyId:{},cannot be added,  because it is already applied to object:{} ", policyId,
						objectId);
				throw new CmisInvalidArgumentException("Policy id " + policyId
						+ "cannot be added, because it is already applied to object " + objectId);
			}
			if (polIds == null) {
				polIds = new ArrayList<String>();
			}
			polIds.add(policyId);
			DBUtils.BaseDAO.updatePolicy(repositoryId, polIds, objectId, token);
			if (LOG.isDebugEnabled()) {
				LOG.debug("PolicyObject after adding policy: {} is : {} ", policyId, polIds.toString());
			}
		}
	}
}
