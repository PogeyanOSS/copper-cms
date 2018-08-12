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
package com.pogeyan.cmis.actors;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.DateTimeFormat;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.json.JSONArray;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.impl.services.CmisObjectService;
import com.pogeyan.cmis.impl.services.CmisPolicyService;

public class PolicyActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(PolicyActor.class);

	@Override
	public String getName() {
		return "policy";
	}

	public PolicyActor() {
		this.registerMessageHandle("policies", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.policies((QueryGetRequest) t))));

		this.registerMessageHandle("applyPolicy", PostRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.applyPolicy((PostRequest) t))));

		this.registerMessageHandle("removePolicy", PostRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.removePolicy((PostRequest) t))));
	}

	private JSONArray policies(QueryGetRequest request) throws CmisObjectNotFoundException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, get appiled policies using this id: {}, repositoryId: {}, filter: {}",
				"getAppliedPolicies", objectId, request.getRepositoryId(), filter);
		List<ObjectData> policies = CmisPolicyService.Impl.getAppliedPolicies(request.getRepositoryId(), objectId,
				filter, null);
		JSONArray jsonPolicies = new JSONArray();
		if (policies != null) {
			for (ObjectData policy : policies) {
				jsonPolicies.add(JSONConverter.convert(policy, null, JSONConverter.PropertyMode.OBJECT, succinct,
						dateTimeFormat));
			}
		}
		return jsonPolicies;

	}

	private JSONObject applyPolicy(PostRequest request)
			throws CmisObjectNotFoundException, CmisInvalidArgumentException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = request.getObjectId();
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, apply policy using this id: {}, repositoryId: {}, policyId: {}", "applyPolicy",
				objectId, request.getRepositoryId(), request.getPolicyId());
		CmisPolicyService.Impl.applyPolicy(request.getRepositoryId(), request.getPolicyId(), objectId);
		LOG.info("Method name: {}, getting object using this id: {}, repositoryId: {}", "getObject", objectId,
				request.getRepositoryId());
		ObjectData object = CmisObjectService.Impl.getSimpleObject(request.getRepositoryId(), objectId,
				request.getUserObject(), BaseTypeId.CMIS_POLICY);
		if (object == null) {
			throw new CmisRuntimeException("Object is null!");
		}

		JSONObject jsonObject = JSONConverter.convert(object, null, JSONConverter.PropertyMode.OBJECT, succinct,
				dateTimeFormat);
		return jsonObject;

	}

	private JSONObject removePolicy(PostRequest request)
			throws CmisInvalidArgumentException, CmisObjectNotFoundException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = request.getObjectId();
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, remove policy using this id: {}, repositoryId: {}, policyId: {}", "removePolicy",
				objectId, request.getRepositoryId(), request.getPolicyId());
		CmisPolicyService.Impl.removePolicy(request.getRepositoryId(), request.getPolicyId(), objectId,
				request.getUserObject().getUserDN());
		LOG.info("Method name: {}, getting object using this id: {}, repositoryId: {}", "getObject", objectId,
				request.getRepositoryId());
		ObjectData object = CmisObjectService.Impl.getSimpleObject(request.getRepositoryId(), objectId,
				request.getUserObject(), BaseTypeId.CMIS_POLICY);
		if (object == null) {
			throw new CmisRuntimeException("Object is null!");
		}

		JSONObject jsonObject = JSONConverter.convert(object, null, JSONConverter.PropertyMode.OBJECT, succinct,
				dateTimeFormat);
		return jsonObject;

	}
}
