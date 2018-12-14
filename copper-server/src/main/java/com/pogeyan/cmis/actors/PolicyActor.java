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

import java.util.HashMap;
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
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.browser.BrowserConstants;
import com.pogeyan.cmis.impl.services.CmisObjectService;
import com.pogeyan.cmis.impl.services.CmisPolicyService;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class PolicyActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(PolicyActor.class);

	@Override
	public String getName() {
		return "policy";
	}

	public PolicyActor() {
		this.registerMessageHandle("policies", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.policies((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("applyPolicy", PostRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.applyPolicy((PostRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("removePolicy", PostRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.removePolicy((PostRequest) t, (HashMap<String, Object>) b))));
	}

	private JSONArray policies(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisObjectNotFoundException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "PolicyActor::policies",
				null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName(), span.getTraceId()),
							ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisObjectNotFoundException(
					String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName(), span.getTraceId()));
		}
		String objectId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, get appiled policies using this id: {}, repositoryId: {}, filter: {}",
				"getAppliedPolicies", objectId, request.getRepositoryId(), filter);
		List<ObjectData> policies = CmisPolicyService.Impl.getAppliedPolicies(request.getRepositoryId(), objectId,
				filter, null, request.getTypeId(), tracingId, span);
		JSONArray jsonPolicies = new JSONArray();
		if (policies != null) {
			for (ObjectData policy : policies) {
				jsonPolicies.add(JSONConverter.convert(policy, null, JSONConverter.PropertyMode.OBJECT, succinct,
						dateTimeFormat));
			}
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonPolicies;

	}

	private JSONObject applyPolicy(PostRequest request, HashMap<String, Object> baggage)
			throws CmisObjectNotFoundException, CmisInvalidArgumentException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"PolicyActor::applyPolicy", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName(), span.getTraceId()),
							ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisObjectNotFoundException(
					String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName(), span.getTraceId()));
		}
		String objectId = request.getObjectId();
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, apply policy using this id: {}, repositoryId: {}, policyId: {}", "applyPolicy",
				objectId, request.getRepositoryId(), request.getPolicyId());
		CmisPolicyService.Impl.applyPolicy(request.getRepositoryId(), request.getPolicyId(), objectId,
				request.getTypeId(), tracingId, span);
		LOG.info("Method name: {}, getting object using this id: {}, repositoryId: {}", "getObject", objectId,
				request.getRepositoryId());
		ObjectData object = CmisObjectService.Impl.getSimpleObject(request.getRepositoryId(), objectId,
				request.getUserObject(), BaseTypeId.CMIS_POLICY, request.getTypeId());
		if (object == null) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(String.format(ErrorMessages.OBJECT_NULL, span.getTraceId()),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(String.format(ErrorMessages.OBJECT_NULL, span.getTraceId()));
		}

		JSONObject jsonObject = JSONConverter.convert(object, null, JSONConverter.PropertyMode.OBJECT, succinct,
				dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonObject;

	}

	private JSONObject removePolicy(PostRequest request, HashMap<String, Object> baggage)
			throws CmisInvalidArgumentException, CmisObjectNotFoundException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"PolicyActor::removePolicy", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName(), span.getTraceId()),
							ErrorMessages.OBJECT_NOT_FOUND_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisObjectNotFoundException(
					String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName(), span.getTraceId()));
		}
		String objectId = request.getObjectId();
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, remove policy using this id: {}, repositoryId: {}, policyId: {}", "removePolicy",
				objectId, request.getRepositoryId(), request.getPolicyId());
		CmisPolicyService.Impl.removePolicy(request.getRepositoryId(), request.getPolicyId(), objectId,
				request.getUserObject().getUserDN(), request.getTypeId(), tracingId, span);
		LOG.info("Method name: {}, getting object using this id: {}, repositoryId: {}", "getObject", objectId,
				request.getRepositoryId());
		ObjectData object = CmisObjectService.Impl.getSimpleObject(request.getRepositoryId(), objectId,
				request.getUserObject(), BaseTypeId.CMIS_POLICY, request.getTypeId());
		if (object == null) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(String.format(ErrorMessages.OBJECT_NULL, span.getTraceId()),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(String.format(ErrorMessages.OBJECT_NULL, span.getTraceId()));
		}

		JSONObject jsonObject = JSONConverter.convert(object, null, JSONConverter.PropertyMode.OBJECT, succinct,
				dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonObject;

	}
}
