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
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.CapabilityAcl;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
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
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.browser.BrowserConstants;
import com.pogeyan.cmis.impl.services.CmisAclServices;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class AclActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(AclActor.class);

	@Override
	public String getName() {
		return "acl";
	}

	public AclActor() {
		this.registerMessageHandle("acl", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getAcl((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("applyACL", PostRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.applyACL((PostRequest) t, (HashMap<String, Object>) b))));
	}

	private JSONObject applyACL(PostRequest t, HashMap<String, Object> baggage)
			throws CmisObjectNotFoundException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "AclActor::applyAcl",
				null);
		Map<String, Object> attrMap = new HashMap<String, Object>();

		String permission = t.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			attrMap.put("error", t.getUserName() + "is not authorized to applyAcl, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true,
					t.getUserName() + " is not authorized to applyAcl", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			throw new CmisRuntimeException(t.getUserName() + " is not authorized to applyAcl.");
		}
		String aclPro = t.getAclPropagation();
		String objectId = t.getObjectId();
		LOG.info("Method name: {}, apply acl for object using this id: {}, repositoryId: {}, addAcl: {}, removeAcl: {}",
				"applyACL", objectId, t.getRepositoryId(), t.getAddAcl(), t.getRemoveAcl());
		Acl objectAcl = CmisAclServices.Impl.applyAcl(t.getRepositoryId(), objectId, t.getAddAcl(), t.getRemoveAcl(),
				AclPropagation.fromValue(aclPro), null, null, CapabilityAcl.NONE, t.getUserObject().getUserDN(),
				tracingId, span);
		if (objectAcl == null) {
			attrMap.put("error", "object acl is null!, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true, "object acl is null!", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			throw new CmisRuntimeException("object acl is null!, TraceId:", span.getTraceId());
		}
		JSONObject jsonObject = JSONConverter.convert(objectAcl);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return jsonObject;

	}

	private JSONObject getAcl(QueryGetRequest t, HashMap<String, Object> baggage)
			throws CmisObjectNotFoundException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "AclActor::getAcl",
				null);
		Map<String, Object> attrMap = new HashMap<String, Object>();
		String permission = t.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			attrMap.put("error", t.getUserName() + "is not authorized to getAcl, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true,
					t.getUserName() + " is not authorized to applyAcl", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			throw new CmisRuntimeException(t.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = t.getObjectId();
		Boolean onlyBasicPermissions = t.getBooleanParameter(QueryGetRequest.PARAM_ONLY_BASIC_PERMISSIONS);
		LOG.info("Method name: {}, get acl using this id: {}, repositoryId: {}, onlyBasicPermissions: {}", "getAcl",
				objectId, t.getRepositoryId(), onlyBasicPermissions);
		Acl objectAcl = CmisAclServices.Impl.getAcl(t.getRepositoryId(), objectId, onlyBasicPermissions, null, null,
				t.getUserObject(), tracingId, span);
		if (objectAcl == null) {
			attrMap.put("error", "object acl is null!, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true, "object acl is null!", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			throw new CmisRuntimeException("object acl is null!, TraceId:" + span.getTraceId());
		}
		JSONObject jsonObject = JSONConverter.convert(objectAcl);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return jsonObject;

	}
}
