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

import java.math.BigInteger;
import java.util.HashMap;
import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.enums.DateTimeFormat;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.JSONConstants;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.api.utils.TracingWriter;
import com.pogeyan.cmis.browser.BrowserConstants;
import com.pogeyan.cmis.impl.services.CmisDiscoveryService;
import com.pogeyan.cmis.impl.services.CmisTypeCacheService;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class DiscoveryActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryActor.class);

	@Override
	public String getName() {
		return "discovery";
	}

	public DiscoveryActor() {
		this.registerMessageHandle("contentChanges", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getContentChanges((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("query", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getQuery((QueryGetRequest) t))));
	}

	private JSONObject getContentChanges(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"DiscoveryActor::getContentChanges", null);
		String permission = request.getUserObject().getPermission();

		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()),
									span.getTraceId()),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter
					.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span.getTraceId()));
		}
		String changeLogToken = request.getParameter(QueryGetRequest.PARAM_CHANGE_LOG_TOKEN);
		Boolean includeProperties = request.getBooleanParameter(QueryGetRequest.PARAM_PROPERTIES);
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		// Set orderby value
		String orderBy = "cmis:lastModificationDate asc";
		Boolean includePolicyIds = request.getBooleanParameter(QueryGetRequest.PARAM_POLICY_IDS);
		Boolean includeAcl = request.getBooleanParameter(QueryGetRequest.PARAM_ACL);
		BigInteger maxItems = request.getBigIntegerParameter(QueryGetRequest.PARAM_MAX_ITEMS);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		Holder<String> changeLogTokenHolder = new Holder<String>(changeLogToken);
		LOG.info(
				"Method name: {}, get latest content changes using this id: {}, repositoryId: {}, includeAcl: {}, includePolicyIds: {}",
				"getContentChanges", changeLogTokenHolder, request.getRepositoryId(), includeAcl, includePolicyIds);
		ObjectList changes = CmisDiscoveryService.Impl.getContentChanges(request.getRepositoryId(),
				changeLogTokenHolder, includeProperties, filter, orderBy, includePolicyIds, includeAcl, maxItems, null,
				request.getUserObject(), tracingId, span);

		JSONObject jsonChanges = JSONConverter.convert(changes, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.CHANGE, succinct, dateTimeFormat);
		jsonChanges.put(JSONConstants.JSON_OBJECTLIST_CHANGE_LOG_TOKEN, changeLogTokenHolder.getValue());
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonChanges;
	}

	private JSONObject getQuery(QueryGetRequest t) throws CmisRuntimeException {
		return null;
	}

}
