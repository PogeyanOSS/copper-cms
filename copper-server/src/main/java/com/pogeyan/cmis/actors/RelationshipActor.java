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
import org.apache.chemistry.opencmis.commons.enums.RelationshipDirection;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
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
import com.pogeyan.cmis.impl.services.CmisRelationshipService;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class RelationshipActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(RelationshipActor.class);

	@Override
	public String getName() {
		return "relationships";
	}

	public RelationshipActor() {
		this.registerMessageHandle("relationships", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getRelationships((QueryGetRequest) t, (HashMap<String, Object>) b))));

	}

	private JSONObject getRelationships(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisObjectNotFoundException, MongoException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RelationshipActor::getRelationships", null);
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
		String objectId = request.getObjectId();
		Boolean includeSubRelationshipTypes = request.getBooleanParameter(QueryGetRequest.PARAM_SUB_RELATIONSHIP_TYPES);
		RelationshipDirection relationshipDirection = request
				.getEnumParameter(QueryGetRequest.PARAM_RELATIONSHIP_DIRECTION, RelationshipDirection.class);
		String renditionFilter = request.getParameter(QueryGetRequest.PARAM_RENDITION_FILTER);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		BigInteger maxItems = request.getBigIntegerParameter(QueryGetRequest.PARAM_MAX_ITEMS);
		BigInteger skipCount = request.getBigIntegerParameter(QueryGetRequest.PARAM_SKIP_COUNT);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		String typeId = request.getParameter(QueryGetRequest.PARAM_TYPE_ID);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info(
				"Method name: {}, get relationship object using this id: {}, repositoryId: {}, includeAllowableActions: {}, relationshipDirection: {}",
				"getObjectRelationships", objectId, request.getRepositoryId(), includeAllowableActions,
				relationshipDirection);

		ObjectList relationships = CmisRelationshipService.Impl.getObjectRelationships(request.getRepositoryId(),
				objectId, includeSubRelationshipTypes, relationshipDirection, typeId, renditionFilter,
				includeAllowableActions, maxItems, skipCount, null, request.getUserObject(), tracingId, span);

		if (relationships == null) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.RELATIONSHIP_NULL), span.getTraceId()),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.RELATIONSHIP_NULL), span.getTraceId()));
		}

		JSONObject jsonChildren = JSONConverter.convert(relationships, null, JSONConverter.PropertyMode.OBJECT,
				succinct, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonChildren;

	}
}
