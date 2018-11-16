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
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectInFolderContainer;
import org.apache.chemistry.opencmis.commons.data.ObjectInFolderList;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.data.ObjectParentData;
import org.apache.chemistry.opencmis.commons.enums.DateTimeFormat;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
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
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.browser.BrowserConstants;
import com.pogeyan.cmis.impl.services.CmisNavigationService;
import com.pogeyan.cmis.impl.services.CmisTypeCacheService;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class NavigationActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(NavigationActor.class);

	@Override
	public String getName() {
		return "navigation";
	}

	public NavigationActor() {
		this.registerMessageHandle("children", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getChildren((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("descendants", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getDescendants((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("folderTree", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getFolderTree((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("parent", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getParent((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("parents", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getParents((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("folder", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getFolderTree((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("checkedout", QueryGetRequest.class, (t, b) -> CompletableFuture.supplyAsync(
				() -> CmisBaseResponse.fromWithTryCatch(() -> this.getCheckedOutDocs((QueryGetRequest) t))));
	}

	private JSONObject getChildren(QueryGetRequest request, HashMap<String, Object> baggage) throws CmisObjectNotFoundException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "NavigationActor::getChildren",
				null);
		Map<String, Object> attrMap = new HashMap<String, Object>();
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			attrMap.put("error", request.getUserName() + "is not authorized to applyAcl, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true,
					request.getUserName() + " is not authorized to applyAcl", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl."+ " TraceId:", span.getTraceId());
		}
		String typeId = request.getParameter("typeId");
		String folderId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		String orderBy = request.getParameter(QueryGetRequest.PARAM_ORDER_BY);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		includeAllowableActions = includeAllowableActions == null ? false : includeAllowableActions;
		IncludeRelationships includeRelationships = request.getEnumParameter(QueryGetRequest.PARAM_RELATIONSHIPS,
				IncludeRelationships.class);
		String renditionFilter = request.getParameter(QueryGetRequest.PARAM_RENDITION_FILTER);
		Boolean includePathSegment = request.getBooleanParameter(QueryGetRequest.PARAM_PATH_SEGMENT);
		BigInteger maxItems = request.getBigIntegerParameter(QueryGetRequest.PARAM_MAX_ITEMS);
		BigInteger skipCount = request.getBigIntegerParameter(QueryGetRequest.PARAM_SKIP_COUNT);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info(
				"Method name: {}, get the first level of  child elements for folder object using this id: {}, repositoryId: {}, includeRelationships: {}, includePathSegment: {}, query filter: {}",
				"getChildren", folderId, request.getRepositoryId(), includeRelationships, includePathSegment, filter);
		ObjectInFolderList children = CmisNavigationService.Impl.getChildren(request.getRepositoryId(), folderId,
				filter, orderBy, includeAllowableActions, includeRelationships, renditionFilter, includePathSegment,
				maxItems, skipCount, null, request.getUserObject(), typeId, tracingId, span);
		if (children == null) {
			attrMap.put("error", "Children are null!, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true, "Children are null!", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException("Children are null!, TraceId:", span.getTraceId());
		}

		JSONObject jsonChildren = JSONConverter.convert(children, CmisTypeCacheService.get(request.getRepositoryId()),
				succinct, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return jsonChildren;

	}

	private JSONArray getDescendants(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisInvalidArgumentException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "NavigationActor::getDescendants",
				null);
		Map<String, Object> attrMap = new HashMap<String, Object>();
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			attrMap.put("error", request.getUserName() + "is not authorized to applyAcl, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true,
					request.getUserName() + " is not authorized to applyAcl", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl."+ " TraceId:", span.getTraceId());
		}
		String folderId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		BigInteger depth = request.getBigIntegerParameter(QueryGetRequest.PARAM_DEPTH);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		includeAllowableActions = includeAllowableActions == null ? false : includeAllowableActions;
		IncludeRelationships includeRelationships = request.getEnumParameter(QueryGetRequest.PARAM_RELATIONSHIPS,
				IncludeRelationships.class);
		String renditionFilter = request.getParameter(QueryGetRequest.PARAM_RENDITION_FILTER);
		Boolean includePathSegment = request.getBooleanParameter(QueryGetRequest.PARAM_PATH_SEGMENT);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info(
				"Method name: {}, getting all child elements for folder object using this id: {}, repositoryId: {}, includeRelationships: {}, includePathSegment: {}, query filter: {}",
				"getDescendants", folderId, request.getRepositoryId(), includeRelationships, includePathSegment,
				filter);
		List<ObjectInFolderContainer> descendants = CmisNavigationService.Impl.getDescendants(request.getRepositoryId(),
				folderId, depth, filter, includeAllowableActions, includeRelationships, renditionFilter,
				includePathSegment, null, request.getUserObject(), request.getTypeId(), tracingId, span);

		if (descendants == null) {
			attrMap.put("error", "Descendants are null!, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true, "Descendants are null!", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException("Descendants are null!, TraceId:" + span.getTraceId());
		}

		JSONArray jsonDescendants = new JSONArray();
		for (ObjectInFolderContainer descendant : descendants) {
			jsonDescendants.add(JSONConverter.convert(descendant, CmisTypeCacheService.get(request.getRepositoryId()),
					succinct, dateTimeFormat));
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return jsonDescendants;

	}

	private JSONArray getFolderTree(QueryGetRequest request, HashMap<String, Object> baggage) throws CmisInvalidArgumentException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "NavigationActor::getFolderTree",
				null);
		Map<String, Object> attrMap = new HashMap<String, Object>();
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			attrMap.put("error", request.getUserName() + "is not authorized to applyAcl, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true,
					request.getUserName() + " is not authorized to applyAcl", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl."+ " TraceId:", span.getTraceId());
		}
		String folderId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		BigInteger depth = request.getBigIntegerParameter(QueryGetRequest.PARAM_DEPTH);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		IncludeRelationships includeRelationships = request.getEnumParameter(QueryGetRequest.PARAM_RELATIONSHIPS,
				IncludeRelationships.class);
		String renditionFilter = request.getParameter(QueryGetRequest.PARAM_RENDITION_FILTER);
		Boolean includePathSegment = request.getBooleanParameter(QueryGetRequest.PARAM_PATH_SEGMENT);

		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();

		LOG.info(
				"Method name: {}, getFolderTree using this id: {}, repositoryId: {}, includeRelationships: {}, includePathSegment: {}, query filter: {}",
				"getFolderTree", folderId, request.getRepositoryId(), includeRelationships, includePathSegment, filter);
		List<ObjectInFolderContainer> folderTree = CmisNavigationService.Impl.getFolderTree(request.getRepositoryId(),
				folderId, depth, filter, includeAllowableActions, includeRelationships, renditionFilter,
				includePathSegment, null, request.getUserObject(), request.getTypeId(), tracingId, span);
		if (folderTree == null) {
			attrMap.put("error", "Folder Tree are null!, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true, "Folder Tree are null!", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException("Folder Tree are null!, TraceId:"+ span.getTraceId());
		}

		JSONArray jsonDescendants = new JSONArray();
		for (ObjectInFolderContainer descendant : folderTree) {
			jsonDescendants.add(JSONConverter.convert(descendant, CmisTypeCacheService.get(request.getRepositoryId()),
					succinct, dateTimeFormat));
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return jsonDescendants;

	}

	private JSONObject getParent(QueryGetRequest request, HashMap<String, Object> baggage) throws CmisInvalidArgumentException, CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "NavigationActor::getParent",
				null);
		Map<String, Object> attrMap = new HashMap<String, Object>();
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			attrMap.put("error", request.getUserName() + "is not authorized to applyAcl, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true,
					request.getUserName() + " is not authorized to applyAcl", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl."+ " TraceId:", span.getTraceId());
		}
		String objectId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, getting first level of parent using this id: {}, repositoryId: {}, query filter: {}",
				"getFolderParent", objectId, request.getRepositoryId(), filter);
		ObjectData parent = CmisNavigationService.Impl.getFolderParent(request.getRepositoryId(), objectId, filter,
				null, request.getUserObject(), request.getTypeId(), tracingId, span);

		if (parent == null) {
			attrMap.put("error", "Parent is null!, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true, "Parent is null!", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException("Parent is null!, TraceId:", span.getTraceId());
		}

		JSONObject jsonObject = JSONConverter.convert(parent, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return jsonObject;

	}

	private JSONArray getParents(QueryGetRequest request, HashMap<String, Object> baggage) throws CmisRuntimeException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan, "NavigationActor::getParents",
				null);
		Map<String, Object> attrMap = new HashMap<String, Object>();
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			attrMap.put("error", request.getUserName() + "is not authorized to applyAcl, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true,
					request.getUserName() + " is not authorized to applyAcl", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl."+ " TraceId:", span.getTraceId());
		}
		String objectId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		IncludeRelationships includeRelationships = request.getEnumParameter(QueryGetRequest.PARAM_RELATIONSHIPS,
				IncludeRelationships.class);
		String renditionFilter = request.getParameter(QueryGetRequest.PARAM_RENDITION_FILTER);

		Boolean includeRelativePathSegment = request.getBooleanParameter(QueryGetRequest.PARAM_RELATIVE_PATH_SEGMENT);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info(
				"Method name: {}, getting all parent objects using this id: {}, repositoryId: {}, includeAllowableActions: {}, includeRelationships: {}, query filter: {}",
				"getObjectParents", objectId, request.getRepositoryId(), includeAllowableActions, includeRelationships,
				filter);
		List<ObjectParentData> parents = CmisNavigationService.Impl.getObjectParents(request.getRepositoryId(),
				objectId, filter, includeAllowableActions, includeRelationships, renditionFilter,
				includeRelativePathSegment, null, request.getUserObject(), request.getTypeId(), tracingId, span);

		if (parents == null) {
			attrMap.put("error", "Parents is null!, TraceId:" + span.getTraceId());
			TracingApiServiceFactory.getApiService().updateSpan(span, true, "Parents is null!", attrMap);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, parentSpan);
			throw new CmisRuntimeException("Parents is null!, TraceId:"+ span.getTraceId());
		}
		JSONArray jsonParents = new JSONArray();
		for (ObjectParentData parent : parents) {
			jsonParents.add(JSONConverter.convert(parent, CmisTypeCacheService.get(request.getRepositoryId()), succinct,
					dateTimeFormat));
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span);
		return jsonParents;

	}

	private JSONObject getCheckedOutDocs(QueryGetRequest request)
			throws CmisObjectNotFoundException, CmisRuntimeException {
		Map<String, Object> attrMap = new HashMap<String, Object>();
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		String folderId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		String orderBy = request.getParameter(QueryGetRequest.PARAM_ORDER_BY);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		includeAllowableActions = includeAllowableActions == null ? false : includeAllowableActions;
		IncludeRelationships includeRelationships = request.getEnumParameter(QueryGetRequest.PARAM_RELATIONSHIPS,
				IncludeRelationships.class);
		String renditionFilter = request.getParameter(QueryGetRequest.PARAM_RENDITION_FILTER);
		BigInteger maxItems = request.getBigIntegerParameter(QueryGetRequest.PARAM_MAX_ITEMS);
		BigInteger skipCount = request.getBigIntegerParameter(QueryGetRequest.PARAM_SKIP_COUNT);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info(
				"Method name: {}, getting all checkout documents objects using this folder id: {}, repositoryId: {}, includeAllowableActions: {}, includeRelationships: {}, query filter: {}",
				"getCheckedOutDocs", folderId, request.getRepositoryId(), includeAllowableActions, includeRelationships,
				filter);
		ObjectList docs = CmisNavigationService.Impl.getCheckedOutDocs(request.getRepositoryId(), folderId, filter,
				orderBy, includeAllowableActions, includeRelationships, renditionFilter, maxItems, skipCount, null,
				null, request.getUserObject(), request.getTypeId());

		if (docs == null) {
			throw new CmisRuntimeException("docs is null!, TraceId:");
		}
		JSONObject jsonDocs = JSONConverter.convert(docs, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);
		return jsonDocs;
	}

}
