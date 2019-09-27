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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
import com.pogeyan.cmis.api.auth.IUserGroupObject;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.uri.exception.CmisRoleValidationException;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.api.utils.TracingWriter;
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
		this.registerMessageHandle("children", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getChildren((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("descendants", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getDescendants((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("folderTree", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getFolderTree((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("parent", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getParent((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("parents", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getParents((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("folder", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getFolderTree((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("checkedout", QueryGetRequest.class, (t, b) -> CompletableFuture.supplyAsync(
				() -> CmisBaseResponse.fromWithTryCatch(() -> this.getCheckedOutDocs((QueryGetRequest) t))));

		this.registerMessageHandle("getAllObjects", PostRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getAllObjects((PostRequest) t, (HashMap<String, Object>) b))));
	}

	private JSONObject getChildren(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisObjectNotFoundException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"NavigationActor::getChildren", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
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
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.CHILDREN_NULL), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.CHILDREN_NULL), span));
		}

		JSONObject jsonChildren = JSONConverter.convert(children, CmisTypeCacheService.get(request.getRepositoryId()),
				succinct, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonChildren;

	}

	private JSONArray getDescendants(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisInvalidArgumentException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"NavigationActor::getDescendants", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
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
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.DESCENDANTS_NULL), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.DESCENDANTS_NULL), span));
		}

		JSONArray jsonDescendants = new JSONArray();
		for (ObjectInFolderContainer descendant : descendants) {
			jsonDescendants.add(JSONConverter.convert(descendant, CmisTypeCacheService.get(request.getRepositoryId()),
					succinct, dateTimeFormat));
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonDescendants;

	}

	private JSONArray getFolderTree(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisInvalidArgumentException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"NavigationActor::getFolderTree", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
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
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.FOLDER_TREE_NULL), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.FOLDER_TREE_NULL), span));
		}

		JSONArray jsonDescendants = new JSONArray();
		for (ObjectInFolderContainer descendant : folderTree) {
			jsonDescendants.add(JSONConverter.convert(descendant, CmisTypeCacheService.get(request.getRepositoryId()),
					succinct, dateTimeFormat));
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonDescendants;

	}

	private JSONObject getParent(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisInvalidArgumentException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"NavigationActor::getParent", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
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
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.PARENT_NULL), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.PARENT_NULL), span));
		}

		JSONObject jsonObject = JSONConverter.convert(parent, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonObject;

	}

	private JSONArray getParents(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"NavigationActor::getParents", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
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
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.PARENTS_NULL), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.PARENTS_NULL), span));
		}
		JSONArray jsonParents = new JSONArray();
		for (ObjectParentData parent : parents) {
			jsonParents.add(JSONConverter.convert(parent, CmisTypeCacheService.get(request.getRepositoryId()), succinct,
					dateTimeFormat));
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonParents;

	}

	private JSONObject getCheckedOutDocs(QueryGetRequest request)
			throws CmisObjectNotFoundException, CmisRuntimeException, CmisRoleValidationException {
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
			throw new CmisRuntimeException("Children are null!");
		}
		JSONObject jsonDocs = JSONConverter.convert(docs, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);

		return jsonDocs;
	}

	private JSONObject getAllObjects(PostRequest request, HashMap<String, Object> baggage)
			throws IllegalArgumentException, CmisInvalidArgumentException, CmisRuntimeException,
			CmisRoleValidationException {
		{
			String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
			ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
			ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
					"NavigationActor::getAllObjects", null);

			String permission = request.getUserObject().getPermission();
			IUserGroupObject[] groupsId = request.getUserObject().getGroups();
			if (!Helpers.getGroupPermission(permission, groupsId)) {
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter
								.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
								ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRuntimeException(
						TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
			}

			boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
			DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
			List<String> objectIds = new ArrayList<>();
			if (request.getPropertyData() != null) {
				objectIds = request.getPropertyData().get("ids");
			}
			LOG.info("Method name: {}, fetching all objects, repositoryId: {}, idList: {}", "getAllObjects",
					request.getRepositoryId(), objectIds);
			if (objectIds == null) {
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(
								TracingWriter.log(String.format(ErrorMessages.INVALID_EXCEPTION), span),
								ErrorMessages.INVALID_EXCEPTION, request.getRepositoryId(), true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisInvalidArgumentException(
						TracingWriter.log(String.format(ErrorMessages.INVALID_EXCEPTION), span));
			}
			ObjectInFolderList children = CmisNavigationService.Impl.getAllObjects(request.getRepositoryId(),
					request.getUserObject(), objectIds, tracingId, span, request.getTypeId());

			if (children == null) {
				TracingApiServiceFactory.getApiService().updateSpan(span,
						TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.CHILDREN_NULL), span),
								ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
				TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
				throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.CHILDREN_NULL), span));
			}

			JSONObject jsonChildren = JSONConverter.convert(children,
					CmisTypeCacheService.get(request.getRepositoryId()), succinct, dateTimeFormat);
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
			return jsonChildren;
		}
	}
}
