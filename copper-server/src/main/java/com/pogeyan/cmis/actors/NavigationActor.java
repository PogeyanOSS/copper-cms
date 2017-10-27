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

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.impl.services.CmisNavigationService;
import com.pogeyan.cmis.impl.services.CmisTypeCacheService;

public class NavigationActor extends BaseClusterActor<BaseRequest, BaseResponse> {

	@Override
	public String getName() {
		return "navigation";
	}

	public NavigationActor() {
		this.registerMessageHandle("children", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getChildren((QueryGetRequest) t))));

		this.registerMessageHandle("descendants", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getDescendants((QueryGetRequest) t))));

		this.registerMessageHandle("folderTree", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getFolderTree((QueryGetRequest) t))));

		this.registerMessageHandle("parent", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getParent((QueryGetRequest) t))));

		this.registerMessageHandle("parents", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getParents((QueryGetRequest) t))));

		this.registerMessageHandle("folder", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getFolderTree((QueryGetRequest) t))));

		this.registerMessageHandle("checkedout", QueryGetRequest.class, (t, b) -> CompletableFuture.supplyAsync(
				() -> CmisBaseResponse.fromWithTryCatch(() -> this.getCheckedOutDocs((QueryGetRequest) t))));
	}

	private JSONObject getChildren(QueryGetRequest request) throws CmisObjectNotFoundException, CmisRuntimeException {
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
		Boolean includePathSegment = request.getBooleanParameter(QueryGetRequest.PARAM_PATH_SEGMENT);
		BigInteger maxItems = request.getBigIntegerParameter(QueryGetRequest.PARAM_MAX_ITEMS);
		BigInteger skipCount = request.getBigIntegerParameter(QueryGetRequest.PARAM_SKIP_COUNT);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();

		ObjectInFolderList children = CmisNavigationService.Impl.getChildren(request.getRepositoryId(), folderId,
				filter, orderBy, includeAllowableActions, includeRelationships, renditionFilter, includePathSegment,
				maxItems, skipCount, null, request.getUserObject());

		if (children == null) {
			throw new CmisRuntimeException("Children are null!");
		}

		JSONObject jsonChildren = JSONConverter.convert(children, CmisTypeCacheService.get(request.getRepositoryId()),
				succinct, dateTimeFormat);

		return jsonChildren;

	}

	private JSONArray getDescendants(QueryGetRequest request)
			throws CmisInvalidArgumentException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
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
		List<ObjectInFolderContainer> descendants = CmisNavigationService.Impl.getDescendants(request.getRepositoryId(),
				folderId, depth, filter, includeAllowableActions, includeRelationships, renditionFilter,
				includePathSegment, null, request.getUserObject());

		if (descendants == null) {
			throw new CmisRuntimeException("Descendants are null!");
		}

		JSONArray jsonDescendants = new JSONArray();
		for (ObjectInFolderContainer descendant : descendants) {
			jsonDescendants.add(JSONConverter.convert(descendant, CmisTypeCacheService.get(request.getRepositoryId()),
					succinct, dateTimeFormat));
		}
		return jsonDescendants;

	}

	private JSONArray getFolderTree(QueryGetRequest request) throws CmisInvalidArgumentException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
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

		List<ObjectInFolderContainer> folderTree = CmisNavigationService.Impl.getFolderTree(request.getRepositoryId(),
				folderId, depth, filter, includeAllowableActions, includeRelationships, renditionFilter,
				includePathSegment, null, request.getUserObject());

		if (folderTree == null) {
			throw new CmisRuntimeException("Folder Tree are null!");
		}

		JSONArray jsonDescendants = new JSONArray();
		for (ObjectInFolderContainer descendant : folderTree) {
			jsonDescendants.add(JSONConverter.convert(descendant, CmisTypeCacheService.get(request.getRepositoryId()),
					succinct, dateTimeFormat));
		}
		return jsonDescendants;

	}

	private JSONObject getParent(QueryGetRequest request) throws CmisInvalidArgumentException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();

		ObjectData parent = CmisNavigationService.Impl.getFolderParent(request.getRepositoryId(), objectId, filter,
				null, request.getUserObject().getUserDN());

		if (parent == null) {
			throw new CmisRuntimeException("Parent is null!");
		}

		JSONObject jsonObject = JSONConverter.convert(parent, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);
		return jsonObject;

	}

	private JSONArray getParents(QueryGetRequest request) throws CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
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

		List<ObjectParentData> parents = CmisNavigationService.Impl.getObjectParents(request.getRepositoryId(),
				objectId, filter, includeAllowableActions, includeRelationships, renditionFilter,
				includeRelativePathSegment, null, request.getUserObject().getUserDN());

		if (parents == null) {
			throw new CmisRuntimeException("Parents are null!");
		}
		JSONArray jsonParents = new JSONArray();
		for (ObjectParentData parent : parents) {
			jsonParents.add(JSONConverter.convert(parent, CmisTypeCacheService.get(request.getRepositoryId()), succinct,
					dateTimeFormat));
		}
		return jsonParents;

	}

	private JSONObject getCheckedOutDocs(QueryGetRequest request)
			throws CmisObjectNotFoundException, CmisRuntimeException {
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

		ObjectList docs = CmisNavigationService.Impl.getCheckedOutDocs(request.getRepositoryId(), folderId, filter,
				orderBy, includeAllowableActions, includeRelationships, renditionFilter, maxItems, skipCount, null,
				null, request.getUserObject());

		if (docs == null) {
			throw new CmisRuntimeException("Children are null!");
		}
		JSONObject jsonDocs = JSONConverter.convert(docs, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);

		return jsonDocs;
	}

}
