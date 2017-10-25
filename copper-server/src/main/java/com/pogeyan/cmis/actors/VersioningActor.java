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
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.DateTimeFormat;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUpdateConflictException;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.json.JSONArray;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.apache.commons.lang3.StringUtils;
import org.bson.types.ObjectId;

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.services.CmisObjectService;
import com.pogeyan.cmis.services.CmisTypeCacheService;
import com.pogeyan.cmis.services.CmisVersioningServices;

public class VersioningActor extends BaseClusterActor<BaseRequest, BaseResponse> {

	@Override
	public String getName() {
		return "versioning";
	}

	public VersioningActor() {
		this.registerMessageHandle("checkOut", PostRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.checkOut((PostRequest) t))));

		this.registerMessageHandle("cancelCheckOut", PostRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.cancelCheckOut((PostRequest) t))));

		this.registerMessageHandle("checkIn", PostRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.checkIn((PostRequest) t))));

		this.registerMessageHandle("versions", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getAllVersions((QueryGetRequest) t))));

		this.registerMessageHandle("getObjectOfLatestVersion", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getObjectOfLatestVersion((QueryGetRequest) t))));

		this.registerMessageHandle("getPropertiesOfLatestVersion", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.getPropertiesOfLatestVersion((QueryGetRequest) t))));
	}

	private JSONObject checkOut(PostRequest request) throws CmisObjectNotFoundException, CmisUpdateConflictException,
			CmisNotSupportedException, CmisInvalidArgumentException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = request.getObjectId();
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		Holder<String> objectsId = new Holder<String>(objectId.toString());
		String pwcId = CmisVersioningServices.Impl.checkOut(request.getRepositoryId(), objectsId, null, null,
				request.getUserName());
		ObjectData object = CmisObjectService.Impl.getSimpleObject(request.getRepositoryId(), pwcId,
				request.getUserName(), BaseTypeId.CMIS_DOCUMENT);
		if (object == null) {
			throw new CmisRuntimeException("New document is null!");
		}
		// return object
		JSONObject jsonObject = JSONConverter.convert(object, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);
		return jsonObject;

	}

	private JSONObject checkIn(PostRequest request) throws CmisObjectNotFoundException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		// get parameters
		String objectId = request.getObjectId();
		Boolean major = request.getBooleanParameter(QueryGetRequest.PARAM_MAJOR);
		String checkinComment = request.getParameter(QueryGetRequest.PARAM_CHECKIN_COMMENT);
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		// execute
		Holder<String> objectIdHolder = new Holder<String>(objectId);
		Map<String, List<String>> listProperties = request.getPropertyData();
		Map<String, Object> properties = new HashMap<String, Object>();
		if (listProperties != null) {
			for (Map.Entry<String, List<String>> entry : listProperties.entrySet()) {
				if (entry.getValue() == null || StringUtils.isBlank(entry.getValue().get(0))) {
					continue;
				} else {
					properties.put(entry.getKey(), entry.getValue());
				}
			}
		}
		/*
		 * if (listProperties != null) { properties =
		 * listProperties.entrySet().stream().collect(Collectors.toMap(p ->
		 * p.getKey(), p -> p.getValue() !=
		 * null||!StringUtils.isBlank(p.getValue().get(0)) ? p.getValue().get(0)
		 * :null)); }
		 */

		String versionId = CmisVersioningServices.Impl.checkIn(request.getRepositoryId(), properties,
				request.getContentStream(), objectIdHolder, major, checkinComment, null, request.getUserName());
		ObjectData object = CmisObjectService.Impl.getSimpleObject(request.getRepositoryId(), versionId,
				request.getUserName(), BaseTypeId.CMIS_DOCUMENT);
		if (object == null) {
			throw new CmisRuntimeException("New document is null!");
		}
		// return object
		JSONObject jsonObject = JSONConverter.convert(object, CmisTypeCacheService.get(request.getRepositoryId()),
				JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat);
		return jsonObject;
	}

	private JSONObject cancelCheckOut(PostRequest request)
			throws CmisUpdateConflictException, CmisUpdateConflictException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = request.getObjectId();
		boolean succinct = request.getBooleanParameter(QueryGetRequest.CONTROL_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		String docId = CmisVersioningServices.Impl.cancelCheckOut(request.getRepositoryId(), objectId, null,
				request.getUserName());
		ObjectData object = CmisObjectService.Impl.getSimpleObject(request.getRepositoryId(), docId,
				request.getUserName(), BaseTypeId.CMIS_DOCUMENT);
		if (object == null) {
			throw new CmisRuntimeException("New document is null!");
		}

		// return object
		JSONObject jsonObject = JSONConverter.convert(object, null, JSONConverter.PropertyMode.OBJECT, succinct,
				dateTimeFormat);
		return jsonObject;

	}

	private JSONArray getAllVersions(QueryGetRequest request)
			throws CmisObjectNotFoundException, CmisUpdateConflictException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		String objectId = request.getObjectId();
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		String versionSeriesId = request.getParameter(QueryGetRequest.PARAM_VERSION_SERIES_ID);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		String userName = request.getUserName();

		List<ObjectData> versions = CmisVersioningServices.Impl.getAllVersions(request.getRepositoryId(), objectId,
				versionSeriesId, filter, includeAllowableActions, null, null, userName);
		if (versions == null) {
			throw new CmisRuntimeException("version are null!");
		}
		JSONArray allVersions = new JSONArray();
		for (ObjectData version1 : versions) {
			allVersions.add(
					JSONConverter.convert(version1, null, JSONConverter.PropertyMode.OBJECT, succinct, dateTimeFormat));
		}
		return allVersions;
	}

	private JSONObject getObjectOfLatestVersion(QueryGetRequest request)
			throws CmisObjectNotFoundException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		String objectId = request.getObjectId();
		String versionSeriesId = request.getParameter(QueryGetRequest.PARAM_VERSION_SERIES_ID);
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		Boolean includeAllowableActions = request.getBooleanParameter(QueryGetRequest.PARAM_ALLOWABLE_ACTIONS);
		String renditionFilter = request.getParameter(QueryGetRequest.PARAM_RENDITION_FILTER);
		Boolean includePolicyIds = request.getBooleanParameter(QueryGetRequest.PARAM_POLICY_IDS);
		Boolean includeAcl = request.getBooleanParameter(QueryGetRequest.PARAM_ACL);
		String userName = request.getUserName();

		ObjectData object = CmisVersioningServices.Impl.getObjectOfLatestVersion(request.getRepositoryId(), objectId,
				versionSeriesId, true, filter, includeAllowableActions, null, includePolicyIds,
				includeAcl, null, null, userName);

		if (object == null) {
			throw new CmisRuntimeException("object is not present!");
		}
		JSONObject jsonObject = JSONConverter.convert(object, null, JSONConverter.PropertyMode.OBJECT, succinct,
				dateTimeFormat);
		return jsonObject;

	}

	private JSONObject getPropertiesOfLatestVersion(QueryGetRequest request)
			throws CmisObjectNotFoundException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		boolean succinct = request.getBooleanParameter(QueryGetRequest.PARAM_SUCCINCT, false);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		String objectId = request.getObjectId();
		String versionSeriesId = request.getParameter(QueryGetRequest.PARAM_VERSION_SERIES_ID);
		String filter = request.getParameter(QueryGetRequest.PARAM_FILTER);
		Boolean major = request.getBooleanParameter(QueryGetRequest.PARAM_MAJOR);
		String userName = request.getUserName();

		Properties properties = CmisVersioningServices.Impl.getPropertiesOfLatestVersion(request.getRepositoryId(),
				objectId, versionSeriesId, major, filter, null, userName);

		if (properties == null) {
			throw new CmisRuntimeException("properties is not present!");
		}
		JSONObject jsonObject = JSONConverter.convert(properties, objectId, null, JSONConverter.PropertyMode.OBJECT,
				succinct, dateTimeFormat);
		return jsonObject;
	}
}