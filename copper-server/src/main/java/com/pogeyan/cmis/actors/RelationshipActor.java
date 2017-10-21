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
import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.enums.DateTimeFormat;
import org.apache.chemistry.opencmis.commons.enums.RelationshipDirection;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.bson.types.ObjectId;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.services.CmisRelationshipService;

public class RelationshipActor extends BaseClusterActor<BaseRequest, BaseResponse> {

	@Override
	public String getName() {
		return "relationships";
	}

	public RelationshipActor() {
		this.registerMessageHandle("relationships", QueryGetRequest.class, (t, b) -> CompletableFuture.supplyAsync(
				() -> CmisBaseResponse.fromWithTryCatch(() -> this.getRelationships((QueryGetRequest) t))));

	}

	private JSONObject getRelationships(QueryGetRequest request)
			throws CmisObjectNotFoundException, MongoException, CmisRuntimeException {
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(request.getUserName() + " is not authorized to applyAcl.");
		}
		ObjectId objectId = new ObjectId(request.getObjectId());
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

		ObjectList relationships = CmisRelationshipService.Impl.getObjectRelationships(request.getRepositoryId(),
				objectId, includeSubRelationshipTypes, relationshipDirection, typeId, renditionFilter,
				includeAllowableActions, maxItems, skipCount, null, request.getUserName());

		if (relationships == null) {
			throw new CmisRuntimeException("Relationships are null!");
		}

		JSONObject jsonChildren = JSONConverter.convert(relationships, null, JSONConverter.PropertyMode.OBJECT,
				succinct, dateTimeFormat);
		return jsonChildren;

	}
}
