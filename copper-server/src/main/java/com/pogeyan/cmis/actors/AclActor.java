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

import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.CapabilityAcl;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.services.CmisAclServices;

public class AclActor extends BaseClusterActor<BaseRequest, BaseResponse> {

	@Override
	public String getName() {
		return "acl";
	}

	public AclActor() {
		this.registerMessageHandle("acl", QueryGetRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.getAcl((QueryGetRequest) t))));

		this.registerMessageHandle("applyACL", PostRequest.class, (t, b) -> CompletableFuture
				.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(() -> this.applyACL((PostRequest) t))));
	}

	private JSONObject applyACL(PostRequest t) throws CmisObjectNotFoundException, CmisRuntimeException {
		String permission = t.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			throw new CmisRuntimeException(t.getUserName() + " is not authorized to applyAcl.");
		}
		String aclPro = t.getAclPropagation();
		String objectId = t.getObjectId();
		Acl objectAcl = CmisAclServices.Impl.applyAcl(t.getRepositoryId(), objectId, t.getAddAcl(), t.getRemoveAcl(),
				AclPropagation.fromValue(aclPro), null, null, CapabilityAcl.NONE, t.getUserObject().getUserDN());
		if (objectAcl == null) {
			throw new CmisRuntimeException("object acl is null!");
		}
		JSONObject jsonObject = JSONConverter.convert(objectAcl);
		return jsonObject;

	}

	private JSONObject getAcl(QueryGetRequest t) throws CmisObjectNotFoundException, CmisRuntimeException {
		String permission = t.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			throw new CmisRuntimeException(t.getUserName() + " is not authorized to applyAcl.");
		}
		String objectId = t.getObjectId();
		Boolean onlyBasicPermissions = t.getBooleanParameter(QueryGetRequest.PARAM_ONLY_BASIC_PERMISSIONS);
		Acl objectAcl = CmisAclServices.Impl.getAcl(t.getRepositoryId(), objectId, onlyBasicPermissions, null, null,
				t.getUserObject().getUserDN());
		if (objectAcl == null) {
			throw new CmisRuntimeException("object acl is null!");
		}
		JSONObject jsonObject = JSONConverter.convert(objectAcl);
		return jsonObject;

	}
}
