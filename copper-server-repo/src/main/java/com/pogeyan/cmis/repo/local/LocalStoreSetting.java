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
package com.pogeyan.cmis.repo.local;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.auth.IAuthStoreSettings;
import com.pogeyan.cmis.api.messages.GroupPermissions;

public class LocalStoreSetting implements IAuthStoreSettings {
	private static final Logger LOG = LoggerFactory.getLogger(LocalStoreSetting.class);
	private Map<String, LocalRepoUserObject> userObject = new HashMap<>();

	@Override
	public String getType() {
		return "local";
	}

	@Override
	public void setStoreSetting(Map<String, String> loginSetting) {
		try {
			JSONArray loginDetails = new JSONArray(loginSetting.get("users"));
			for (Object loginObject : loginDetails) {
				JSONObject jsonLoginObject = (JSONObject) loginObject;
				String userDetails = (String) jsonLoginObject.get("userDetails");
				Map<String, String> user = new HashMap<>();
				String[] userNamePassword = userDetails.split(":");
				user.put(userNamePassword[0], userNamePassword[1]);
				String permission = (String) jsonLoginObject.get("permission");
				JSONArray groupsArray = (JSONArray) jsonLoginObject.get("groups");
				List<GroupPermissions> groupPermissions = new ArrayList<>();
				for (Object groupObject : groupsArray) {
					JSONObject jsonGropupObject = (JSONObject) groupObject;
					String groupDN = (String) jsonGropupObject.get("groupName");
					String groupPermission = (String) jsonGropupObject.get("permission");
					GroupPermissions groupDetails = new GroupPermissions(groupDN, groupPermission);
					groupPermissions.add(groupDetails);
				}
				LocalRepoUserObject userDetailsList = new LocalRepoUserObject(user, groupPermissions, permission);
				this.userObject.put(userNamePassword[0], userDetailsList);

			}
		} catch (Exception e) {
			LOG.error("reading json file exception: {}", ExceptionUtils.getStackTrace(e));
		}
	}

	public Map<String, LocalRepoUserObject> getUserObject() {
		return userObject;
	}

	public void setUserObject(Map<String, LocalRepoUserObject> userObject) {
		this.userObject = userObject;
	}

}
