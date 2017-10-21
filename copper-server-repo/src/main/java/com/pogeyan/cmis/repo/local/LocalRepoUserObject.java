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

import java.util.List;
import java.util.Map;

import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.messages.GroupPermissions;

public class LocalRepoUserObject implements IUserObject {
	private Map<String, String> userDN;
	private List<GroupPermissions> groups;
	private String permission;

	public LocalRepoUserObject() {

	}

	public LocalRepoUserObject(Map<String, String> userDN, List<GroupPermissions> groups, String permission) {
		super();
		this.userDN = userDN;
		this.groups = groups;
		this.permission = permission;
	}

	/**
	 * @return the userDN
	 */
	public String getUserDN() {
		return userDN.keySet().stream().findFirst().get();
	}

	/**
	 * @param userDN
	 *            the userDN to set
	 */
	public void setUserDN(Map<String, String> userDN) {
		this.userDN = userDN;
	}

	/**
	 * @return the permission
	 */
	public String getPermission() {
		return permission;
	}

	/**
	 * @param permission
	 *            the permission to set
	 */
	public void setPermission(String permission) {
		this.permission = permission;
	}

	/**
	 * @return the groups
	 */
	public GroupPermissions[] getGroups() {
		return groups.toArray(new GroupPermissions[groups.size()]);
	}

	/**
	 * @param groups
	 *            the groups to set
	 */
	public void setGroups(List<GroupPermissions> groups) {
		this.groups = groups;
	}

	public Map<String, String> getUserNameAndPassword() {
		return this.userDN;
	}
}
