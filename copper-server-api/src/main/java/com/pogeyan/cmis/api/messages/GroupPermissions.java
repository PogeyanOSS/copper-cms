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
package com.pogeyan.cmis.api.messages;

import com.pogeyan.cmis.api.auth.IUserGroupObject;

public class GroupPermissions implements IUserGroupObject {
	private String groupDN;
	private String permission;

	public GroupPermissions() {

	}

	public GroupPermissions(String groupDN, String permission) {
		super();
		this.groupDN = groupDN;
		this.permission = permission;
	}

	/**
	 * @return the groupDN
	 */
	public String getGroupDN() {
		return groupDN;
	}

	/**
	 * @param groupDN
	 *            the groupDN to set
	 */
	public void setGroupDN(String groupDN) {
		this.groupDN = groupDN;
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
}
