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
package com.pogeyan.cmis.ldap.auth;

import java.util.Map;

import com.pogeyan.cmis.api.auth.IAuthStoreSettings;

public class LDAPAuthStoreSettings implements IAuthStoreSettings {
	private String serverName;
	private int port;
	private String companyName;
	private String adminUser;
	private String mastercompany;
	private String userIdAttribute;

	@Override
	public void setStoreSetting(Map<String, String> dbSettings) {
		this.serverName = dbSettings.get("serverName");
		this.companyName = dbSettings.get("companyName");
		this.port = Integer.parseInt(dbSettings.get("port"));
		this.adminUser = dbSettings.get("adminUser");
		this.mastercompany = dbSettings.get("masterCompany");
		this.userIdAttribute = dbSettings.get("userIdAttribute");
	}

	@Override
	public String getType() {
		return "ldap";
	}

	public String getServerName() {
		return serverName;
	}

	public int getPort() {
		return port;
	}

	public String getCompanyName() {
		return companyName;
	}

	public String getAdminUser() {
		return this.adminUser;
	}

	public String getMastercompany() {
		return mastercompany;
	}

	public void setMastercompany(String mastercompany) {
		this.mastercompany = mastercompany;
	}

	public String getUserIdAttribute() {
		return userIdAttribute;
	}

	public void setUserIdAttribute(String userIdAttribute) {
		this.userIdAttribute = userIdAttribute;
	}
}
