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
package com.pogeyan.cmis.ldap.model;

public class LoginProperties {

	/** The user name. */
	private String userName;

	/** The password. */
	private String password;

	private String organizationalUnit = "People";

	/** The server name. */
	private String serverName;

	/** The server port. */
	private int port;

	/** The company Name */
	private String companyName;

	/** The admin user name. */
	private String adminUser;

	/** The master company. */
	private String masterCompany;

	/** The UserIdAttribute. */
	private String UserIdAttribute;

	/**
	 * @return the userName
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @param userName
	 *            the userName to set
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @param password
	 *            the password to set
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 * @return the organizationalUnit
	 */
	public String getOrganizationalUnit() {
		return organizationalUnit;
	}

	/**
	 * @param organizationalUnit
	 *            the organizationalUnit to set
	 */
	public void setOrganizationalUnit(String organizationalUnit) {
		this.organizationalUnit = organizationalUnit;
	}

	/**
	 * @return the companyName
	 */
	public String getCompanyName() {
		return companyName;
	}

	/**
	 * @param companyName
	 *            the companyName to set
	 */
	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	/**
	 * @return the serverName
	 */
	public String getServerName() {
		return serverName;
	}

	/**
	 * @param serverName
	 *            the serverName to set
	 */
	public void setServerName(String serverName) {
		this.serverName = serverName;
	}

	/**
	 * @return the port
	 */
	public int getPort() {
		return port;
	}

	/**
	 * @param port
	 *            the port to set
	 */
	public void setPort(int port) {
		this.port = port;
	}

	/**
	 * @return the adminUserName
	 */
	public String getAdminUser() {
		return this.adminUser;
	}

	/**
	 * @param adminUserName
	 *            the adminUserName to set
	 */
	public void setAdminUser(String adminUser) {
		this.adminUser = adminUser;
	}

	public String getMasterCompany() {
		return masterCompany;
	}

	public void setMasterCompany(String masterCompany) {
		this.masterCompany = masterCompany;
	}

	public String getUserIdAttribute() {
		return UserIdAttribute;
	}

	public void setUserIdAttribute(String userIdAttribute) {
		UserIdAttribute = userIdAttribute;
	}
}
