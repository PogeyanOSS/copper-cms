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

/**
 * The Class LdapProperties is pojo class which get and set all the properties
 * for particular user.
 */
public class LdapProperties {

	/** The server name. */
	private String serverName;

	/** The port. */
	private int port;

	/** The user name. */
	private String userName;

	/** The password. */
	private String password;
	/** The base dn. */
	private String baseDN;

	/** The use tls. */
	private boolean useTLS = false;

	/** The user id attribute. */
	private String userIdAttribute;

	/** The search filter. */
	private String searchFilter;

	/** The port which supports TLS. */
	private String portTLS;

	/** The port which supports TLS. */
	private String masterCompany;

	/**
	 * Instantiates a new ldap properties.
	 */
	public LdapProperties() {
		setUseTLS(false);
	}

	/**
	 * Sets the base dn.
	 *
	 * @param baseDN
	 *            the new base dn
	 */
	public void setBaseDN(final String baseDN) {
		this.baseDN = baseDN;
	}

	/**
	 * Sets the password.
	 *
	 * @param password
	 *            the new password
	 */
	public void setPassword(final String password) {
		this.password = password;
	}

	/**
	 * Sets the port.
	 *
	 * @param port
	 *            the new port
	 */
	public void setPort(final int port) {
		this.port = port;
	}

	/**
	 * Sets the server name.
	 *
	 * @param serverName
	 *            the new server name
	 */
	public void setServerName(final String serverName) {
		this.serverName = serverName;
	}

	/**
	 * Sets the user name.
	 *
	 * @param userName
	 *            the new user name
	 */
	public void setUserName(final String userName) {
		this.userName = userName;
	}

	/**
	 * Sets the use tls.
	 *
	 * @param useTLS
	 *            the new use tls
	 */
	public void setUseTLS(final boolean useTLS) {
		this.useTLS = useTLS;
	}

	/**
	 * Sets the search filter.
	 *
	 * @param searchFilter
	 *            the new search filter
	 */
	public void setSearchFilter(final String searchFilter) {
		this.searchFilter = searchFilter;
	}

	/**
	 * Gets the base dn.
	 *
	 * @return the base dn
	 */
	public final String getBaseDN() {
		return baseDN;
	}

	/**
	 * Gets the password.
	 *
	 * @return the password
	 */
	public final String getPassword() {
		return password;
	}

	/**
	 * Gets the port.
	 *
	 * @return the port
	 */
	public final int getPort() {
		return port;
	}

	/**
	 * Gets the user name.
	 *
	 * @return the user name
	 */
	public final String getUserName() {
		return userName;
	}

	/**
	 * Gets the server name.
	 *
	 * @return the server name
	 */
	public final String getServerName() {
		return serverName;
	}

	/**
	 * Checks if is use tls.
	 *
	 * @return true, if is use tls
	 */
	public final boolean isUseTLS() {
		return useTLS;
	}

	/**
	 * Gets the user id attribute.
	 *
	 * @return the user id attribute
	 */
	public final String getUserIdAttribute() {
		return userIdAttribute;
	}

	/**
	 * Gets the search filter.
	 *
	 * @return the search filter
	 */
	public final String getSearchFilter() {
		return searchFilter;
	}

	/**
	 * @return the portTLS
	 */
	public String getPortTLS() {
		return portTLS;
	}

	/**
	 * @param portTLS
	 *            the portTLS to set
	 */
	public void setPortTLS(String portTLS) {
		this.portTLS = portTLS;
	}

	public String getMasterCompany() {
		return masterCompany;
	}

	public void setMasterCompany(String masterCompany) {
		this.masterCompany = masterCompany;
	}
}
