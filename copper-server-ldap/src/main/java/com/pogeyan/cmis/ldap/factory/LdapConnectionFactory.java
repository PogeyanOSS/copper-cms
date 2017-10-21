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
package com.pogeyan.cmis.ldap.factory;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.net.ssl.SSLContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPConnectionPool;
import com.unboundid.ldap.sdk.LDAPException;
import com.unboundid.ldap.sdk.StartTLSPostConnectProcessor;
import com.unboundid.ldap.sdk.extensions.StartTLSExtendedRequest;
import com.unboundid.util.ssl.SSLUtil;
import com.unboundid.util.ssl.TrustStoreTrustManager;

public class LdapConnectionFactory {
	private final String server;
	private final int port;
	private final String userDN;
	private final String password;

	public static final String filename = "jssecacerts";
	private static Logger logger = LoggerFactory.getLogger(LdapConnectionFactory.class);

	public LdapConnectionFactory(String server, int port, String userDN, String password) {
		this.server = server;
		this.port = port;
		this.userDN = userDN;
		this.password = password;
	}

	public LDAPConnection getLDAPConnection() throws LDAPException {
		return getLDAPConnection(userDN, password);
	}

	public LDAPConnection getLDAPConnection(int portTLS) throws LDAPException {
		return getLDAPConnection(userDN, password, portTLS);
	}

	public LDAPConnection getLDAPConnection(String userDN, String password) throws LDAPException {
		LDAPConnection ldapConnection = new LDAPConnection();

		ldapConnection.connect(server, port);
		ldapConnection.bind(userDN, password);

		return ldapConnection;
	}

	/**
	 * Gets LDAP connection for TLS certification
	 * 
	 * @param userDN
	 *            String
	 * @param password
	 *            String
	 * @param portTLS
	 *            integer
	 * @return LDAPConnection
	 */
	public LDAPConnection getLDAPConnection(String userDN, String password, int portTLS) throws LDAPException {
		LDAPConnection ldapConnection = new LDAPConnection(server, port);
		try {
			String workingDir = System.getProperty("user.dir");
			Path filePath = Paths.get(workingDir, filename);
			File file = filePath.toFile();
			if (!file.exists()) {
				// Ldap.TlsInfo info = Ldap.getCerts(server, portTLS, workingDir, null);
			}
			SSLUtil sslUtil = new SSLUtil(new TrustStoreTrustManager(file));
			SSLContext sslContext = sslUtil.createSSLContext();

			/** Use the StartTLS extended operation to secure the connection. */
			ldapConnection.processExtendedOperation(new StartTLSExtendedRequest(sslContext));

			ldapConnection.bind(userDN, password);
			StartTLSPostConnectProcessor startTLSProcessor = new StartTLSPostConnectProcessor(sslContext);
			LDAPConnectionPool pool = new LDAPConnectionPool(ldapConnection, 1, 10, startTLSProcessor);
			/**
			 * Verify that we can use the pool to communicate with the directory
			 * server.
			 */
			ldapConnection = pool.getConnection();
			pool.close();

		} catch (Exception e) {
			logger.error("Exception on TLS connection, {}", e.getMessage());
		}
		return ldapConnection;
	}
}
