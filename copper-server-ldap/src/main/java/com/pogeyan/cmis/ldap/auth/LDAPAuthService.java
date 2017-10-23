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

import java.io.InvalidObjectException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.impl.Base64;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.auth.IAuthRequestObject;
import com.pogeyan.cmis.api.auth.IAuthService;
import com.pogeyan.cmis.api.auth.IAuthStoreSettings;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.ldap.LDAPUtils;
import com.pogeyan.cmis.ldap.model.LDAPLogin;
import com.pogeyan.cmis.ldap.model.LoginProperties;

public class LDAPAuthService implements IAuthService {
	private static final Logger LOG = LoggerFactory.getLogger(LDAPAuthService.class);

	private LDAPAuthStoreSettings storeSettings;
	private static String USERNAME = "USERNAME";
	private static String PASSWORD = "PASSWORD";

	@Override
	public IUserObject authenticate(IAuthRequestObject loginRequest)
			throws InvalidObjectException, CmisPermissionDeniedException {
		if (loginRequest.getAuthorization() != null) {
			String authHeader = loginRequest.getAuthorization();
			Map<String, String> callContext = this.getCallContextMap(authHeader);
			return this.authenticateInternal(loginRequest.getRepositoryId(), callContext.get(USERNAME),
					callContext.get(PASSWORD));
		} else {
			throw new InvalidObjectException("LoginRequest doesn't contain the header key: Authorization");
		}
	}

	@Override
	public void setAuthStoreSettings(IAuthStoreSettings storeSettings) throws InvalidTargetObjectTypeException {
		if (storeSettings instanceof LDAPAuthStoreSettings) {
			this.storeSettings = (LDAPAuthStoreSettings) storeSettings;
		} else {
			throw new InvalidTargetObjectTypeException("storeSettings not of object type LDAPAuthStoreSettings");
		}
	}

	/**
	 * fetch repository MRepository based on RepositoryId from CallContext and
	 * Takes user and password from the CallContext and checks them.
	 */
	private LDAPLogin authenticateInternal(String repositoryId, String userName, String password)
			throws CmisPermissionDeniedException {
		LoginProperties loginProperties = new LoginProperties();
		if (StringUtils.isNotBlank(this.storeSettings.getCompanyName())) {
			loginProperties.setCompanyName(this.storeSettings.getCompanyName());
		}

		loginProperties.setAdminUser(this.storeSettings.getAdminUser());
		loginProperties.setPort(this.storeSettings.getPort());
		loginProperties.setServerName(this.storeSettings.getServerName());
		loginProperties.setUserName(userName);
		loginProperties.setPassword(password);
		loginProperties.setMasterCompany(this.storeSettings.getMastercompany());

		LDAPLogin login;
		try {
			login = LDAPUtils.login(loginProperties);
			if (login != null) {
				LOG.info("LDAP login successfull {}", userName);
				return login;
			}
		} catch (Exception e) {
			LOG.error("writeContent exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
		}

		throw new CmisPermissionDeniedException("Login authentication failed for user: " + userName);
	}

	private Map<String, String> getCallContextMap(String authHeader) {
		Map<String, String> result = null;
		if ((authHeader != null) && (authHeader.trim().toLowerCase(Locale.ENGLISH).startsWith("basic "))) {
			int x = authHeader.lastIndexOf(' ');
			if (x == -1) {
				return result;
			}

			String credentials = null;
			try {
				credentials = new String(Base64.decode(authHeader.substring(x + 1).getBytes(IOUtils.ISO_8859_1)),
						IOUtils.UTF8);
			} catch (Exception e) {
				return result;
			}

			x = credentials.indexOf(':');
			if (x == -1) {
				return result;
			}

			// extract user and password and add them to map
			result = new HashMap<String, String>();
			result.put(LDAPAuthService.USERNAME, credentials.substring(0, x));
			result.put(LDAPAuthService.PASSWORD, credentials.substring(x + 1));
		}

		return result;
	}
}
