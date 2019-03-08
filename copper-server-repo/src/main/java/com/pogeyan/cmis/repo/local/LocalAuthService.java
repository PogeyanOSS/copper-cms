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

import java.io.InvalidObjectException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.impl.Base64;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;

import com.pogeyan.cmis.api.auth.IAuthRequestObject;
import com.pogeyan.cmis.api.auth.IAuthService;
import com.pogeyan.cmis.api.auth.IAuthStoreSettings;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IDBClientFactory;

public class LocalAuthService implements IAuthService {
	private static String USERNAME = "USERNAME";
	private static String PASSWORD = "PASSWORD";
	private LocalStoreSetting storeSettings;

	@Override
	public void setAuthStoreSettings(IAuthStoreSettings storeSetting) throws InvalidTargetObjectTypeException {
		if (storeSetting instanceof LocalStoreSetting) {
			this.storeSettings = (LocalStoreSetting) storeSetting;
		} else {
			throw new InvalidTargetObjectTypeException("storeSettings not of object type LDAPAuthStoreSettings");
		}
	}

	@Override
	public IUserObject authenticate(IAuthRequestObject loginRequest, IDBClientFactory databaseServiceFactory)
			throws InvalidObjectException, CmisPermissionDeniedException {
		if (loginRequest.getAuthorization() != null) {
			String authHeader = loginRequest.getAuthorization();
			Map<String, String> callContext = this.getCallContextMap(authHeader);
			boolean logincheck = false;
			if (this.storeSettings.getUserObject().get(callContext.get(USERNAME)) != null) {
				if (this.storeSettings.getUserObject().get(callContext.get(USERNAME)).getUserNameAndPassword()
						.get(callContext.get(USERNAME)).equals(callContext.get(PASSWORD))) {
					logincheck = true;
				}
			}
			if (logincheck) {
				return this.storeSettings.getUserObject().get(callContext.get(USERNAME));
			}
			return null;
		} else {
			throw new InvalidObjectException("LoginRequest doesn't contain the header key: Authorization");
		}

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
			result.put(LocalAuthService.USERNAME, credentials.substring(0, x));
			result.put(LocalAuthService.PASSWORD, credentials.substring(x + 1));
		}

		return result;
	}

}
