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
package com.pogeyan.cmis.impl.factory;

import java.util.HashMap;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import com.pogeyan.cmis.api.auth.IAuthFactory;
import com.pogeyan.cmis.api.auth.IAuthService;
import com.pogeyan.cmis.api.auth.IAuthStoreSettings;

public class LoginAuthServiceFactory {
	static Map<String, IAuthFactory> authFactory = new HashMap<String, IAuthFactory>();

	public static IAuthService createAuthService(Map<String, String> loginSettings)
			throws InvalidTargetObjectTypeException {
		if (loginSettings.containsKey("type")) {
			String type = loginSettings.get("type");
			type = type.contains(":") ? type.split(":")[0] : type;
			if (authFactory.get(type) != null) {
				IAuthStoreSettings authStoreSetting = authFactory.get(type).getStoreSetting();
				authStoreSetting.setStoreSetting(loginSettings);
				return authFactory.get(type).getAuthService(authStoreSetting);
			} else {
				return null;
			}
		}

		return null;
	}

	public static void add(IAuthFactory authFactoryCalss) {
		authFactory.put(authFactoryCalss.getStoreSetting().getType(), authFactoryCalss);
	}
}
