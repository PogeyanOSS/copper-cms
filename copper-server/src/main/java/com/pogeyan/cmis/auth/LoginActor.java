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
package com.pogeyan.cmis.auth;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.auth.IAuthService;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.auth.LoginRequestObject;
import com.pogeyan.cmis.api.messages.LoginRequest;
import com.pogeyan.cmis.api.messages.LoginResponse;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.service.factory.LoginAuthServiceFactory;

public class LoginActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(LoginActor.class);

	public LoginActor() {
		this.registerMessageHandle("authenticate", LoginRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> this.authenticate((LoginRequest) t, b)));
	}

	public String getName() {
		return "login";
	}

	private LoginResponse authenticate(LoginRequest t, HashMap<String, Object> baggage) {
		LoginResponse response = new LoginResponse();
		try {
			Map<String, String> loginSettings = RepositoryManagerFactory.getLoginDetails(t.getRepositoryId());
			if (LOG.isDebugEnabled()) {
				LOG.debug("Login settings for repositoryId: {}", loginSettings.toString());
			}

			IAuthService authService = LoginAuthServiceFactory.createAuthService(loginSettings);
			if (authService != null) {
				LoginRequestObject loginObject = new LoginRequestObject(t.getHeaders().get("authorization"),
						t.getRepositoryId());
				IUserObject result = authService.authenticate(loginObject);
				response.setSuccessfulLogin(result != null);
				response.setLoginDetails(result);
			} else {
				LOG.error("Login authenticate service not found for: {}", loginSettings.toString());
				response.setSuccessfulLogin(false);
			}
		} catch (Exception e) {
			LOG.error("Login authenticate error: {}", ExceptionUtils.getStackTrace(e));
			response.setSuccessfulLogin(false);
		}

		return response;
	}
}
