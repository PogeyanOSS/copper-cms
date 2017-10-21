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

import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.auth.IUserObject;

public class LoginResponse extends BaseResponse {
	private boolean isSuccessfulLogin;
	private IUserObject loginDetails;

	public boolean isSuccessfulLogin() {
		return isSuccessfulLogin;
	}

	public void setSuccessfulLogin(boolean isSuccessfulLogin) {
		this.isSuccessfulLogin = isSuccessfulLogin;
	}

	/**
	 * @return the loginDetails
	 */
	public IUserObject getLoginDetails() {
		return loginDetails;
	}

	/**
	 * @param loginDetails
	 *            the loginDetails to set
	 */
	public void setLoginDetails(IUserObject loginDetails) {
		this.loginDetails = loginDetails;
	}
}
