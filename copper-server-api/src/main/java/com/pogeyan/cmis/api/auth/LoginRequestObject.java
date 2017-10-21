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
package com.pogeyan.cmis.api.auth;

public class LoginRequestObject implements IAuthRequestObject {

	private String authorization;
	private String repositoryId;

	public LoginRequestObject(String authorization, String repoistoryId) {
		this.repositoryId = repoistoryId;
		this.authorization = authorization;
	}

	@Override
	public String getAuthorization() {
		return this.authorization;
	}

	@Override
	public String getRepositoryId() {
		return this.repositoryId;
	}

	public void setAuthorization(String authorization) {
		this.authorization = authorization;
	}

	public void setRepositoryId(String repositoryId) {
		this.repositoryId = repositoryId;
	}

}
