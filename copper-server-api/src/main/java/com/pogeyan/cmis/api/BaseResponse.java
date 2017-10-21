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
package com.pogeyan.cmis.api;

/**
 * Response class to be used in all actor message implementation.
 */
public class BaseResponse extends ResponseStatus {
	private String repositoryId;

	public String getRepositoryId() {
		return repositoryId;
	}

	public void setRepositoryId(String repositoryId) {
		this.repositoryId = repositoryId;
	}	

	public static BaseResponse error(String errorMsg) {
		return BaseResponse.error(errorMsg, 500);
	}

	public static BaseResponse error(String errorMsg, int errorCode) {
		BaseResponse r = new BaseResponse();
		r.setError(errorMsg);
		r.setErrorCode(errorCode);
		return r;
	}
}
