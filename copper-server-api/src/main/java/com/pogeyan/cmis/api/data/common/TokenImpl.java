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
package com.pogeyan.cmis.api.data.common;

public class TokenImpl {
	TokenChangeType changeType;
	Long time;

	public TokenImpl() {

	}

	public TokenImpl(TokenChangeType changetype, Long time) {
		this.changeType = changetype;
		this.time = time;
	}

	public TokenChangeType getChangeType() {
		return changeType;
	}

	public void setChangeType(TokenChangeType changetype) {
		this.changeType = changetype;
	}

	public Long getTime() {
		return time;
	}

	public void setTime(Long time) {
		this.time = time;
	}

}
