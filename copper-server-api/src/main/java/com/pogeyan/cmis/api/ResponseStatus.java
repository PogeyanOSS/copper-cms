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
 * The ResponseStatus with error code and error message string.
 */
public abstract class ResponseStatus {
	private String error;
	private int errorCode;

	/**
	 * Instantiates a new response status.
	 */
	public ResponseStatus() {
	}

	/**
	 * Gets the error.
	 *
	 * @return the error
	 */
	public final String getError() {
		return error;
	}

	/**
	 * Sets the error.
	 *
	 * @param error
	 *            the new error
	 */
	public final void setError(final String error) {
		this.error = error;
	}

	/**
	 * Gets the error code.
	 *
	 * @return the error code
	 */
	public final int getErrorCode() {
		return errorCode;
	}

	/**
	 * Sets the error code.
	 *
	 * @param errorCode
	 *            the new error code
	 */
	public final void setErrorCode(final int errorCode) {
		this.errorCode = errorCode;
	}

	@Override
	public String toString() {
		return "ErrorCode [" + this.errorCode + "];ErrorMsg [" + this.error != null ? this.error : "" + "]";
	}
}
