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

import java.util.function.Supplier;

import org.apache.chemistry.opencmis.commons.exceptions.CmisConstraintException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisContentAlreadyExistsException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisFilterNotValidException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNameConstraintViolationException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisServiceUnavailableException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisStorageException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisStreamNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisTooManyRequestsException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUpdateConflictException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisVersioningException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.util.concurrent.UncheckedExecutionException;
import com.pogeyan.cmis.api.BaseResponse;

public class CmisBaseResponse extends BaseResponse {
	private static final Logger LOG = LoggerFactory.getLogger(CmisBaseResponse.class);

	public static final int InvalidArgumentExceptionCode = 400;
	public static final int ConstraintExceptionCode = 409;
	public static final int CmisObjectNotFoundExceptionCode = 404;
	public static final int CmisUpdateConflictExceptionCode = 409;
	public static final int CmisContentAlreadyExistsExceptionCode = 409;
	public static final int CmisNotSupportedExceptionCode = 405;
	public static final int IllegalArgumentExceptionCode = 400;
	public static final int CmisFilterNotValidExceptionCode = 400;
	public static final int CmisNameConstraintViolationExceptionCode = 409;
	public static final int CmisRuntimeExceptionCode = 500;
	public static final int CmisPermissionDeniedExceptionCode = 403;
	public static final int CmisStorageExceptionCode = 500;
	public static final int CmisStreamNotSupportedExceptionCode = 403;
	public static final int CmisVersioningExceptionCode = 409;
	public static final int CmisTooManyRequestsExceptionCode = 429;
	public static final int CmisServiceUnavailableExceptionCode = 503;
	private Object cmisData;

	public static CmisBaseResponse fromWithTryCatch(Supplier<Object> fn) {
		CmisBaseResponse r = new CmisBaseResponse();
		try {
			Object result = fn.get();
			r.setCmisData(result);
		} catch (Exception ex) {
			LOG.error("CmisBaseResponse error: {}, {}", ex.getMessage(), ExceptionUtils.getStackTrace(ex));
			if (ex instanceof CmisInvalidArgumentException) {
				r = setCmisResponse(ex.getMessage(), InvalidArgumentExceptionCode);
				return r;
			} else if (ex instanceof CmisObjectNotFoundException) {
				r = setCmisResponse(ex.getMessage(), CmisObjectNotFoundExceptionCode);
				return r;
			} else if (ex instanceof CmisConstraintException) {
				r = setCmisResponse(ex.getMessage(), ConstraintExceptionCode);
				return r;
			} else if (ex instanceof CmisUpdateConflictException) {
				r = setCmisResponse(ex.getMessage(), CmisUpdateConflictExceptionCode);
				return r;
			} else if (ex instanceof CmisNotSupportedException) {
				r = setCmisResponse(ex.getMessage(), CmisNotSupportedExceptionCode);
				return r;
			} else if (ex instanceof IllegalArgumentException) {
				r = setCmisResponse(ex.getMessage(), IllegalArgumentExceptionCode);
				return r;
			} else if (ex instanceof CmisRuntimeException) {
				r = setCmisResponse(ex.getMessage(), CmisRuntimeExceptionCode);
				return r;
			} else if (ex instanceof CmisStorageException) {
				r = setCmisResponse(ex.getMessage(), CmisStorageExceptionCode);
				return r;
			} else if (ex instanceof CmisContentAlreadyExistsException) {
				r = setCmisResponse(ex.getMessage(), CmisContentAlreadyExistsExceptionCode);
				return r;
			} else if (ex instanceof CmisFilterNotValidException) {
				r = setCmisResponse(ex.getMessage(), CmisFilterNotValidExceptionCode);
				return r;
			} else if (ex instanceof CmisNameConstraintViolationException) {
				r = setCmisResponse(ex.getMessage(), CmisNameConstraintViolationExceptionCode);
				return r;
			} else if (ex instanceof CmisPermissionDeniedException) {
				r = setCmisResponse(ex.getMessage(), CmisPermissionDeniedExceptionCode);
				return r;
			} else if (ex instanceof CmisStreamNotSupportedException) {
				r = setCmisResponse(ex.getMessage(), CmisStreamNotSupportedExceptionCode);
				return r;
			} else if (ex instanceof CmisVersioningException) {
				r = setCmisResponse(ex.getMessage(), CmisVersioningExceptionCode);
				return r;
			} else if (ex instanceof CmisTooManyRequestsException) {
				r = setCmisResponse(ex.getMessage(), CmisTooManyRequestsExceptionCode);
				return r;
			} else if (ex instanceof CmisServiceUnavailableException) {
				r = setCmisResponse(ex.getMessage(), CmisServiceUnavailableExceptionCode);
				return r;
			} else if (ex instanceof UncheckedExecutionException) {
				r = setCmisResponse(ex.getMessage(), CmisServiceUnavailableExceptionCode);
				return r;
			} else {
				r = setCmisResponse(ex.getMessage() + "\n" + ExceptionUtils.getStackTrace(ex),
						CmisServiceUnavailableExceptionCode);
				return r;
			}
		}
		return r;
	}

	public static CmisBaseResponse setCmisResponse(String message, int errorCode) {
		CmisBaseResponse r = new CmisBaseResponse();
		CmisErrorResponse resp = new CmisErrorResponse();
		resp.setError(message);
		resp.setErrorCode(errorCode);
		r.setCmisData(resp);
		r.setError(message);
		return r;
	}

	public Object getCmisData() {
		return cmisData;
	}

	public void setCmisData(Object cmisData) {
		this.cmisData = cmisData;
	}

	public static CmisBaseResponse from(Object o) {
		CmisBaseResponse r = new CmisBaseResponse();
		r.setCmisData(o);
		return r;
	}
}
