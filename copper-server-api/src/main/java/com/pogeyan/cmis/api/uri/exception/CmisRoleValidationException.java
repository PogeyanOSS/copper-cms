package com.pogeyan.cmis.api.uri.exception;

import java.math.BigInteger;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;

/**
 * CMIS ObjectNotFound Exception.
 * <p>
 * Cause: The service call has specified an object, an object-type or a
 * repository that does not exist.
 */
public class CmisRoleValidationException extends CmisBaseException {

	private static final long serialVersionUID = 1L;
	public static final String EXCEPTION_NAME = "roleValidationError";

	/**
	 * Default constructor.
	 */
	public CmisRoleValidationException() {
		super();
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param code
	 *            error code
	 * @param cause
	 *            the cause
	 */
	public CmisRoleValidationException(String message, BigInteger code, Throwable cause) {
		super(message, code, cause);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param errorContent
	 *            error page content
	 */
	public CmisRoleValidationException(String message, String errorContent) {
		super(message, errorContent);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param code
	 *            error code
	 */
	public CmisRoleValidationException(String message, BigInteger code) {
		super(message, code);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param code
	 *            error code
	 * @param errorContent
	 *            error page content
	 */
	public CmisRoleValidationException(String message, BigInteger code, String errorContent) {
		super(message, code, errorContent);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param code
	 *            error code
	 * @param errorContent
	 *            error page content
	 * @param additionalData
	 *            additional data
	 */
	public CmisRoleValidationException(String message, BigInteger code, String errorContent,
			Map<String, String> additionalData) {
		super(message, code, errorContent, additionalData);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param errorContent
	 *            error page content
	 * @param additionalData
	 *            additional data
	 * @param cause
	 *            the cause
	 */
	public CmisRoleValidationException(String message, String errorContent, Map<String, String> additionalData,
			Throwable cause) {
		super(message, errorContent, additionalData, cause);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param errorContent
	 *            error page content
	 * @param cause
	 *            the cause
	 */
	public CmisRoleValidationException(String message, String errorContent, Throwable cause) {
		super(message, errorContent, cause);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 * @param cause
	 *            the cause
	 */
	public CmisRoleValidationException(String message, Throwable cause) {
		super(message, BigInteger.ZERO, cause);
	}

	/**
	 * Constructor.
	 * 
	 * @param message
	 *            error message
	 */
	public CmisRoleValidationException(String message) {
		super(message, BigInteger.ZERO);
	}

	@Override
	public final String getExceptionName() {
		return EXCEPTION_NAME;
	}
}
