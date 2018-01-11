package com.pogeyan.cmis.api.uri.exception;

import java.util.Locale;

public abstract class BaseUriException extends Throwable {
	protected MessageReference messageReference;
	protected final String errorCode;

	private static final long serialVersionUID = -6884673558124441214L;
	protected static final Locale DEFAULT_LOCALE = Locale.ENGLISH;
	
	public static final MessageReference COMMON = createMessageReference(BaseUriException.class, "COMMON");

	protected BaseUriException(final Throwable e) {
		this(null, e, null);
	}

	protected BaseUriException(final MessageReference msgRef) {
		this(msgRef, null, null);
	}

	/**
	 * Creates {@link BaseUriException} with given {@link MessageReference} and
	 * cause {@link Throwable} which caused this exception.
	 * 
	 * @param messageReference
	 *            references the message text (and additional values) of this
	 *            {@link BaseUriException}
	 * @param cause
	 *            exception which caused this exception
	 */
	public BaseUriException(final MessageReference messageReference, final Throwable cause) {
		this(messageReference, cause, null);
	}

	/**
	 * Creates {@link BaseUriException} with given {@link MessageReference},
	 * cause {@link Throwable} and error code.
	 * 
	 * @param messageReference
	 *            references the message text (and additional values) of this
	 *            {@link BaseUriException}
	 * @param cause
	 *            exception which caused this exception
	 * @param errorCode
	 *            a String with a unique code identifying this exception
	 */
	public BaseUriException(final MessageReference messageReference, final Throwable cause, final String errorCode) {
		super(cause);
		this.messageReference = messageReference;
		this.errorCode = errorCode;
	}

	/**
	 * Creates {@link BaseUriException} with given {@link MessageReference} and
	 * error code.
	 * 
	 * @param messageReference
	 *            references the message text (and additional values) of this
	 *            {@link ODataMessageException}
	 * @param errorCode
	 *            a String with a unique code identifying this exception
	 */
	public BaseUriException(final MessageReference messageReference, final String errorCode) {
		this(messageReference, null, errorCode);
	}

	/**
	 * The method creates a Reference to Message Object
	 * {@link org.apache.olingo.odata2.api.exception.MessageReference} . The
	 * message text key is derived out of parameters clazz.messageReferenceKey.
	 * 
	 * @param clazz
	 *            is name of the class extending
	 *            {@link org.apache.olingo.odata2.jpa.processor.api.exception.ODataJPAException}
	 * @param messageReferenceKey
	 *            is the key of the message
	 * @return an instance of type
	 *         {@link org.apache.olingo.odata2.api.exception.MessageReference}
	 */
	protected static MessageReference createMessageReference(final Class<? extends Throwable> clazz,
			final String messageReferenceKey) {
		return MessageReference.create(clazz, messageReferenceKey);
	}

	public MessageReference getMessageReference() {
		return messageReference;
	}
}
