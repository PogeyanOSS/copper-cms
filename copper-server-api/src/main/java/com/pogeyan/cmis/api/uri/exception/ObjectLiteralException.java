package com.pogeyan.cmis.api.uri.exception;

import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;

public class ObjectLiteralException extends BaseUriException {

	private static final long serialVersionUID = 3769L;
	public static final MessageReference COMMON_ERROR = createMessageReference(ExpressionParserException.class, "COMMON");
	
	public ObjectLiteralException() {
		super(COMMON_ERROR);
	}

}
