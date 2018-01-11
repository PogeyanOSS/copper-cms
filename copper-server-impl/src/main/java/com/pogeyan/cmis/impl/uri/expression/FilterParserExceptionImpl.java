/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 ******************************************************************************/
package com.pogeyan.cmis.impl.uri.expression;

import com.pogeyan.cmis.api.uri.exception.MessageReference;
import com.pogeyan.cmis.api.uri.expression.CommonExpression;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;

/**
 * This class is used to create exceptions of type FilterParserException.
 * Because this class lies inside org.apache.olingo.odata2.core it is possible
 * to define better/more detailed input parameters for inserting into the
 * exception text.<br>
 * The exception {@link ExpressionParserException} does not know the
 * org.apache.olingo.odata2.core content
 * 
 * 
 */
public class FilterParserExceptionImpl extends ExpressionParserException {
	private static final long serialVersionUID = 77L;

	static public ExpressionParserException createERROR_IN_TOKENIZER(final TokenizerException exceptionTokenizer,
			final String expression) {
		Token token = exceptionTokenizer.getToken();
		MessageReference msgRef = ExpressionParserException.ERROR_IN_TOKENIZER.create();

		msgRef.addContent(token.getUriLiteral());
		msgRef.addContent(Integer.toString(token.getPosition() + 1));
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef, exceptionTokenizer);
	}

	static public ExpressionParserException createINVALID_TRAILING_TOKEN_DETECTED_AFTER_PARSING(final Token token,
			final String expression) {
		MessageReference msgRef = ExpressionParserException.INVALID_TRAILING_TOKEN_DETECTED_AFTER_PARSING.create();

		msgRef.addContent(token.getUriLiteral());
		msgRef.addContent(Integer.toString(token.getPosition() + 1));
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	static public ExpressionParserException createEXPRESSION_EXPECTED_AFTER_POS(final Token token,
			final String expression) {
		MessageReference msgRef = ExpressionParserException.EXPRESSION_EXPECTED_AFTER_POS.create();

		msgRef.addContent(Integer.toString(token.getPosition() + 1));
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	static public ExpressionParserException createEXPRESSION_EXPECTED_AFTER_POS(final int position,
			final String expression) {
		MessageReference msgRef = ExpressionParserException.EXPRESSION_EXPECTED_AFTER_POS.create();

		msgRef.addContent(position);
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	static public ExpressionParserException createCOMMA_OR_END_EXPECTED_AT_POS(final Token token,
			final String expression) {
		MessageReference msgRef = ExpressionParserException.COMMA_OR_END_EXPECTED_AT_POS.create();

		msgRef.addContent(Integer.toString(token.getPosition() + 1));
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	static public ExpressionParserException createEXPRESSION_EXPECTED_AT_POS(final Token token,
			final String expression) {
		MessageReference msgRef = ExpressionParserException.EXPRESSION_EXPECTED_AT_POS.create();

		msgRef.addContent(Integer.toString(token.getPosition() + 1));
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	static public ExpressionParserException createCOMMA_OR_CLOSING_PARENTHESIS_EXPECTED_AFTER_POS(final Token token,
			final String expression) {
		MessageReference msgRef = ExpressionParserException.COMMA_OR_CLOSING_PARENTHESIS_EXPECTED_AFTER_POS.create();

		msgRef.addContent(Integer.toString(token.getPosition() + token.getUriLiteral().length()));
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	public static ExpressionParserException createMETHOD_WRONG_ARG_COUNT(final MethodExpressionImpl methodExpression,
			final Token token, final String expression) {
		MessageReference msgRef = null;
		int minParam = methodExpression.getMethodInfo().getMinParameter();
		int maxParam = methodExpression.getMethodInfo().getMaxParameter();

		if ((minParam == -1) && (maxParam == -1)) {
			// no exception thrown in this case
		} else if ((minParam != -1) && (maxParam == -1)) {
			// Tested with TestParserExceptions.TestPMreadParameters CASE 7-1
			msgRef = ExpressionParserException.METHOD_WRONG_ARG_X_OR_MORE.create();
			msgRef.addContent(methodExpression.getMethod().toUriLiteral());
			msgRef.addContent(token.getPosition() + 1);
			msgRef.addContent(expression);
			msgRef.addContent(minParam);
		} else if ((minParam == -1) && (maxParam != -1)) {
			// Tested with TestParserExceptions.TestPMreadParameters CASE 8-2
			msgRef = ExpressionParserException.METHOD_WRONG_ARG_X_OR_LESS.create();
			msgRef.addContent(methodExpression.getMethod().toUriLiteral());
			msgRef.addContent(token.getPosition() + 1);
			msgRef.addContent(expression);
			msgRef.addContent(maxParam);
		} else if ((minParam != -1) && (maxParam != -1)) {
			if (minParam == maxParam) {
				// Tested with TestParserExceptions.TestPMreadParameters CASE
				// 11-1
				msgRef = ExpressionParserException.METHOD_WRONG_ARG_EXACT.create();
				msgRef.addContent(methodExpression.getMethod().toUriLiteral());
				msgRef.addContent(token.getPosition() + 1);
				msgRef.addContent(expression);
				msgRef.addContent(minParam);
			} else {
				// Tested with TestParserExceptions.TestPMreadParameters CASE
				// 10-1
				msgRef = ExpressionParserException.METHOD_WRONG_ARG_BETWEEN.create();
				msgRef.addContent(methodExpression.getMethod().toUriLiteral());
				msgRef.addContent(token.getPosition() + 1);
				msgRef.addContent(expression);
				msgRef.addContent(minParam);
				msgRef.addContent(maxParam);
			}
		}

		return new ExpressionParserException(msgRef);
	}

	public static ExpressionParserException createMETHOD_WRONG_INPUT_TYPE(final MethodExpressionImpl methodExpression,
			final Token token, final String expression) {
		MessageReference msgRef = null;

		// Tested with TestParserExceptions.TestPMreadParameters CASE 7-1
		msgRef = ExpressionParserException.METHOD_WRONG_INPUT_TYPE.create();
		msgRef.addContent(methodExpression.getMethod().toUriLiteral());
		msgRef.addContent(token.getPosition() + 1);
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	public static ExpressionParserException createLEFT_SIDE_NOT_A_PROPERTY(final Token token, final String expression)
			throws ExpressionParserInternalError {
		MessageReference msgRef = ExpressionParserException.LEFT_SIDE_NOT_A_PROPERTY.create();

		msgRef.addContent(token.getPosition() + 1);
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	public static ExpressionParserException createMISSING_CLOSING_PARENTHESIS(final int position,
			final String expression, final TokenizerExpectError e) {
		MessageReference msgRef = ExpressionParserException.MISSING_CLOSING_PARENTHESIS.create();

		msgRef.addContent(position + 1);
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef, e);
	}

	public static ExpressionParserException createINVALID_SORT_ORDER(final Token token, final String expression) {
		MessageReference msgRef = ExpressionParserException.INVALID_SORT_ORDER.create();
		msgRef.addContent(token.getPosition() + 1);
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);
	}

	public static ExpressionParserException createINVALID_METHOD_CALL(final CommonExpression leftNode,
			final Token prevToken, final String expression) {
		final MessageReference msgRef = ExpressionParserException.INVALID_METHOD_CALL.create();

		msgRef.addContent(leftNode.getUriLiteral());
		msgRef.addContent(prevToken.getPosition() + 1);
		msgRef.addContent(expression);

		return new ExpressionParserException(msgRef);

	}

	public static TokenizerException createTOKEN_UNDETERMINATED_STRING(int oldPosition, String expression) {
		final MessageReference msgRef = ExpressionParserException.TOKEN_UNDETERMINATED_STRING.create();

	    msgRef.addContent(oldPosition);
	    msgRef.addContent(expression);

	    return new TokenizerException(msgRef);
	}
}
