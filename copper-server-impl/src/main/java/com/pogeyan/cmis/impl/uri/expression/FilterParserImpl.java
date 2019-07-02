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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.pogeyan.cmis.api.uri.expression.BinaryExpression;
import com.pogeyan.cmis.api.uri.expression.BinaryOperator;
import com.pogeyan.cmis.api.uri.expression.CommonExpression;
import com.pogeyan.cmis.api.uri.expression.ExpressionKind;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;
import com.pogeyan.cmis.api.uri.expression.LiteralExpression;
import com.pogeyan.cmis.api.uri.expression.MethodExpression;
import com.pogeyan.cmis.api.uri.expression.MethodOperator;
import com.pogeyan.cmis.api.uri.expression.UnaryExpression;
import com.pogeyan.cmis.api.uri.expression.UnaryOperator;

/**
 *  
 */
public class FilterParserImpl implements FilterParser {
	/* do the static initialization */
	protected static Map<String, InfoBinaryOperator> availableBinaryOperators;
	protected static Map<String, InfoMethod> availableMethods;
	protected static Map<String, InfoUnaryOperator> availableUnaryOperators;

	static {
		initAvailTables();
	}

	/* instance attributes */
	protected TokenList tokenList = null;
	protected String curExpression;

	/**
	 * Creates a new FilterParser implementation
	 * 
	 * @param resourceEntityType
	 *            EntityType of the resource on which the filter is applied
	 */
	public FilterParserImpl() {
	}

	@Override
	public FilterExpression parseFilterString(final String filterExpression)
			throws ExpressionParserException {
		return parseFilterString(filterExpression, false);
	}

	public FilterExpression parseFilterString(final String filterExpression, final boolean allowOnlyBinary)
			throws ExpressionParserException {
		CommonExpression node = null;
		curExpression = filterExpression;
		try {
			// Throws TokenizerException and FilterParserException.
			// FilterParserException is caught somewhere above
			tokenList = new Tokenizer(filterExpression).tokenize();
			if (!tokenList.hasTokens()) {
				return new FilterExpressionImpl(filterExpression);
			}
		} catch (TokenizerException tokenizerException) {
			// Tested with TestParserExceptions.TestPMparseFilterString
			throw FilterParserExceptionImpl.createERROR_IN_TOKENIZER(tokenizerException, curExpression);
		}

		try {
			CommonExpression nodeLeft = readElement(null);
			node = readElements(nodeLeft, 0);
		} catch (ExpressionParserException filterParserException) {
			// Add empty filterTree to Exception
			// Tested for original throw point
			filterParserException.setFilterTree(new FilterExpressionImpl(filterExpression));
			throw filterParserException;
		} catch (ExpressionParserInternalError ex) {
			// throw FilterParserExceptionImpl.COMMON_ERROR(ex, curExpression);
			ex.printStackTrace();
		}

		// Post check
		if (tokenList.tokenCount() > tokenList.currentToken) // this indicates
																// that not all
																// tokens have
																// been read
		{
			// Tested with TestParserExceptions.TestPMparseFilterString
			throw FilterParserExceptionImpl.createINVALID_TRAILING_TOKEN_DETECTED_AFTER_PARSING(
					tokenList.elementAt(tokenList.currentToken), filterExpression);
		}

		// Create and return filterExpression node
		/*
		 * if ((allowOnlyBinary == true) && (node.getEdmType() != null) &&
		 * (node.getEdmType() !=
		 * EdmSimpleTypeKind.Boolean.getEdmSimpleTypeInstance())) { // Tested
		 * with TestParserExceptions.testAdditionalStuff CASE 9 throw
		 * FilterParserExceptionImpl.createTYPE_EXPECTED_AT(EdmBoolean.
		 * getInstance(), node.getEdmType(), 1, curExpression); }
		 */

		return new FilterExpressionImpl(filterExpression, node);
	}

	protected CommonExpression readElements(final CommonExpression leftExpression, final int priority)
			throws ExpressionParserException, ExpressionParserInternalError {
		CommonExpression leftNode = leftExpression;
		CommonExpression rightNode;
		BinaryExpression binaryNode;

		ActualBinaryOperator operator = readBinaryOperator();
		ActualBinaryOperator nextOperator;

		while ((operator != null) && (operator.getOP().getPriority() >= priority)) {
			tokenList.next(); // eat the operator
			rightNode = readElement(leftNode, operator); // throws
															// FilterParserException,
															// FilterParserInternalError
			if (rightNode == null) {
				// Tested with TestParserExceptions.testAdditionalStuff CASE 10
				throw FilterParserExceptionImpl.createEXPRESSION_EXPECTED_AFTER_POS(
						operator.getToken().getPosition() + operator.getToken().getUriLiteral().length(),
						curExpression);
			}
			nextOperator = readBinaryOperator();

			// It must be "while" because for example in "Filter=a or c eq d and
			// e eq f"
			// after reading the "eq" operator the "and" operator must be
			// consumed too. This is due to the fact that "and" has
			// a higher priority than "or"
			while ((nextOperator != null) && (nextOperator.getOP().getPriority() > operator.getOP().getPriority())) {
				// recurse until the a binary operator with a lower priority is
				// detected
				rightNode = readElements(rightNode, nextOperator.getOP().getPriority());
				nextOperator = readBinaryOperator();
			}

			// Although the member operator is also a binary operator, there is
			// some special handling in the filterTree
			if (operator.getOP().getOperator() == BinaryOperator.PROPERTY_ACCESS) {
				binaryNode = new MemberExpressionImpl(leftNode, rightNode);
			} else {
				binaryNode = new BinaryExpressionImpl(operator.getOP(), leftNode, rightNode, operator.getToken());
			}

			// try {
			// validateBinaryOperatorTypes(binaryNode);
			// } catch (ExpressionParserException expressionException) {
			// // Extend the error information
			// // Tested for original throw point
			// expressionException.setFilterTree(binaryNode);
			// throw expressionException;
			// }

			leftNode = binaryNode;
			operator = readBinaryOperator();
		}

		// Add special handling for expressions like
		// $filter=notsupportedfunction('a')
		// If this special handling is not in place the error text would be
		// -->Invalid token "(" detected after parsing at position 21 in
		// "notsupportedfunction('a')".
		// with this special handling we ensure that the error text would be

		Token token = tokenList.lookToken();
		if (token != null) {
			if ((leftNode.getKind() == ExpressionKind.PROPERTY)
					&& (tokenList.lookToken().getKind() == TokenKind.OPENPAREN)) {
				// Tested with TestParserExceptions.testAdditionalStuff CASE 2
				throw FilterParserExceptionImpl.createINVALID_METHOD_CALL(leftNode, tokenList.lookPrevToken(),
						curExpression);
			}
		}

		return leftNode;
	}

	/**
	 * Reads the content between parenthesis. Its is expected that the current
	 * token is of kind {@link TokenKind#OPENPAREN} because it MUST be check in
	 * the calling method ( when read the method name and the '(' is read).
	 * 
	 * @return An expression which reflects the content within the parenthesis
	 * @throws ExpressionParserException
	 *             While reading the elements in the parenthesis an error
	 *             occurred
	 * @throws TokenizerMessage
	 *             The next token did not match the expected token
	 */
	protected CommonExpression readParenthesis() throws ExpressionParserException, ExpressionParserInternalError {
		// The existing of a '(' is verified BEFORE this method is called --> so
		// it's a internal error
		Token openParenthesis = tokenList.expectToken(TokenKind.OPENPAREN, true);

		CommonExpression firstExpression = readElement(null);
		CommonExpression parenthesisExpression = readElements(firstExpression, 0);

		// check for ')'
		try {
			tokenList.expectToken(TokenKind.CLOSEPAREN); // TokenizerMessage
		} catch (TokenizerExpectError e) {
			// Internal parsing error, even if there are no more token (then
			// there should be a different exception).
			// Tested with TestParserExceptions.TestPMreadParenthesis
			throw FilterParserExceptionImpl.createMISSING_CLOSING_PARENTHESIS(openParenthesis.getPosition(),
					curExpression, e);
		}
		return parenthesisExpression;
	}

	/**
	 * Read the parameters of a method expression
	 * 
	 * @param methodInfo
	 *            Signature information about the method whose parameters should
	 *            be read
	 * @param methodExpression
	 *            Method expression to which the read parameters are added
	 * @return The method expression input parameter
	 * @throws ExpressionParserException
	 * @throws ExpressionParserInternalError
	 * @throws TokenizerExpectError
	 *             The next token did not match the expected token
	 */
	protected MethodExpression readParameters(final InfoMethod methodInfo, final MethodExpressionImpl methodExpression,
			final Token methodToken) throws ExpressionParserException, ExpressionParserInternalError {
		CommonExpression expression;
		boolean expectAnotherExpression = false;
		boolean readComma = true;

		// The existing of a '(' is verified BEFORE this method is called --> so
		// it's a internal error
		Token openParenthesis = tokenList.expectToken(TokenKind.OPENPAREN, true); // throws
																					// FilterParserInternalError

		Token token = tokenList.lookToken();
		if (token == null) {
			// Tested with TestParserExceptions.TestPMreadParameters CASE 1 e.g.
			// "$filter=concat("
			throw FilterParserExceptionImpl.createEXPRESSION_EXPECTED_AFTER_POS(openParenthesis, curExpression);
		}

		while (token.getKind() != TokenKind.CLOSEPAREN) {
			if (readComma == false) {
				// Tested with TestParserExceptions.TestPMreadParameters CASE 12
				// e.g. "$filter=concat('a' 'b')"
				throw FilterParserExceptionImpl.createCOMMA_OR_CLOSING_PARENTHESIS_EXPECTED_AFTER_POS(
						tokenList.lookPrevToken(), curExpression);
			}
			expression = readElement(null);
			if (expression != null) {
				expression = readElements(expression, 0);
			}

			if ((expression == null) && (expectAnotherExpression == true)) {
				// Tested with TestParserExceptions.TestPMreadParameters CASE 4
				// e.g. "$filter=concat(,"
				throw FilterParserExceptionImpl.createEXPRESSION_EXPECTED_AFTER_POS(token, curExpression);
			} else if (expression != null) {// parameter list may be empty
				methodExpression.appendParameter(expression);
			}

			token = tokenList.lookToken();
			if (token == null) {
				// Tested with TestParserExceptions.TestPMreadParameters CASE 2
				// e.g. "$filter=concat(123"
				throw FilterParserExceptionImpl.createCOMMA_OR_CLOSING_PARENTHESIS_EXPECTED_AFTER_POS(
						tokenList.lookPrevToken(), curExpression);
			}

			if (token.getKind() == TokenKind.COMMA) {
				expectAnotherExpression = true;
				if (expression == null) {
					// Tested with TestParserExceptions.TestPMreadParameters
					// CASE 3 e.g. "$filter=concat(,"
					throw FilterParserExceptionImpl.createEXPRESSION_EXPECTED_AT_POS(token, curExpression);
				}

				tokenList.expectToken(",", true);
				readComma = true;
			} else {
				readComma = false;
			}
		}

		// because the while loop above only exits if a ')' has been found it is
		// an
		// internal error if there is not ')'
		tokenList.expectToken(TokenKind.CLOSEPAREN, true);

		// ---check parameter count
		int count = methodExpression.getParameters().size();
		if ((methodInfo.getMinParameter() > -1) && (count < methodInfo.getMinParameter())) {
			// Tested with TestParserExceptions.TestPMreadParameters CASE 12
			throw FilterParserExceptionImpl.createMETHOD_WRONG_ARG_COUNT(methodExpression, methodToken, curExpression);
		}

		if ((methodInfo.getMaxParameter() > -1) && (count > methodInfo.getMaxParameter())) {
			// Tested with TestParserExceptions.TestPMreadParameters CASE 15
			throw FilterParserExceptionImpl.createMETHOD_WRONG_ARG_COUNT(methodExpression, methodToken, curExpression);
		}

		return methodExpression;
	}

	protected CommonExpression readElement(final CommonExpression leftExpression)
			throws ExpressionParserException, ExpressionParserInternalError {
		return readElement(leftExpression, null);
	}

	/**
	 * Reads: Unary operators, Methods, Properties, ... but not binary operators
	 * which are handelt in {@link #readElements(CommonExpression, int)}
	 * 
	 * @param leftExpression
	 *            Used while parsing properties. In this case ( e.g. parsing
	 *            "a/b") the property "a" ( as leftExpression of "/") is
	 *            relevant to verify whether the property "b" exists inside the
	 *            edm
	 * @return a CommonExpression
	 * @throws ExpressionParserException
	 * @throws ExpressionParserInternalError
	 * @throws TokenizerMessage
	 */
	protected CommonExpression readElement(final CommonExpression leftExpression,
			final ActualBinaryOperator leftOperator) throws ExpressionParserException, ExpressionParserInternalError {
		CommonExpression node = null;
		Token token;
		Token lookToken;
		lookToken = tokenList.lookToken();
		if (lookToken == null) {
			return null;
		}

		switch (lookToken.getKind()) {
		case OPENPAREN:
			node = readParenthesis();
			return node;
		case CLOSEPAREN: // ')' finishes a parenthesis (it is no extra token)" +
		case COMMA: // . " ',' is a separator for function parameters (it is no
					// extra token)" +
			return null;
		default:
			// continue
		}

		// -->Check if the token is a unary operator
		InfoUnaryOperator unaryOperator = isUnaryOperator(lookToken);
		if (unaryOperator != null) {
			return readUnaryoperator(lookToken, unaryOperator);
		}

		// ---expect the look ahead token
		token = tokenList.expectToken(lookToken.getUriLiteral(), true);
		lookToken = tokenList.lookToken();

		// -->Check if the token is a method
		// To avoid name clashes between method names and property names we
		// accept here only method names if a "(" follows.
		// Hence the parser accepts a property named "concat"
		InfoMethod methodOperator = isMethod(token, lookToken);
		if (methodOperator != null) {
			return readMethod(token, methodOperator);
		}

		// -->Check if token is a terminal
		// is a terminal e.g. a Value like an EDM.String 'hugo' or 125L or
		// 1.25D"
		if (token.getKind() == TokenKind.SIMPLE_TYPE) {
			LiteralExpression literal = new LiteralExpressionImpl(token.getUriLiteral(), token.getJavaLiteral());
			return literal;
		}

		// -->Check if token is a property, e.g. "name" or "address"
		if (token.getKind() == TokenKind.LITERAL) {
			PropertyExpressionImpl property = new PropertyExpressionImpl(token.getUriLiteral());
			// validateEdmProperty(leftExpression, property, token,
			// leftOperator);
			return property;
		}

		// not Tested, should not occur
		throw ExpressionParserInternalError.createCOMMON();
	}

	protected CommonExpression readUnaryoperator(final Token lookToken, final InfoUnaryOperator unaryOperator)
			throws ExpressionParserException, ExpressionParserInternalError {
		tokenList.expectToken(lookToken.getUriLiteral(), true);

		CommonExpression operand = readElement(null);
		UnaryExpression unaryExpression = new UnaryExpressionImpl(unaryOperator, operand);
		// validateUnaryOperatorTypes(unaryExpression); // throws
		// ExpressionInvalidOperatorTypeException

		return unaryExpression;
	}

	protected CommonExpression readMethod(final Token token, final InfoMethod methodOperator)
			throws ExpressionParserException, ExpressionParserInternalError {
		MethodExpressionImpl method = new MethodExpressionImpl(methodOperator);

		readParameters(methodOperator, method, token);
		// validateMethodTypes(method, token); // throws
		// ExpressionInvalidOperatorTypeException

		return method;
	}

	protected ActualBinaryOperator readBinaryOperator() {
		InfoBinaryOperator operator = null;
		Token token = tokenList.lookToken();
		if (token == null) {
			return null;
		}
		if ((token.getKind() == TokenKind.SYMBOL) && ("/".equals(token.getUriLiteral()))) {
			operator = availableBinaryOperators.get(token.getUriLiteral());
		} else if (token.getKind() == TokenKind.LITERAL) {
			operator = availableBinaryOperators.get(token.getUriLiteral());
		}

		if (operator == null) {
			return null;
		}

		return new ActualBinaryOperator(operator, token);
	}

	/**
	 * Check if a token is a UnaryOperator ( e.g. "not" or "-" )
	 * 
	 * @param token
	 *            Token to be checked
	 * 
	 * @return
	 *         <li>An instance of {@link InfoUnaryOperator} containing
	 *         information about the specific unary operator</li>
	 *         <li><code>null</code> if the token is not an unary operator</li>
	 */
	protected InfoUnaryOperator isUnaryOperator(final Token token) {
		if ((token.getKind() == TokenKind.LITERAL) || (token.getKind() == TokenKind.SYMBOL)) {
			InfoUnaryOperator operator = availableUnaryOperators.get(token.getUriLiteral());
			return operator;
		}
		return null;
	}

	protected InfoMethod isMethod(final Token token, final Token lookToken) {
		if ((lookToken != null) && (lookToken.getKind() == TokenKind.OPENPAREN)) {
			return availableMethods.get(token.getUriLiteral());
		}
		return null;
	}

	/**
	 * Check if the property name is the last or only element of the filter
	 * 
	 * @param propertyName
	 *            name of the property
	 * @return <code>true</code> if this is the last or only otherwise
	 *         <code>false</code>
	 */
	@SuppressWarnings("unused")
	private boolean isLastFilterElement(String propertyName) {
		return curExpression.contains(propertyName + " ");
	}

	static void initAvailTables() {
		Map<String, InfoBinaryOperator> lAvailableBinaryOperators = new HashMap<String, InfoBinaryOperator>();
		Map<String, InfoMethod> lAvailableMethods = new HashMap<String, InfoMethod>();
		Map<String, InfoUnaryOperator> lAvailableUnaryOperators = new HashMap<String, InfoUnaryOperator>();

		// ---Member member access---
		lAvailableBinaryOperators.put("/", new InfoBinaryOperator(BinaryOperator.PROPERTY_ACCESS, "Primary", 100));

		lAvailableBinaryOperators.put(BinaryOperator.MUL.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.MUL, "Multiplicative", 60));
		lAvailableBinaryOperators.put(BinaryOperator.DIV.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.DIV, "Multiplicative", 60));
		lAvailableBinaryOperators.put(BinaryOperator.MODULO.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.MODULO, "Multiplicative", 60));

		lAvailableBinaryOperators.put(BinaryOperator.ADD.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.ADD, "Additive", 50));
		lAvailableBinaryOperators.put(BinaryOperator.SUB.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.SUB, "Additive", 50));

		lAvailableBinaryOperators.put(BinaryOperator.LT.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.LT, "Relational", 40));
		lAvailableBinaryOperators.put(BinaryOperator.GT.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.GT, "Relational", 40));
		lAvailableBinaryOperators.put(BinaryOperator.GE.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.GE, "Relational", 40));
		lAvailableBinaryOperators.put(BinaryOperator.LE.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.LE, "Relational", 40));

		lAvailableBinaryOperators.put(BinaryOperator.EQ.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.EQ, "Equality", 30));
		lAvailableBinaryOperators.put(BinaryOperator.NE.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.NE, "Equality", 30));

		lAvailableBinaryOperators.put(BinaryOperator.AND.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.AND, "Conditional", 20));

		lAvailableBinaryOperators.put(BinaryOperator.OR.toUriLiteral(),
				new InfoBinaryOperator(BinaryOperator.OR, "Conditional", 10));

		lAvailableMethods.put(MethodOperator.ENDSWITH.toUriLiteral(), new InfoMethod(MethodOperator.ENDSWITH, 2, 2));

		lAvailableMethods.put(MethodOperator.INDEXOF.toUriLiteral(), new InfoMethod(MethodOperator.INDEXOF, 2, 2));
		
		lAvailableMethods.put(MethodOperator.EXISTS.toUriLiteral(), new InfoMethod(MethodOperator.EXISTS, 2, 2));

		lAvailableMethods.put(MethodOperator.STARTSWITH.toUriLiteral(),
				new InfoMethod(MethodOperator.STARTSWITH, 2, 2));

		lAvailableMethods.put(MethodOperator.TOLOWER.toUriLiteral(), new InfoMethod(MethodOperator.TOLOWER));

		lAvailableMethods.put(MethodOperator.TOUPPER.toUriLiteral(), new InfoMethod(MethodOperator.TOUPPER));

		lAvailableMethods.put(MethodOperator.TRIM.toUriLiteral(), new InfoMethod(MethodOperator.TRIM));

		lAvailableMethods.put(MethodOperator.SUBSTRING.toUriLiteral(), new InfoMethod(MethodOperator.SUBSTRING, 1, -1));

		lAvailableMethods.put(MethodOperator.SUBSTRINGOF.toUriLiteral(),
				new InfoMethod(MethodOperator.SUBSTRINGOF, 1, -1));

		// concat
		lAvailableMethods.put(MethodOperator.CONCAT.toUriLiteral(), new InfoMethod(MethodOperator.CONCAT, 2, -1));
		
		lAvailableMethods.put(MethodOperator.CONTAINS.toUriLiteral(), new InfoMethod(MethodOperator.CONTAINS, 2, -1));

		// length
		lAvailableMethods.put(MethodOperator.LENGTH.toUriLiteral(), new InfoMethod(MethodOperator.LENGTH));

		// year
		lAvailableMethods.put(MethodOperator.YEAR.toUriLiteral(), new InfoMethod(MethodOperator.YEAR));

		// month
		lAvailableMethods.put(MethodOperator.MONTH.toUriLiteral(), new InfoMethod(MethodOperator.MONTH));

		// day
		lAvailableMethods.put(MethodOperator.DAY.toUriLiteral(), new InfoMethod(MethodOperator.DAY));

		// hour
		lAvailableMethods.put(MethodOperator.HOUR.toUriLiteral(), new InfoMethod(MethodOperator.HOUR));

		// minute
		lAvailableMethods.put(MethodOperator.MINUTE.toUriLiteral(), new InfoMethod(MethodOperator.MINUTE));

		// second
		lAvailableMethods.put(MethodOperator.SECOND.toUriLiteral(), new InfoMethod(MethodOperator.SECOND));

		// round
		lAvailableMethods.put(MethodOperator.ROUND.toUriLiteral(), new InfoMethod(MethodOperator.ROUND));

		// ceiling
		lAvailableMethods.put(MethodOperator.CEILING.toUriLiteral(), new InfoMethod(MethodOperator.CEILING));

		// floor
		lAvailableMethods.put(MethodOperator.FLOOR.toUriLiteral(), new InfoMethod(MethodOperator.FLOOR));

		// ---unary---

		// minus
		lAvailableUnaryOperators.put(UnaryOperator.MINUS.toUriLiteral(),
				new InfoUnaryOperator(UnaryOperator.MINUS, "minus"));

		// not
		lAvailableUnaryOperators.put(UnaryOperator.NOT.toUriLiteral(), new InfoUnaryOperator(UnaryOperator.NOT, "not"));

		availableBinaryOperators = Collections.unmodifiableMap(lAvailableBinaryOperators);
		availableMethods = Collections.unmodifiableMap(lAvailableMethods);
		availableUnaryOperators = Collections.unmodifiableMap(lAvailableUnaryOperators);
	}
}
