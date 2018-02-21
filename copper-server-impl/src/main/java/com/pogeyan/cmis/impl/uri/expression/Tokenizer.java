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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.pogeyan.cmis.api.uri.exception.ObjectLiteralException;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.ObjectLiteral;
import com.pogeyan.cmis.api.uri.expression.ObjectLiteralTypeDetect;

/**
 * Expression tokenizer
 * 
 */
public class Tokenizer {

	private static final Pattern OTHER_LIT = Pattern.compile("(?:\\p{L}|\\p{Digit}|[-._~%!$&*+;:@])+");
	private static final Pattern FUNK = Pattern
			.compile("^(startswith|endswith|contains|substring|substring|substringof|indexof|replace|tolower|toupper"
					+ "|trim|concat|length|year|mounth|day|hour|minute|second|round|ceiling|floor)( *)\\(");
	private static final Pattern AND_SUB1 = Pattern.compile("^(add|sub|mul|div|mod|not) ");
	private static final Pattern AND_SUB = Pattern.compile("^(and|or|eq|ne|lt|gt|le|ge) ");
	private static final Pattern prefix = Pattern.compile("^(X|binary|guid|datetime|datetimeoffset|time)'");
	private boolean flagIncludeWhitespace = false;

	int curPosition;
	final String expression;
	final int expressionLength;
	TokenList tokens;

	public Tokenizer(final String expression) {
		this.expression = expression;
		expressionLength = expression.length();
		tokens = new TokenList();
	}

	/**
	 * Inform the Tokenizer whether extra tokens for whitespace characters
	 * should be added to the token list or not.
	 * 
	 * @param flagIncludeWhitespace
	 *            True -> Whitespace token will be added to token list; False
	 *            otherwise
	 * @return this
	 */
	public Tokenizer setFlagWhiteSpace(final Boolean flagIncludeWhitespace) {
		this.flagIncludeWhitespace = flagIncludeWhitespace;
		return this;
	}

	/**
	 * Tokenizes an expression as defined per OData specification
	 * 
	 * @return Token list
	 */
	public TokenList tokenize() throws TokenizerException, ExpressionParserException {
		curPosition = 0;
		int oldPosition;
		char curCharacter;
		String token = "";

		while (curPosition < expressionLength) {
			oldPosition = curPosition;

			curCharacter = expression.charAt(curPosition);
			switch (curCharacter) {
			case ' ':
				// count whitespace and move pointer to next non-whitespace char
				eatWhiteSpaces(curPosition, curCharacter);
				break;

			case '(':
				tokens.appendToken(curPosition, TokenKind.OPENPAREN, curCharacter);
				curPosition = curPosition + 1;

				break;

			case ')':
				tokens.appendToken(curPosition, TokenKind.CLOSEPAREN, curCharacter);
				curPosition = curPosition + 1;
				break;

			case '\'':
				token = "";
				readLiteral(curCharacter);

				break;

			case ',':
				tokens.appendToken(oldPosition, TokenKind.COMMA, curCharacter);
				curPosition = curPosition + 1;
				break;

			case '=':
			case '/':
			case '?':
			case '.':
			case '*':
				curPosition = curPosition + 1;
				tokens.appendToken(oldPosition, TokenKind.SYMBOL, curCharacter);
				break;

			default:
				String rem_expr = expression.substring(curPosition); // remaining
																		// expression

				boolean isBinary = checkForBinary(oldPosition, rem_expr);
				if (isBinary) {
					break;
				}

				// check for prefixes like X, binary, guid, datetime
				boolean isPrefix = checkForPrefix(rem_expr);
				if (isPrefix) {
					break;
				}

				// check for math
				boolean isMath = checkForMath(oldPosition, rem_expr);
				if (isMath) {
					break;
				}

				// check for function
				boolean isFunction = checkForMethod(oldPosition, rem_expr);
				if (isFunction) {
					break;
				}
				
				// Bug->If we send rightLiteral boolean value,it won't add in
				// token map because if it is boolean value,it will return
				// empty.
				// boolean isBoolean = checkForBoolean(oldPosition, rem_expr);
				// if (isBoolean) {
				// break;
				// }

				boolean isLiteral = checkForLiteral(oldPosition, curCharacter, rem_expr);
				if (isLiteral) {
					break;
				}

				token = new Character(curCharacter).toString();
				throw TokenizerException.createUNKNOWN_CHARACTER(oldPosition, token, expression);
			}
		}
		return tokens;
	}

	private boolean checkForLiteral(final int oldPosition, final char curCharacter, final String rem_expr) {
		final Matcher matcher = OTHER_LIT.matcher(rem_expr);
		boolean isLiteral = false;
		if (matcher.lookingAt()) {
			String token = matcher.group();
			try {
				ObjectLiteral edmLiteral = ObjectLiteralTypeDetect.parseUriLiteral(token);
				curPosition = curPosition + token.length();
				// It is a simple type.
				tokens.appendEdmTypedToken(oldPosition, TokenKind.SIMPLE_TYPE, token, edmLiteral);
				isLiteral = true;
			} catch (ObjectLiteralException e) {
				// We treat it as normal untyped literal.

				// The '-' is checked here (and not in the switch statement)
				// because it may be
				// part of a negative number.
				if (curCharacter == '-') {
					curPosition = curPosition + 1;
					tokens.appendToken(oldPosition, TokenKind.SYMBOL, curCharacter);
					isLiteral = true;
				} else {
					curPosition = curPosition + token.length();
					tokens.appendToken(oldPosition, TokenKind.LITERAL, token);
					isLiteral = true;
				}
			}
		}
		return isLiteral;
	}

	private boolean checkForBoolean(final int oldPosition, final String rem_expr) {
		boolean isBoolean = false;
		if ("true".equals(rem_expr) || "false".equals(rem_expr)) {
			curPosition = curPosition + rem_expr.length();
			// tokens.appendEdmTypedToken(oldPosition, TokenKind.SIMPLE_TYPE,
			// rem_expr, new EdmLiteral(EdmSimpleTypeFacadeImpl
			// .getEdmSimpleType(EdmSimpleTypeKind.Boolean), rem_expr));
			isBoolean = true;
		}
		return isBoolean;
	}

	@SuppressWarnings("unused")
	private void eatWhiteSpaces(final int oldPosition, char curCharacter) {
		int lv_token_len;
		String expression_sub;
		while ((curCharacter == ' ') && (curPosition < expressionLength)) {
			curPosition = curPosition + 1;
			if (curPosition < expressionLength) {
				curCharacter = expression.charAt(curPosition);
			}
		}

		lv_token_len = curPosition - oldPosition;

		if (flagIncludeWhitespace == true) {
			expression_sub = expression.substring(oldPosition, oldPosition + lv_token_len);
			// tokens.appendEdmTypedToken(oldPosition, TokenKind.WHITESPACE,
			// expression_sub, null);
		}
	}

	private boolean checkForMethod(final int oldPosition, final String rem_expr) {
		boolean isMethod = false;
		Matcher matcher = FUNK.matcher(rem_expr);
		if (matcher.find()) {
			String token = matcher.group(1);
			curPosition = curPosition + token.length();
			tokens.appendToken(oldPosition, TokenKind.LITERAL, token);
			isMethod = true;
		}
		return isMethod;
	}

	private boolean checkForMath(final int oldPosition, final String rem_expr) {
		boolean isMath = false;
		Matcher matcher1 = AND_SUB1.matcher(rem_expr);
		if (matcher1.find()) {
			String token = matcher1.group(1);
			curPosition = curPosition + token.length();
			tokens.appendToken(oldPosition, TokenKind.LITERAL, token);
			isMath = true;
		}
		return isMath;
	}

	private boolean checkForBinary(final int oldPosition, final String rem_expr) {
		boolean isBinary = false;
		Matcher matcher1 = AND_SUB.matcher(rem_expr);
		if (matcher1.find()) {
			String token = matcher1.group(1);
			curPosition = curPosition + token.length();
			tokens.appendToken(oldPosition, TokenKind.LITERAL, token);
			isBinary = true;
		}
		return isBinary;
	}

	private boolean checkForPrefix(final String rem_expr) throws ExpressionParserException, TokenizerException {
		boolean isPrefix = false;
		Matcher matcher = prefix.matcher(rem_expr);
		String token = "";
		char curCharacter;

		if (matcher.find()) {
			token = matcher.group(1);
			curPosition = curPosition + token.length();
			curCharacter = expression.charAt(curPosition); // "should be '
			readLiteral(curCharacter, token);
			isPrefix = true;
		}
		return isPrefix;
	}

	private void readLiteral(final char curCharacter) throws ExpressionParserException, TokenizerException {
		readLiteral(curCharacter, "");
	}

	/**
	 * Read up to single ' and move pointer to the following char and tries a
	 * type detection
	 * 
	 * @param curCharacter
	 * @param token
	 * @throws Exception
	 */
	private void readLiteral(char curCharacter, String token) throws TokenizerException {
		int offsetPos = -token.length();
		int oldPosition = curPosition;
		token = token + Character.toString(curCharacter);
		curPosition = curPosition + 1;

		boolean wasApostroph = false; // leading ' does not count
		while (curPosition < expressionLength) {
			curCharacter = expression.charAt(curPosition);

			if (curCharacter != '\'') {
				if (wasApostroph == true) {
					break;
				}

				token = token + curCharacter;
				wasApostroph = false;
			} else {
				if (wasApostroph) {
					wasApostroph = false; // a double ' is a normal character '
				} else {
					wasApostroph = true;
				}
				token = token + curCharacter;
			}
			curPosition = curPosition + 1;
		}

		if (!wasApostroph) {
			// Exception tested within TestPMparseFilterString
			throw FilterParserExceptionImpl.createTOKEN_UNDETERMINATED_STRING(oldPosition, expression);
		}

		try {
			ObjectLiteral edmLiteral = ObjectLiteralTypeDetect.parseUriLiteral(token);
			tokens.appendEdmTypedToken(oldPosition + offsetPos, TokenKind.SIMPLE_TYPE, token, edmLiteral);
		} catch (ObjectLiteralException ex) {
			throw TokenizerException.createTYPEDECTECTION_FAILED_ON_STRING(ex, oldPosition, token);
		}
	}
}
