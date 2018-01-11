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

import com.pogeyan.cmis.api.uri.expression.ObjectLiteral;

/* 1 */

public class Token {

	private TokenKind kind;
	private int position;
	private String uriLiteral;
	private ObjectLiteral objectLiteral;

	public Token(final TokenKind kind, final int position, final String uriLiteral, final ObjectLiteral objectLiteral) {
		this.kind = kind;
		this.position = position;
		this.uriLiteral = uriLiteral;
		this.objectLiteral = objectLiteral;
	}
	
	public Token(final TokenKind kind, final int position, final String uriLiteral) {
	    this.kind = kind;
	    this.position = position;
	    this.uriLiteral = uriLiteral;
	    this.objectLiteral = null;
	  }

	public TokenKind getKind() {
		return kind;
	}

	public int getPosition() {
		return position;
	}

	public String getUriLiteral() {
		return uriLiteral;
	}

	public ObjectLiteral getJavaLiteral() {
		return this.objectLiteral;
	}
}
