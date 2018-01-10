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

import com.pogeyan.cmis.api.uri.expression.ExpressionKind;
import com.pogeyan.cmis.api.uri.expression.ExpressionVisitor;
import com.pogeyan.cmis.api.uri.expression.LiteralExpression;
import com.pogeyan.cmis.api.uri.expression.ObjectLiteral;

public class LiteralExpressionImpl implements LiteralExpression {

	private String uriLiteral;
	private ObjectLiteral objectLiteral;

	public LiteralExpressionImpl(final String uriLiteral, final ObjectLiteral objectLiteral) {
		this.uriLiteral = uriLiteral;
		this.objectLiteral = objectLiteral;
	}

	@Override
	public ExpressionKind getKind() {
		return ExpressionKind.LITERAL;
	}

	@Override
	public String getUriLiteral() {
		return uriLiteral;
	}

	@Override
	public Object accept(final ExpressionVisitor visitor) {
		Object ret = visitor.visitLiteral(this, this.objectLiteral);
		return ret;
	}
}
