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

import java.util.List;

import com.pogeyan.cmis.api.uri.expression.BinaryOperator;

/**
 * Describes a binary operator which is allowed in OData expressions
 * 
 */
class InfoBinaryOperator {
	private BinaryOperator operator;
	private String category;
	private String syntax;
	private int priority;

	public InfoBinaryOperator(final BinaryOperator operator, final String category, final int priority) {
		this.operator = operator;
		this.category = category;
		syntax = operator.toUriLiteral();
		this.priority = priority;
	}

	public String getCategory() {
		return category;
	}

	public String getSyntax() {
		return syntax;
	}

	public BinaryOperator getOperator() {
		return operator;
	}

	public int getPriority() {
		return priority;
	}
}
