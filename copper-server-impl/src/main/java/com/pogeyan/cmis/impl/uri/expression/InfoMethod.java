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

import com.pogeyan.cmis.api.uri.expression.MethodOperator;

/**
 * Describes a method expression which is allowed in OData expressions
 * 
 */
class InfoMethod {

	public MethodOperator method;
	public String syntax;
	public int minParameter;
	public int maxParameter;

	public InfoMethod(final MethodOperator method) {
		this.method = method;
		syntax = method.toUriLiteral();
		minParameter = 1;
		maxParameter = 1;
	}

	public InfoMethod(final MethodOperator method, final int minParameters, final int maxParameters) {
		this.method = method;
		syntax = method.toUriLiteral();
		minParameter = minParameters;
		maxParameter = maxParameters;
	}

	public InfoMethod(final MethodOperator method, final String string, final int minParameters,
			final int maxParameters) {
		this.method = method;
		syntax = string;
		minParameter = minParameters;
		maxParameter = maxParameters;
	}

	public MethodOperator getMethod() {
		return method;
	}

	public String getSyntax() {
		return syntax;
	}

	public int getMinParameter() {
		return minParameter;
	}

	public int getMaxParameter() {
		return maxParameter;
	}
}
