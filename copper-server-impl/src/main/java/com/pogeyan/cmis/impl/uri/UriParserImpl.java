
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
package com.pogeyan.cmis.impl.uri;

import com.pogeyan.cmis.api.uri.UriParser;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;
import com.pogeyan.cmis.impl.uri.expression.FilterParserImpl;
import com.pogeyan.cmis.impl.uri.expression.OrderByParserImpl;

/**
 * Parser for the OData part of the URL.
 * 
 */
public class UriParserImpl extends UriParser {
	public UriParserImpl() {
	}

	@Override
	public FilterExpression parseFilterString(final String expression) throws ExpressionParserException {
		return new FilterParserImpl().parseFilterString(expression);
	}

	@Override
	public OrderByExpression parseOrderByString(final String expression) throws ExpressionParserException {
		return new OrderByParserImpl().parseOrderByString(expression);
	}
}
