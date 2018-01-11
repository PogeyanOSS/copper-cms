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
package com.pogeyan.cmis.api.uri;

import com.pogeyan.cmis.api.uri.RuntimeDelegate;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;

/**
 * Wrapper for UriParser functionality.
 * 
 */
public abstract class UriParser {
  
  /**
   * Parses a $filter expression string and create an expression tree.
   * <p>The current expression parser supports expressions as defined in the
   * OData specification 2.0 with the following restrictions:
   * <ul>
   * <li>the methods "cast", "isof" and "replace" are not supported</li>
   * </ul></p>
   * 
   * <p>The expression parser can be used with providing an Entity Data Model (EDM)
   * and without providing it. When an EDM is provided the expression parser will be
   * as strict as possible. That means:
   * <ul>
   * <li>All properties used in the expression must be defined inside the EDM,</li>
   * <li>the types of EDM properties will be checked against the lists of allowed
   * types per method and per binary or unary operator, respectively</li>
   * </ul>
   * If no EDM is provided the expression parser performs a lax validation:
   * <ul>
   * <li>The properties used in the expression are not looked up inside the EDM
   * and the type of the expression node representing the property will be "null",</li>
   * <li>expression nodes with EDM type "null" are not considered during the parameter
   * type validation, so the return type of the parent expression node will
   * also become "null".</li>
   * </ul>
   * @param edm entity data model of the accessed OData service
   * @param edmType EDM type of the OData entity/complex type/... addressed by the URL
   * @param expression $filter expression string to be parsed
   * @return expression tree which can be traversed with help of the interfaces
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.ExpressionVisitor ExpressionVisitor} and
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.Visitable Visitable}
   * @throws ExpressionParserException thrown due to errors while parsing the $filter expression string
   * @throws ODataMessageException for extensibility
   */
  public static FilterExpression parseFilter(final String expression)
      throws ExpressionParserException {
    return RuntimeDelegate.getUriParserInstance().parseFilterString(expression);
  }

  /**
   * Parses a $filter expression string and create an expression tree.
   * <p>The current expression parser supports expressions as defined in the
   * OData specification 2.0 with the following restrictions:
   * <ul>
   * <li>the methods "cast", "isof" and "replace" are not supported</li>
   * </ul></p>
   * 
   * <p>The expression parser can be used with providing an Entity Data Model (EDM)
   * and without providing it. When an EDM is provided the expression parser will be
   * as strict as possible. That means:
   * <ul>
   * <li>All properties used in the expression must be defined inside the EDM,</li>
   * <li>the types of EDM properties will be checked against the lists of allowed
   * types per method and per binary or unary operator, respectively</li>
   * </ul>
   * If no EDM is provided the expression parser performs a lax validation:
   * <ul>
   * <li>The properties used in the expression are not looked up inside the EDM
   * and the type of the expression node representing the property will be "null",</li>
   * <li>expression nodes with EDM type "null" are not considered during the parameter
   * type validation, so the return type of the parent expression node will
   * also become "null".</li>
   * </ul>
   * @param edmType EDM type of the OData entity/complex type/... addressed by the URL
   * @param expression $filter expression string to be parsed
   * @return expression tree which can be traversed with help of the interfaces
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.ExpressionVisitor ExpressionVisitor} and
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.Visitable Visitable}
   * @throws ExpressionParserException thrown due to errors while parsing the $filter expression string
   * @throws ODataMessageException for extensibility
   */
  public abstract FilterExpression parseFilterString(String expression)
		  throws ExpressionParserException;

  /**
   * Parses a $orderby expression string and creates an expression tree.
   * @param edm EDM model of the accessed OData service
   * @param edmType EDM type of the OData entity/complex type/... addressed by the URL
   * @param expression $orderby expression string to be parsed
   * @return expression tree which can be traversed with help of the interfaces
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.ExpressionVisitor ExpressionVisitor} and
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.Visitable Visitable}
   * @throws ExpressionParserException thrown due to errors while parsing the $orderby expression string
   * @throws ODataMessageException used for extensibility
   */
  public static OrderByExpression parseOrderBy(final String expression)
      throws ExpressionParserException {
    return RuntimeDelegate.getUriParserInstance().parseOrderByString(expression);
  }

  /**
   * Parses a $orderby expression string and creates an expression tree.
   * @param edmType EDM type of the OData entity/complex type/... addressed by the URL
   * @param expression $orderby expression string to be parsed
   * @return expression tree which can be traversed with help of the interfaces
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.ExpressionVisitor ExpressionVisitor} and
   * {@link com.pogeyan.cmis.impl.uri.uri.expression.Visitable Visitable}
   * @throws ExpressionParserException thrown due to errors while parsing the $orderby expression string
   * @throws ODataMessageException used for extensibility
   */
  public abstract OrderByExpression parseOrderByString(String expression)
      throws ExpressionParserException;
}
