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

import java.io.IOException;
import java.io.StringWriter;
import java.util.List;

import com.pogeyan.cmis.api.uri.expression.BinaryExpression;
import com.pogeyan.cmis.api.uri.expression.BinaryOperator;
import com.pogeyan.cmis.api.uri.expression.ExpressionVisitor;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;
import com.pogeyan.cmis.api.uri.expression.LiteralExpression;
import com.pogeyan.cmis.api.uri.expression.MemberExpression;
import com.pogeyan.cmis.api.uri.expression.MethodExpression;
import com.pogeyan.cmis.api.uri.expression.MethodOperator;
import com.pogeyan.cmis.api.uri.expression.ObjectLiteral;
import com.pogeyan.cmis.api.uri.expression.OrderByExpression;
import com.pogeyan.cmis.api.uri.expression.OrderExpression;
import com.pogeyan.cmis.api.uri.expression.PropertyExpression;
import com.pogeyan.cmis.api.uri.expression.SortOrder;
import com.pogeyan.cmis.api.uri.expression.UnaryExpression;
import com.pogeyan.cmis.api.uri.expression.UnaryOperator;

/**
 *  
 */
public class JsonVisitor implements ExpressionVisitor {

  @Override
  public Object visitFilterExpression(final FilterExpression filterExpression, final String expressionString,
      final Object expression) {
    return expression;
  }

  @Override
  public Object visitBinary(final BinaryExpression binaryExpression, final BinaryOperator operator,
      final Object leftSide, final Object rightSide) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", binaryExpression.getKind().toString()).separator()
          .namedStringValue("operator", operator.toUriLiteral()).separator().namedStringValueRaw("type",
              null).separator().name("left").unquotedValue(leftSide.toString()).separator().name(
              "right").unquotedValue(rightSide.toString()).endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }

  @Override
  public Object visitOrderByExpression(final OrderByExpression orderByExpression, final String expressionString,
      final List<Object> orders) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", "order collection").separator().name("orders")
          .beginArray();
      boolean first = true;
      for (final Object order : orders) {
        if (first) {
          first = false;
        } else {
          jsonStreamWriter.separator();
        }
        jsonStreamWriter.unquotedValue(order.toString());
      }
      jsonStreamWriter.endArray().endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }

  @Override
  public Object visitOrder(final OrderExpression orderExpression, final Object filterResult,
      final SortOrder sortOrder) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", orderExpression.getKind().toString()).separator()
          .namedStringValueRaw("sortorder", sortOrder.toString()).separator().name("expression").unquotedValue(
              filterResult.toString()).endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }

  @Override
  public Object visitLiteral(final LiteralExpression literal, ObjectLiteral objectLiteral) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", literal.getKind().toString()).separator()
          .namedStringValueRaw("type", null)
          .endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }

  @Override
  public Object visitMethod(final MethodExpression methodExpression, final MethodOperator method,
      final List<Object> parameters) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", methodExpression.getKind().toString()).separator()
          .namedStringValueRaw("operator", method.toUriLiteral()).separator().namedStringValueRaw("type",
              null).separator().name("parameters").beginArray();
      boolean first = true;
      for (Object parameter : parameters) {
        if (first) {
          first = false;
        } else {
          jsonStreamWriter.separator();
        }
        jsonStreamWriter.unquotedValue(parameter.toString());
      }
      jsonStreamWriter.endArray().endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }

  @Override
  public Object visitMember(final MemberExpression memberExpression, final Object path, final Object property) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", memberExpression.getKind().toString()).separator()
          .namedStringValueRaw("type", null).separator().name("source").unquotedValue(
              path.toString()).separator().name("path").unquotedValue(property.toString()).endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }

  @Override
  public Object visitProperty(final PropertyExpression propertyExpression, final String uriLiteral) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", propertyExpression.getKind().toString())
          .separator().namedStringValue("name", uriLiteral).separator().namedStringValueRaw("type",
              null).endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }

  @Override
  public Object visitUnary(final UnaryExpression unaryExpression, final UnaryOperator operator, final Object operand) {
    try {
      StringWriter writer = new StringWriter();
      JsonStreamWriter jsonStreamWriter = new JsonStreamWriter(writer);
      jsonStreamWriter.beginObject().namedStringValueRaw("nodeType", unaryExpression.getKind().toString()).separator()
          .namedStringValueRaw("operator", operator.toUriLiteral()).separator().namedStringValueRaw("type",
              null).separator().name("operand").unquotedValue(operand.toString()).endObject();
      writer.flush();
      return writer.toString();
    } catch (final IOException e) {
      return null;
    }
  }
}
