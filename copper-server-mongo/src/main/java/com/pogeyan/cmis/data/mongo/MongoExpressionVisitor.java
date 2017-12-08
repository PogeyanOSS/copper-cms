package com.pogeyan.cmis.data.mongo;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.olingo.odata2.api.edm.EdmLiteral;
import org.apache.olingo.odata2.api.edm.EdmSimpleTypeKind;
import org.apache.olingo.odata2.api.edm.EdmTyped;
import org.apache.olingo.odata2.api.uri.expression.BinaryExpression;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.ExpressionVisitor;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.apache.olingo.odata2.api.uri.expression.LiteralExpression;
import org.apache.olingo.odata2.api.uri.expression.MemberExpression;
import org.apache.olingo.odata2.api.uri.expression.MethodExpression;
import org.apache.olingo.odata2.api.uri.expression.MethodOperator;
import org.apache.olingo.odata2.api.uri.expression.OrderByExpression;
import org.apache.olingo.odata2.api.uri.expression.OrderExpression;
import org.apache.olingo.odata2.api.uri.expression.PropertyExpression;
import org.apache.olingo.odata2.api.uri.expression.SortOrder;
import org.apache.olingo.odata2.api.uri.expression.UnaryExpression;
import org.apache.olingo.odata2.api.uri.expression.UnaryOperator;

import org.mongodb.morphia.query.Criteria;
import org.mongodb.morphia.query.Query;

public class MongoExpressionVisitor<T> implements ExpressionVisitor {
	private Query<T> query;

	public MongoExpressionVisitor(Query<T> query) {
		this.query = query;
	}

	@Override
	public Object visitFilterExpression(FilterExpression filterExpression, String expressionString, Object expression) {
		return expression;
	}

	@Override
	public Object visitBinary(BinaryExpression binaryExpression, BinaryOperator operator, Object leftSide,
			Object rightSide) {
		if (leftSide instanceof BinaryExpression) {
			// If something is lower in the tree and is of the type AND or OR it
			// needs brackets to show the higher priority
			if (BinaryOperator.AND.equals(((BinaryExpression) binaryExpression).getOperator())
					|| BinaryOperator.OR.equals(((BinaryExpression) binaryExpression).getOperator())) {

			}
		} else if (leftSide instanceof PropertyExpression) {

			PropertyExpression leftOp = (PropertyExpression) leftSide;
			switch (operator) {
			case EQ:
				return this.query.criteria(leftOp.getUriLiteral()).equal(rightSide);
			case NE:
				return this.query.criteria(leftOp.getUriLiteral()).notEqual(rightSide);
			case GE:
				return this.query.criteria(leftOp.getUriLiteral()).greaterThanOrEq(rightSide);
			case GT:
				return this.query.criteria(leftOp.getUriLiteral()).greaterThan(rightSide);
			case LE:
				return this.query.criteria(leftOp.getUriLiteral()).lessThanOrEq(rightSide);
			case LT:
				return this.query.criteria(leftOp.getUriLiteral()).lessThan(rightSide);
			default:
				// Other operators are not supported for SQL Statements
				throw new UnsupportedOperationException("Unsupported operator: " + operator.toUriLiteral());
			}
		} else {
			ArrayList<Criteria> operands = new ArrayList<>();
			operands.add((Criteria) leftSide);
			operands.add((Criteria) rightSide);
			switch (operator) {
			case OR:
				this.query.or(operands.toArray(new Criteria[operands.size()]));
				break;
			case AND:
				this.query.and(operands.toArray(new Criteria[operands.size()]));
				break;
			default:
				// Other operators are not supported for SQL Statements
				throw new UnsupportedOperationException("Unsupported operator: " + operator.toUriLiteral());
			}
		}

		// return the binary statement
		return this.query;
	}

	@Override
	public Object visitOrderByExpression(OrderByExpression orderByExpression, String expressionString,
			List<Object> orders) {
		String orderByQuery = orders.stream().filter(t -> t != null).map(n -> n.toString())
				.collect(Collectors.joining(","));
		return this.query.order(orderByQuery);
	}

	@Override
	public Object visitOrder(OrderExpression orderExpression, Object filterResult, SortOrder sortOrder) {
		if (filterResult instanceof PropertyExpression) {
			PropertyExpression orderByProperty = (PropertyExpression) filterResult;
			String orderByLiteral = sortOrder == SortOrder.asc ? orderByProperty.getUriLiteral()
					: "-" + orderByProperty.getUriLiteral();
			// morphia maintains an internal sort representation
			return orderByLiteral;
		}

		return null;
	}

	@Override
	public Object visitLiteral(LiteralExpression literal, EdmLiteral edmLiteral) {
		if (EdmSimpleTypeKind.String.getEdmSimpleTypeInstance().equals(edmLiteral.getType())) {
			return "'" + edmLiteral.getLiteral() + "'";
		} else {
			return "'" + edmLiteral.getLiteral() + "'";
		}
	}

	@Override
	public Object visitMethod(MethodExpression methodExpression, MethodOperator method, List<Object> parameters) {
		if (parameters.size() != 2) {
			throw new UnsupportedOperationException("Unsupported parameters length, it should be 2");
		}

		PropertyExpression fieldOperand = (PropertyExpression) parameters.get(0);
		String fieldValue = parameters.get(1).toString();
		fieldValue = fieldValue.replaceAll("\'","");
		switch (method) {
		case STARTSWITH:
			Pattern sw_pattern = Pattern.compile("^" + Pattern.quote(fieldValue), Pattern.CASE_INSENSITIVE);
			return this.query.filter(fieldOperand.getUriLiteral(), sw_pattern);
		case ENDSWITH:
			Pattern ew_pattern = Pattern.compile(Pattern.quote(fieldValue) + "$", Pattern.CASE_INSENSITIVE);
			return this.query.filter(fieldOperand.getPropertyName(), ew_pattern);
		default:
			// Other operators are not supported for SQL Statements
			throw new UnsupportedOperationException("Unsupported operator: " + method.toUriLiteral());
		}
	}

	@Override
	public Object visitMember(MemberExpression memberExpression, Object path, Object property) {
		return null;
	}

	@Override
	public Object visitProperty(PropertyExpression propertyExpression, String uriLiteral, EdmTyped edmProperty) {
		return propertyExpression;
	}

	@Override
	public Object visitUnary(UnaryExpression unaryExpression, UnaryOperator operator, Object operand) {
		return null;
	}
}
