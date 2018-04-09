package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;

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
import com.pogeyan.cmis.impl.utils.DBUtils;

public class JDOExpressionVisitor implements ExpressionVisitor {
	private static List<String> fieldExpression = new ArrayList<>();
	private static List<String> expression = new ArrayList<>();
	private static Map<String, String> operatorExtension = new HashMap<>();
	private TypeDefinition type = null;

	public JDOExpressionVisitor(String repositoryId, String typeId) {
		List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId, Arrays.asList(typeId));
		if (typeDef != null && typeDef.size() > 0) {
			type = typeDef.get(0);
		}
		operatorExtension.put(BinaryOperator.ADD.name(), "&&");
		operatorExtension.put(BinaryOperator.OR.name(), "||");
	}

	@Override
	public Object visitFilterExpression(FilterExpression filterExpression, String expressionString, Object expression) {
		return expression;
	}

	@Override
	public Object visitBinary(BinaryExpression binaryExpression, BinaryOperator operator, Object leftSide,
			Object rightSide) {
		if (leftSide instanceof BinaryExpression) {
			if (BinaryOperator.AND.equals(((BinaryExpression) binaryExpression).getOperator())
					|| BinaryOperator.OR.equals(((BinaryExpression) binaryExpression).getOperator())) {

			}
		} else if (leftSide instanceof PropertyExpression) {
			PropertyExpression leftOp = (PropertyExpression) leftSide;
			String rightSideValue = null;
			if (rightSide instanceof PropertyExpression) {
				PropertyExpression rightOp = (PropertyExpression) rightSide;
				rightSideValue = rightOp.getUriLiteral();
			} else if (rightSide instanceof LiteralExpression) {
				LiteralExpression rightOp = (LiteralExpression) rightSide;
				rightSideValue = rightOp.getUriLiteral();
			} else if (rightSide instanceof String) {
				rightSideValue = rightSide.toString();
			}
			switch (operator) {
			case EQ:
				fieldExpression.add("this." + getQueryName(leftOp.getUriLiteral()) + " == "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue));
				return getQueryName("this." + leftOp.getUriLiteral()) + " == "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue);
			case NE:
				fieldExpression.add("this." + getQueryName(leftOp.getUriLiteral()) + " != "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue));
				return getQueryName("this." + leftOp.getUriLiteral()) + " != "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue);
			case GE:
				fieldExpression.add("this." + getQueryName(leftOp.getUriLiteral()) + " <= "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue));
				return getQueryName("this." + leftOp.getUriLiteral()) + " <= "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue);
			case GT:
				fieldExpression.add("this." + getQueryName(leftOp.getUriLiteral()) + " < "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue));
				return getQueryName("this." + leftOp.getUriLiteral()) + " < "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue);
			case LE:
				fieldExpression.add("this." + getQueryName(leftOp.getUriLiteral()) + " >= "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue));
				return getQueryName("this." + leftOp.getUriLiteral()) + " >= "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue);
			case LT:
				fieldExpression.add("this." + getQueryName(leftOp.getUriLiteral()) + " > "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue));
				return getQueryName("this." + leftOp.getUriLiteral()) + " > "
						+ getPropertyValue(leftOp.getUriLiteral(), rightSideValue);
			default:
				// Other operators are not supported for SQL Statements
				throw new UnsupportedOperationException("Unsupported operator: " + operator.toUriLiteral());
			}
		} else {
			switch (operator) {
			case OR:
				String orExpression = fieldExpression.stream().collect(Collectors.joining(" || "));
				fieldExpression = new ArrayList<>();
				return getExpression(orExpression, BinaryOperator.OR.name());
			case AND:
				String addExpression = fieldExpression.stream().collect(Collectors.joining(" && "));
				fieldExpression = new ArrayList<>();
				return getExpression(addExpression, BinaryOperator.ADD.name());
			default:
				// Other operators are not supported for SQL Statements
				throw new UnsupportedOperationException("Unsupported operator: " + operator.toUriLiteral());
			}
		}
		return null;
	}

	@Override
	public Object visitOrderByExpression(OrderByExpression orderByExpression, String expressionString,
			List<Object> orders) {
		return null;
	}

	@Override
	public Object visitOrder(OrderExpression orderExpression, Object filterResult, SortOrder sortOrder) {
		return null;
	}

	@Override
	public Object visitLiteral(LiteralExpression literal, ObjectLiteral objectLiteral) {
		return objectLiteral.getLiteral();
	}

	@Override
	public Object visitMethod(MethodExpression methodExpression, MethodOperator method, List<Object> parameters) {

		PropertyExpression fieldOperand = (PropertyExpression) parameters.get(0);
		String fieldValue = parameters.get(1).toString();
		fieldValue = fieldValue.replaceAll("\'", "");
		switch (method) {
		case STARTSWITH:
			fieldExpression.add("this." + getQueryName(fieldOperand.getUriLiteral()) + ".startsWith("
					+ getPropertyValue(fieldOperand.getUriLiteral(), fieldValue) + ")");
			return "this." + getQueryName(fieldOperand.getUriLiteral()) + ".startsWith("
					+ getPropertyValue(fieldOperand.getUriLiteral(), fieldValue) + ")";
		case ENDSWITH:
			fieldExpression.add("this." + getQueryName(fieldOperand.getUriLiteral()) + ".endsWith("
					+ getPropertyValue(fieldOperand.getUriLiteral(), fieldValue) + ")");
			return "this." + getQueryName(fieldOperand.getUriLiteral()) + ".endsWith("
					+ getPropertyValue(fieldOperand.getUriLiteral(), fieldValue) + ")";
		case CONTAINS:
			fieldExpression.add("this." + getQueryName(fieldOperand.getUriLiteral()) + ".matches('.*"
					+ getPropertyValue(fieldOperand.getUriLiteral(), fieldValue) + ".*')");
			return fieldExpression.add("this." + getQueryName(fieldOperand.getUriLiteral()) + ".matches('.*"
					+ getPropertyValue(fieldOperand.getUriLiteral(), fieldValue) + ".*')");
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
	public Object visitProperty(PropertyExpression propertyExpression, String uriLiteral) {
		return propertyExpression;
	}

	@Override
	public Object visitUnary(UnaryExpression unaryExpression, UnaryOperator operator, Object operand) {
		return null;
	}

	private static String getQueryName(String name) {
		if (name.equalsIgnoreCase("cmis:path") || name.equalsIgnoreCase("cmis:description")
				|| name.equalsIgnoreCase("cmis:parentId") || name.equalsIgnoreCase("cmis:contentStreamLength")
				|| name.equalsIgnoreCase("cmis:contentStreamFileName")
				|| name.equalsIgnoreCase("cmis:contentStreamMimeType") || name.equalsIgnoreCase("cmis:checkinComment")
				|| name.equalsIgnoreCase("cmis:versionLabel") || name.equalsIgnoreCase("cmis:isMajorVersion")
				|| name.equalsIgnoreCase("cmis:isLatestVersion") || name.equalsIgnoreCase("cmis:isLatestMajorVersion")
				|| name.equalsIgnoreCase("cmis:name") || name.equalsIgnoreCase("cmis:isPrivateWorkingCopy")
				|| name.equalsIgnoreCase("cmis:createdBy") || name.equalsIgnoreCase("cmis:contentStreamId")
				|| name.equalsIgnoreCase("cmis:versionSeriesCheckedOutId")
				|| name.equalsIgnoreCase("cmis:versionSeriesId")
				|| name.equalsIgnoreCase("cmis:isVersionSeriesCheckedOut") || name.equalsIgnoreCase("cmis:isImmutable")
				|| name.equalsIgnoreCase("cmis:modifiedBy")
				|| name.equalsIgnoreCase("cmis:versionSeriesCheckedOutBy")) {
			return getFieldName(name);
		} else if (name.equalsIgnoreCase("cmis:objectId")) {
			return "id";
		} else if (name.equalsIgnoreCase("cmis:secondaryObjectTypeIds")) {
			return "secondaryTypeIds";
		} else if (name.equalsIgnoreCase("cmis:objectTypeId")) {
			return "typeId";
		} else if (name.equalsIgnoreCase("cmis:lastModifiedBy")) {
			return "modifiedBy";
		} else if (name.equalsIgnoreCase("cmis:creationDate")) {
			return "createdAt";
		} else if (name.equalsIgnoreCase("cmis:changeToken")) {
			return "token";
		} else if (name.equalsIgnoreCase("cmis:lastModificationDate")) {
			return "modifiedAt";
		} else if (name.equalsIgnoreCase("cmis:baseTypeId")) {
			return "baseId";
		} else if (name.equalsIgnoreCase("id")) {
			return "id";
		} else if (name.equalsIgnoreCase("operator")) {
			return "operator";
		} else {
			return name;
		}
	}

	private static String getFieldName(Object value) {
		String valueString = value.toString();
		String[] values = valueString.split(":");
		String stringValue = values[1].replaceAll("[-+.^:',{}]", "");
		return stringValue;
	}

	public Object getPropertyValue(String id, String value) {
		if (type != null) {
			Map<String, PropertyDefinition<?>> prof = type.getPropertyDefinitions();
			PropertyDefinition<?> profType = prof.get(id);
			if (profType.getPropertyType().equals(PropertyType.STRING)
					|| profType.getPropertyType().equals(PropertyType.ID)) {
				value = "'" + value + "'";
			}
		} else {
			value = "'" + value + "'";
		}
		return value;
	}

	private static String getExpression(String addExpression, String Extension) {
		String prvsExpression = null;
		if (!expression.isEmpty()) {
			prvsExpression = expression.stream()
					.collect(Collectors.joining(" " + operatorExtension.get(Extension) + " "));
			expression = new ArrayList<>();
		}

		if (prvsExpression != null && !prvsExpression.isEmpty() && prvsExpression != "") {
			addExpression = addExpression != null && !addExpression.isEmpty()
					? " " + operatorExtension.get(Extension) + " " + addExpression
					: "";
			addExpression = "(" + prvsExpression + addExpression + ")";
		} else {
			addExpression = "(" + addExpression + ")";
		}
		expression.add(addExpression);
		return addExpression;
	}

}
