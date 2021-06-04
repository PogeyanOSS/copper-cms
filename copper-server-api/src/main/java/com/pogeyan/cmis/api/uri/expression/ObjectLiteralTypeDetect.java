package com.pogeyan.cmis.api.uri.expression;

import java.util.function.Function;

import com.pogeyan.cmis.api.uri.exception.ObjectLiteralException;

/**
 *  
 */
public class ObjectLiteralTypeDetect {

	@SuppressWarnings("unused")
	public static ObjectLiteral parseUriLiteral(final String uriLiteral) throws ObjectLiteralException {
		if (uriLiteral == null || "null".equals(uriLiteral)) {
			return new ObjectLiteral(ObjectTypeKind.Null, uriLiteral);
		}

		if ("true".equals(uriLiteral) || "false".equals(uriLiteral)) {
			return new ObjectLiteral(ObjectTypeKind.Boolean, uriLiteral);
		}

		if (uriLiteral.length() >= 2 && uriLiteral.startsWith("'") && uriLiteral.endsWith("'")) {
			return new ObjectLiteral(ObjectTypeKind.String, uriLiteral);
		}

		if (uriLiteral.matches("-?\\p{Digit}+")) {
			int i = 0;
			try {
				i = Integer.parseInt(uriLiteral);
			} catch (Exception e) {
				if (uriLiteral.contains(".")) {
					return new ObjectLiteral(ObjectTypeKind.Decimal, uriLiteral);
				} else {
					return new ObjectLiteral(ObjectTypeKind.Int64, uriLiteral);
				}
			}
			if (i == 0 || i == 1) {
				return new ObjectLiteral(ObjectTypeKind.Binary, uriLiteral);
			}
			if (i >= 0 && i <= Byte.MAX_VALUE) {
				return new ObjectLiteral(ObjectTypeKind.Byte, uriLiteral);
			}
			if (i >= Byte.MIN_VALUE && i < 0) {
				return new ObjectLiteral(ObjectTypeKind.SByte, uriLiteral);
			} else if (i > Byte.MAX_VALUE && i <= 255) {
				return new ObjectLiteral(ObjectTypeKind.Byte, uriLiteral);
			} else if (i >= Short.MIN_VALUE && i <= Short.MAX_VALUE) {
				return new ObjectLiteral(ObjectTypeKind.Int16, uriLiteral);
			} else {
				return new ObjectLiteral(ObjectTypeKind.Int32, uriLiteral);
			}
		}

		if (uriLiteral.endsWith("L") || uriLiteral.endsWith("l")) {
			tryWith((t) -> Long.parseLong(uriLiteral.substring(0, uriLiteral.length() - 1)), uriLiteral);
			return createObjectLiteral(ObjectTypeKind.Int64, uriLiteral, 0, 1);
		}
		if (uriLiteral.endsWith("M") || uriLiteral.endsWith("m")) {
			tryWith((t) -> Long.parseLong(uriLiteral.substring(0, uriLiteral.length() - 1)), uriLiteral);
			return createObjectLiteral(ObjectTypeKind.Decimal, uriLiteral, 0, 1);
		}
		if (uriLiteral.endsWith("D") || uriLiteral.endsWith("d")) {
			tryWith((t) -> Double.parseDouble(uriLiteral.substring(0, uriLiteral.length() - 1)), uriLiteral);
			return createObjectLiteral(ObjectTypeKind.Double, uriLiteral, 0, 1);
		}
		
		/* if ("-INF".equals(uriLiteral) || "INF".equals(uriLiteral) || "NaN".equals(uriLiteral)) {
			return new ObjectLiteral(ObjectTypeKind.Single, uriLiteral);
		}
		if (uriLiteral.endsWith("F") || uriLiteral.endsWith("f")) {
			return createObjectLiteral(ObjectTypeKind.Single, uriLiteral, 0, 1);
		}

		if (uriLiteral.startsWith("datetime'")) {
			return createObjectLiteral(ObjectTypeKind.DateTime, uriLiteral, 9, 1);
		}
		if (uriLiteral.startsWith("datetimeoffset'")) {
			return createObjectLiteral(ObjectTypeKind.DateTimeOffset, uriLiteral, 15, 1);
		}
		if (uriLiteral.startsWith("guid'")) {
			return createObjectLiteral(ObjectTypeKind.Guid, uriLiteral, 5, 1);
		}
		if (uriLiteral.startsWith("time'")) {
			return createObjectLiteral(ObjectTypeKind.Time, uriLiteral, 5, 1);
		}

		if (uriLiteral.startsWith("X'") || uriLiteral.startsWith("binary'")) {
			return new ObjectLiteral(ObjectTypeKind.Binary, uriLiteral);
		} */

		throw new ObjectLiteralException();
	}

	private static <T> String tryWith(Function<String, T> fn, String uriLiteral) throws ObjectLiteralException {
		try {
			T v = fn.apply(uriLiteral);
			return v.toString();
		} catch (Exception ex) {
			throw new ObjectLiteralException();
		}
	}

	private static ObjectLiteral createObjectLiteral(final ObjectTypeKind typeKind, final String literal,
			final int prefixLength, final int suffixLength) throws ObjectLiteralException {
		return new ObjectLiteral(typeKind, literal.substring(prefixLength, literal.length() - suffixLength));
	}
}
