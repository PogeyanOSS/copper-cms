package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;

import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.data.jdo.JAceImpl;
import com.pogeyan.cmis.data.jdo.JAclImpl;
import com.pogeyan.cmis.data.jdo.JTokenImpl;

import groovy.lang.GroovyObject;

public class JDOHelper {
	public static class Impl {
		public static String getJDOTypeId(String typeId, boolean base) {
			if (base) {
				if (typeId == BaseTypeId.CMIS_FOLDER.toString() || typeId == BaseTypeId.CMIS_ITEM.toString()
						|| typeId == BaseTypeId.CMIS_POLICY.toString()
						|| typeId == BaseTypeId.CMIS_SECONDARY.toString()) {
					typeId = "JBaseObject";
				}
			} else {
				if (typeId == BaseTypeId.CMIS_DOCUMENT.toString()) {
					typeId = "JDocumentObject";
				}
			}

			return typeId;
		}

		@SuppressWarnings("unlikely-arg-type")
		public static String getPropertyType(String property) {
			if (PropertyType.BOOLEAN.equals(property)) {
				return "boolean";
			} else if (PropertyType.STRING.equals(property) || PropertyType.ID.equals(property)
					|| PropertyType.URI.equals(property)) {
				return "String";
			} else if (PropertyType.INTEGER.equals(property)) {
				return "int";
			} else if (PropertyType.DATETIME.equals(property)) {
				return "long";
			}
			return property;
		}

		public static List<Map<String, Object>> getPropDefFields(Map<String, PropertyDefinition<?>> propDef) {
			List<Map<String, Object>> listOfPropDef = new ArrayList<>();
			propDef.forEach((key, value) -> {
				Map<String, Object> propDetails = new HashMap<>();
				propDetails.put("id", value.getId());
				propDetails.put("property", getPropertyType(value.getPropertyType().toString()));
				if (value.getLocalName().equalsIgnoreCase("primary")) {
					propDetails.put("primary", true);
				} else {
					propDetails.put("primary", false);
				}
				listOfPropDef.add(propDetails);
			});
			return listOfPropDef;
		}

		public static GroovyObject setPropDefFields(Map<String, Object> properties, GroovyObject groovyInstance) {
			properties.forEach((key, value) -> {
				groovyInstance.invokeMethod("set" + key, value);
			});
			return groovyInstance;
		}

		public static String getDeclareParameter(Map<String, Object> mapValues) {
			String declareField = mapValues.entrySet().stream()
					.map(entry -> getFieldValue(entry.getKey()) + " " + entry.getKey())
					.collect(Collectors.joining(", "));
			return declareField + ",int tokenType";
		}

		public static String getFilterParameter(Map<String, Object> mapValues) {
			String declareField = mapValues.entrySet().stream()
					.map(entry -> entry.getKey() + " == " + entry.getKey() + " && ").collect(Collectors.joining(", "));
			return declareField + "token.changeType != tokenType";

		}

		public static String getFieldValue(Object mapValues) {
			if (mapValues instanceof String) {
				return "String";
			} else if (mapValues instanceof Long) {
				return "Long";
			} else if (mapValues instanceof Integer) {
				return "int";
			}
			return null;
		}

		public static JAclImpl convertJDOAcl(Acl acl) {
			if (acl != null) {
				AccessControlListImplExt acessControl = (AccessControlListImplExt) acl;
				List<JAceImpl> list = new ArrayList<JAceImpl>(acl.getAces().size());
				for (Ace ace : acl.getAces()) {
					JAceImpl aces = new JAceImpl();
					aces.setDirect(true);
					aces.setPrincipal(ace.getPrincipalId());
					aces.setPermissions(ace.getPermissions());
					list.add(aces);
				}
				JAclImpl mAcl = new JAclImpl(list, acessControl.getAclPropagation(), true);
				return mAcl;
			}

			return null;
		}

		public static JTokenImpl convertJDOToken(TokenImpl token) {
			JTokenImpl mongoToken = new JTokenImpl();
			mongoToken.setChangeType(TokenChangeType.toValue(token.getChangeType().toString()));
			mongoToken.setTime(token.getTime());
			return mongoToken;
		}
	}
}
