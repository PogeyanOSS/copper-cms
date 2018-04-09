package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.Choice;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.bson.types.ObjectId;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.data.jdo.JAceImpl;
import com.pogeyan.cmis.data.jdo.JAclImpl;
import com.pogeyan.cmis.data.jdo.JChoiceImpl;
import com.pogeyan.cmis.data.jdo.JPermission;
import com.pogeyan.cmis.data.jdo.JTokenImpl;
import com.pogeyan.cmis.data.jdo.JTypeDefinition;
import com.pogeyan.cmis.impl.utils.DBUtils;

import groovy.lang.GroovyObject;

public class JDOHelper {
	private final static String packageName = "com.pogeyan.cmis.data.jdo.";

	public static class Impl {
		public static String getJDOTypeId(String typeId, boolean base) {
			if (base) {
				try {
					BaseTypeId value = BaseTypeId.fromValue(typeId);
					if (value != null) {
						return "JBaseObject";
					}
				} catch (Exception e) {
					return typeId;
				}
			} else {
				try {
					BaseTypeId value = BaseTypeId.fromValue(typeId);
					if (value != null) {
						return "JDocumentObject";
					}
				} catch (Exception e) {
					return typeId;
				}
			}

			return typeId;
		}

		public static String getPropertyType(String property) {
			if (PropertyType.BOOLEAN.value().equalsIgnoreCase(property)) {
				return "Boolean";
			} else if (PropertyType.STRING.value().equalsIgnoreCase(property)
					|| PropertyType.ID.value().equalsIgnoreCase(property)
					|| PropertyType.URI.value().equalsIgnoreCase(property)) {
				return "String";
			} else if (PropertyType.INTEGER.value().equalsIgnoreCase(property)) {
				return "int";
			} else if (PropertyType.DATETIME.value().equalsIgnoreCase(property)) {
				return "long";
			}
			return property;
		}

		public static List<Map<String, Object>> getPropDefFields(Map<String, PropertyDefinition<?>> propDef) {
			List<Map<String, Object>> listOfPropDef = new ArrayList<>();
			propDef.forEach((key, value) -> {
				Map<String, Object> propDetails = new HashMap<>();
				propDetails.put("id", value.getId());
				propDetails.put("property", getPropertyType(value.getPropertyType().value()));
				if (value.getLocalName().equalsIgnoreCase("primary")) {
					propDetails.put("primary", true);
				} else {
					propDetails.put("primary", false);
				}
				listOfPropDef.add(propDetails);
			});
			return listOfPropDef;
		}

		@SuppressWarnings("unchecked")
		public static GroovyObject propDefFields(Map<String, Object> properties, String methodNameIns,
				GroovyObject groovyInstance) {
			properties.forEach((key, value) -> {
				String fieldValue = getFieldValue(value);
				String fieldName = getBaseFieldName(key) != null ? getBaseFieldName(key) : key;
				String methodName = "set" + fieldName;
				if (fieldValue == null) {
					List<String> convertValue = (List<String>) value;
					groovyInstance.invokeMethod(methodName, convertValue);
				} else if (fieldValue.equalsIgnoreCase("String")) {
					groovyInstance.invokeMethod(methodName, value.toString());
				} else if (fieldValue.equalsIgnoreCase("Long")) {
					groovyInstance.invokeMethod(methodName, Long.parseLong(value.toString()));
				} else if (fieldValue.equalsIgnoreCase("int")) {
					groovyInstance.invokeMethod(methodName, Integer.parseInt(value.toString()));
				}

			});
			return groovyInstance;
		}

		public static String getDeclareParameter(Map<String, Object> mapValues) {
			String declareField = mapValues.entrySet().stream()
					.map(entry -> getFieldValue(entry.getValue()) + " " + entry.getKey())
					.collect(Collectors.joining(", "));
			return declareField + ",int tokenType";
		}

		public static String getFilterParameter(Map<String, Object> mapValues) {
			String declareField = mapValues.entrySet().stream()
					.map(entry -> "this." + entry.getKey() + " == " + entry.getKey() + " && ")
					.collect(Collectors.joining(""));
			return declareField + "token.changeType != tokenType";

		}

		public static String getACLFilterParameter(String[] principalIds) {
			String declareField = Stream.of(principalIds).map(t -> "|| jace.principal == " + t + " ")
					.collect(Collectors.joining(""));
			declareField = declareField.substring(1);
			declareField = declareField.substring(1);
			declareField = declareField.substring(1);
			return declareField;
		}

		public static String getACLDeclareParameter(String[] principalIds) {
			String declareField = Stream.of(principalIds).map(t -> "String " + t).collect(Collectors.joining(","));
			return declareField;
		}

		public static Map<String, Object> getACLMap(String[] principalIds) {
			Map<String, Object> declareFieldMap = Stream.of(principalIds).distinct()
					.collect(Collectors.toMap(t -> t, t -> t));
			return declareFieldMap;
		}

		public static String getFieldValue(Object mapValues) {
			if (mapValues instanceof String) {
				return "String";
			} else if (mapValues instanceof Long) {
				return "Long";
			} else if (mapValues instanceof Integer) {
				return "int";
			} else if (mapValues instanceof Boolean) {
				return "Boolean";
			}
			return null;
		}

		public static List<JAclImpl> convertJDOAcl(String id, Acl acl) {
			List<JAclImpl> aclList = new ArrayList<>();
			if (acl != null) {
				AccessControlListImplExt acessControl = (AccessControlListImplExt) acl;
				List<JAceImpl> list = new ArrayList<JAceImpl>(acl.getAces().size());
				for (Ace ace : acl.getAces()) {
					JAceImpl aces = new JAceImpl();
					aces.setDirect(true);
					List<JPermission> jPermission = new ArrayList<>();
					if (ace.getPermissions().size() > 0) {
						for (String permissions : ace.getPermissions()) {
							JPermission permission = new JPermission();
							permission.setPermissionRule(permissions);
							permission.setBaseId(id);
							jPermission.add(permission);
						}
					}
					aces.setPrincipal(ace.getPrincipalId());
					aces.setPermissions(jPermission);
					aces.setAceId((new ObjectId()).toString());
					aces.setBaseId(id);
					list.add(aces);
				}
				JAclImpl mAcl = new JAclImpl(list, acessControl.getAclPropagation(), true);
				mAcl.setBaseId(id);
				mAcl.setAclId((new ObjectId()).toString());
				aclList.add(mAcl);
				return aclList;
			}

			return null;
		}

		public static JTokenImpl convertJDOToken(TokenImpl token) {
			JTokenImpl mongoToken = new JTokenImpl();
			mongoToken.setChangeType(TokenChangeType.toValue(token.getChangeType().toString()));
			mongoToken.setTime(token.getTime());
			return mongoToken;
		}

		public static Class<?> load(String repositoryId, String typeId, boolean baseObject) {
			Class<?> enhancedCC = JDOServiceImpl.getInstance().getCacheMapValue(repositoryId,
					packageName + getJDOTypeId(typeId, baseObject));
			if (enhancedCC != null) {
				return enhancedCC;
			} else {
				if (typeId.equalsIgnoreCase("JDocumentObject")) {
					typeId = BaseTypeId.CMIS_DOCUMENT.value();
				} else if (typeId.equalsIgnoreCase("JBaseObject")) {
					typeId = BaseTypeId.CMIS_FOLDER.value();
				}
				List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
						Arrays.asList(typeId));
				if (typeDef == null || typeDef != null && typeDef.size() == 0) {
					return null;
				}

				JTypeDefinition type = (JTypeDefinition) typeDef.get(0);
				if (type.getParentTypeId() != null) {
					enhancedCC = JDOServiceImpl.getInstance().getEnhancedClass(repositoryId, typeId, typeId,
							FileNameType.PROPERTYOBJECT, getClassMap(type, baseObject), baseObject,
							type.getModifiedAt());
				} else {
					if (baseObject) {
						enhancedCC = JDOServiceImpl.getInstance().getEnhancedClass(repositoryId, typeId, "JBaseObject",
								FileNameType.BASEOBJECT, new HashMap<>(), baseObject, type.getModifiedAt());
					} else {
						enhancedCC = JDOServiceImpl.getInstance().getEnhancedClass(repositoryId, typeId,
								"JDocumentObject", FileNameType.DOCUMENTOBJECT, new HashMap<>(), baseObject,
								type.getModifiedAt());
					}

				}
				return enhancedCC;
			}

		}

		public static Map<String, Object> getClassMap(TypeDefinition type, boolean baseObject) {
			List<Map<String, Object>> listFields = JDOHelper.Impl.getPropDefFields(type.getPropertyDefinitions());
			Map<String, Object> classMap = new HashMap<>();
			classMap.put("className", type.getId());
			classMap.put("propDef", listFields);
			if (type.getParentTypeId() != null) {
				if (type.getBaseTypeId().equals(BaseTypeId.CMIS_DOCUMENT)) {
					classMap.put("parentClassName", getJDOTypeId(type.getParentTypeId(), false));
				} else {
					classMap.put("parentClassName", getJDOTypeId(type.getParentTypeId(), true));
				}

			} else {
				if (baseObject) {
					classMap.put("parentClassName", "JBaseObject");
				} else {
					classMap.put("parentClassName", "JDocumentObject");
				}
			}
			return classMap;

		}

		public static String getBaseFieldName(String fieldName) {
			Map<String, String> fieldNames = new HashMap<>();
			// baseProp
			fieldNames.put("name", "Name");
			fieldNames.put("repositoryId", "RepositoryId");
			fieldNames.put("typeId", "TypeId");
			fieldNames.put("parentId", "ParentId");
			fieldNames.put("path", "Path");
			fieldNames.put("baseId", "BaseId");
			fieldNames.put("modifiedAt", "ModifiedAt");
			fieldNames.put("modifiedBy", "ModifiedBy");
			fieldNames.put("internalPath", "InternalPath");
			fieldNames.put("policies", "Policies");
			fieldNames.put("secondaryTypeIds", "SecondaryTypeIds");
			fieldNames.put("policyText", "PolicyText");

			// documentProps
			fieldNames.put("isImmutable", "IsImmutable");
			fieldNames.put("isLatestVersion", "IsLatestVersion");
			fieldNames.put("isMajorVersion", "IsMajorVersion");
			fieldNames.put("isLatestMajorVersion", "IsLatestMajorVersion");
			fieldNames.put("isPrivateWorkingCopy", "IsPrivateWorkingCopy");
			fieldNames.put("versionLabel", "VersionLabel");
			fieldNames.put("versionSeriesId", "VersionSeriesId");
			fieldNames.put("versionReferenceId", "VersionReferenceId");
			fieldNames.put("isVersionSeriesCheckedOut", "IsVersionSeriesCheckedOut");
			fieldNames.put("versionSeriesCheckedOutBy", "VersionSeriesCheckedOutBy");
			fieldNames.put("versionSeriesCheckedOutId", "VersionSeriesCheckedOutId");
			fieldNames.put("checkinComment", "CheckinComment");
			fieldNames.put("contentStreamLength", "ContentStreamLength");
			fieldNames.put("contentStreamMimeType", "ContentStreamMimeType");
			fieldNames.put("contentStreamFileName", "ContentStreamFileName");
			fieldNames.put("contentStreamId", "ContentStreamId");
			fieldNames.put("previousVersionObjectId", "PreviousVersionObjectId");
			return fieldNames.get(fieldName);
		}

		public static List<String> getPropertyDefinition(String repositoryId, String typeId, List<String> propFields) {
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
					Arrays.asList(typeId));
			if (typeDef == null || typeDef != null && typeDef.size() == 0) {
				return propFields;
			}

			TypeDefinition type = typeDef.get(0);
			if (type.getParentTypeId() != null) {
				if (!type.getParentTypeId().equalsIgnoreCase(BaseTypeId.CMIS_FOLDER.value())
						|| !type.getParentTypeId().equalsIgnoreCase(BaseTypeId.CMIS_DOCUMENT.value())
						|| !type.getParentTypeId().equalsIgnoreCase(BaseTypeId.CMIS_ITEM.value())
						|| !type.getParentTypeId().equalsIgnoreCase(BaseTypeId.CMIS_POLICY.value())
						|| !type.getParentTypeId().equalsIgnoreCase(BaseTypeId.CMIS_SECONDARY.value())) {
					List<String> propField = type.getPropertyDefinitions().entrySet().stream().map(t -> t.getKey())
							.collect(Collectors.toList());
					List<String> totalFields = Stream.concat(propFields.stream(), propField.stream()).distinct()
							.collect(Collectors.toList());
					getPropertyDefinition(repositoryId, type.getParentTypeId(), totalFields);
				}
			}

			return propFields;

		}

		public static List<? extends IBaseObject> getBaseObject(Class<?> objectClass, List<Object> results,
				List<String> propFields) {
			List<IBaseObject> result = new ArrayList<>();
			results.stream().forEach((k -> {
				try {
					GroovyObject myInstance = (GroovyObject) objectClass.newInstance();
					myInstance = (GroovyObject) k;
					if (!propFields.isEmpty() && propFields.size() > 0) {
						Map<String, Object> properties = getPropFieldValues(myInstance, propFields);
						myInstance.invokeMethod("setProperties", properties);
					}
					result.add((IBaseObject) myInstance);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}));
			return result;
		}

		public static List<? extends IDocumentObject> getDocumentObject(Class<?> objectClass, List<Object> results,
				List<String> propFields) {
			List<IDocumentObject> result = new ArrayList<>();
			results.stream().forEach((k -> {
				try {
					GroovyObject myInstance = (GroovyObject) objectClass.newInstance();
					myInstance = (GroovyObject) k;
					if (!propFields.isEmpty() && propFields.size() > 0) {
						Map<String, Object> properties = getPropFieldValues(myInstance, propFields);
						myInstance.invokeMethod("setProperties", properties);
					}
					result.add((IDocumentObject) myInstance);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}));
			return result;
		}

		private static Map<String, Object> getPropFieldValues(GroovyObject myInstance, List<String> propFields) {
			Map<String, Object> propValues = new HashMap<>();
			propFields.stream().forEach((field -> {
				if (myInstance.getProperty(field) != null) {
					propValues.put(field, myInstance.getProperty(field));
				}
			}));
			return propValues;

		}

		@SuppressWarnings("unchecked")
		public static List<JChoiceImpl> setChoice(List<?> choice) {
			if (choice != null && !choice.isEmpty()) {
				List<JChoiceImpl> jChoiceList = new ArrayList<>();
				List<Choice<?>> choiceList = (List<Choice<?>>) choice;
				for (Choice<?> ch : choiceList) {
					JChoiceImpl jChoice = new JChoiceImpl(ch.getDisplayName(), (List<Object>) ch.getValue());
					jChoiceList.add(jChoice);
				}
				return jChoiceList;
			}
			return null;
		}

		public static String getMappedColumns(String[] mappedColumns) {
			String mappedField = Stream.of(mappedColumns).filter(t -> t != "acl").map(t -> "this." + t)
					.collect(Collectors.joining(","));
			return mappedField;

		}

	}
}
