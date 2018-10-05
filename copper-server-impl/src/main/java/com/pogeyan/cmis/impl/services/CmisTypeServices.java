/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.impl.services;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ExtensionsData;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyIntegerDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyStringDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionContainer;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionList;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.TypeDefinitionContainerImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.TypeDefinitionListImpl;
import org.apache.chemistry.opencmis.commons.impl.jaxb.CmisException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.CustomTypeId;
import com.pogeyan.cmis.api.auth.IUserGroupObject;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.common.CmisDocumentTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisFolderTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisPolicyTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisRelationshipTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.common.CmisSecondaryTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.common.ItemTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;
import com.pogeyan.cmis.api.data.common.TypePermissionType;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.impl.factory.TypeServiceFactory;
import com.pogeyan.cmis.impl.utils.DBUtils;

public class CmisTypeServices {
	private static final Logger LOG = LoggerFactory.getLogger(CmisTypeServices.class);

	public static class Impl {

		public static void addBaseType(String repositoryId, IUserObject userObject) throws MongoException {
			LOG.info("addBaseType for this repo: {}", repositoryId);
			try {
				MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MTypeManagerDAO.class);
				List<? extends TypeDefinition> getTypeObject = DBUtils.TypeServiceDAO.getById(repositoryId, null, null);
				if (getTypeObject != null) {
				} else {
					List<? extends TypeDefinition> typeDef = typeManagerDAO.getById(null, null);
					if (typeDef != null && typeDef.size() > 0) {
						LOG.info("BaseType already created for repository: {}", repositoryId);
					} else {
						List<TypeDefinition> baseType = upset(repositoryId);
						for (TypeDefinition tm : baseType) {
							typeManagerDAO.commit(tm);
							if (tm.getId().equalsIgnoreCase(BaseTypeId.CMIS_FOLDER.value())) {
								CmisObjectService.Impl.addRootFolder(repositoryId,
										userObject == null ? null : userObject.getUserDN());
							}
							if (tm.getId().equalsIgnoreCase(CustomTypeId.CMIS_EXT_RELATIONMD.value())
									|| tm.getId().equalsIgnoreCase(CustomTypeId.CMIS_EXT_RELATIONSHIP.value())
									|| tm.getId().equalsIgnoreCase(CustomTypeId.CMIS_EXT_CONFIG.value())) {
								try {
									createFolderForType(tm, userObject, repositoryId);
								} catch (IOException e) {
									typeManagerDAO.delete(tm.getId());
									LOG.error("Folder creation exception:  {}, repositoryId: {}", e, repositoryId);
									throw new IllegalArgumentException(e.getMessage());
								}
							}
							CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, tm.getId(), tm);
						}
						LOG.info("BaseType created for this repository: {}", repositoryId);
					}
				}

			} catch (MongoException e) {
				LOG.error("MongoObject shouldnot be null: {}, repository: {}", e, repositoryId);
				throw new MongoException("MongoObject shouldnot be null");
			}
		}

		/**
		 * returns the Morphia BaseTypeObject
		 */
		private static List<TypeDefinition> upset(String repositoryId) {
			List<TypeDefinition> typeList = new ArrayList<>();
			TypeMutabilityImpl type = new TypeMutabilityImpl(true, true, true);
			Map<String, PropertyDefinitionImpl<?>> folderProperty = getBaseFolderProperty();
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			TypeDefinition folderType = typeManagerDAO.createObjectFacade(BaseTypeId.CMIS_FOLDER.value(),
					BaseTypeId.CMIS_FOLDER.value(), BaseTypeId.CMIS_FOLDER.value(), BaseTypeId.CMIS_FOLDER.value(),
					BaseTypeId.CMIS_FOLDER.value(), "Folder", BaseTypeId.CMIS_FOLDER, null, true, true, true, true,
					true, true, true, type, folderProperty, null, null);
			Map<String, PropertyDefinitionImpl<?>> documentProperty = getBaseDocumentProperty();
			DocumentTypeDefinition documentType = (DocumentTypeDefinition) typeManagerDAO.createObjectFacade(
					BaseTypeId.CMIS_DOCUMENT.value(), BaseTypeId.CMIS_DOCUMENT.value(),
					BaseTypeId.CMIS_DOCUMENT.value(), BaseTypeId.CMIS_DOCUMENT.value(),
					BaseTypeId.CMIS_DOCUMENT.value(), "Document", BaseTypeId.CMIS_DOCUMENT, null, true, true, true,
					true, true, true, true, type, documentProperty, true, ContentStreamAllowed.ALLOWED);

			TypeDefinition itemType = typeManagerDAO.createObjectFacade(BaseTypeId.CMIS_ITEM.value(),
					BaseTypeId.CMIS_ITEM.value(), BaseTypeId.CMIS_ITEM.value(), BaseTypeId.CMIS_ITEM.value(),
					BaseTypeId.CMIS_ITEM.value(), "Item", BaseTypeId.CMIS_ITEM, null, true, true, true, true, true,
					true, true, type, getBaseProperty(), null, null);

			Map<String, PropertyDefinitionImpl<?>> relationShipProperty = getBaserelationShipProperty();
			TypeDefinition realtionShipType = typeManagerDAO.createObjectFacade(BaseTypeId.CMIS_RELATIONSHIP.value(),
					BaseTypeId.CMIS_RELATIONSHIP.value(), BaseTypeId.CMIS_RELATIONSHIP.value(),
					BaseTypeId.CMIS_RELATIONSHIP.value(), BaseTypeId.CMIS_RELATIONSHIP.value(), "Relationship",
					BaseTypeId.CMIS_RELATIONSHIP, null, true, false, true, true, true, true, true, type,
					relationShipProperty, null, null);

			Map<String, PropertyDefinitionImpl<?>> policyProperty = getBasepolicyProperty();
			TypeDefinition policyType = typeManagerDAO.createObjectFacade(BaseTypeId.CMIS_POLICY.value(),
					BaseTypeId.CMIS_POLICY.value(), BaseTypeId.CMIS_POLICY.value(), BaseTypeId.CMIS_POLICY.value(),
					BaseTypeId.CMIS_POLICY.value(), "Policy", BaseTypeId.CMIS_POLICY, null, true, true, true, true,
					true, true, true, type, policyProperty, null, null);

			Map<String, PropertyDefinitionImpl<?>> SecondaryTypeProperty = getBaseSecondaryTypeProperty();
			TypeDefinition secondaryType = typeManagerDAO.createObjectFacade(BaseTypeId.CMIS_SECONDARY.value(),
					BaseTypeId.CMIS_SECONDARY.value(), BaseTypeId.CMIS_SECONDARY.value(),
					BaseTypeId.CMIS_SECONDARY.value(), BaseTypeId.CMIS_SECONDARY.value(), "Secondary Type",
					BaseTypeId.CMIS_SECONDARY, null, false, false, true, true, true, false, false, type,
					SecondaryTypeProperty, null, null);

			Map<String, PropertyDefinitionImpl<?>> cmisRelationExt = getRelationExt();
			TypeDefinition cmisRelationExtObject = typeManagerDAO.createObjectFacade(
					CustomTypeId.CMIS_EXT_RELATIONMD.value(), CustomTypeId.CMIS_EXT_RELATIONMD.value(),
					CustomTypeId.CMIS_EXT_RELATIONMD.value(), CustomTypeId.CMIS_EXT_RELATIONMD.value(),
					CustomTypeId.CMIS_EXT_RELATIONMD.value(), CustomTypeId.CMIS_EXT_RELATIONMD.value(),
					BaseTypeId.CMIS_ITEM, null, true, true, true, true, true, true, true, type, cmisRelationExt, null,
					null);

			Map<String, PropertyDefinitionImpl<?>> cmisRelationMd = getRelationShipPropertyExt();
			TypeDefinition cmisRelationMdObject = typeManagerDAO.createObjectFacade(
					CustomTypeId.CMIS_EXT_RELATIONSHIP.value(), CustomTypeId.CMIS_EXT_RELATIONSHIP.value(),
					CustomTypeId.CMIS_EXT_RELATIONSHIP.value(), CustomTypeId.CMIS_EXT_RELATIONSHIP.value(),
					CustomTypeId.CMIS_EXT_RELATIONSHIP.value(), "Relationship", BaseTypeId.CMIS_RELATIONSHIP, null,
					true, false, true, true, true, true, true, type, cmisRelationMd, null, null);
			Map<String, PropertyDefinitionImpl<?>> cmisExtConfig = getConfigExt();
			TypeDefinition cmisExtConfigObject = typeManagerDAO.createObjectFacade(CustomTypeId.CMIS_EXT_CONFIG.value(),
					CustomTypeId.CMIS_EXT_CONFIG.value(), CustomTypeId.CMIS_EXT_CONFIG.value(),
					CustomTypeId.CMIS_EXT_CONFIG.value(), CustomTypeId.CMIS_EXT_CONFIG.value(),
					CustomTypeId.CMIS_EXT_CONFIG.value(), BaseTypeId.CMIS_ITEM, null, true, true, true, true, true,
					true, true, type, cmisExtConfig, null, null);

			typeList.add(folderType);
			typeList.add(documentType);
			typeList.add(itemType);
			typeList.add(realtionShipType);
			typeList.add(policyType);
			typeList.add(secondaryType);
			typeList.add(cmisRelationExtObject);
			typeList.add(cmisRelationMdObject);
			typeList.add(cmisExtConfigObject);

			return typeList;
		}

		@SuppressWarnings("rawtypes")
		private static Map<String, PropertyDefinitionImpl<?>> getBaseProperty() {
			Map<String, PropertyDefinitionImpl<?>> list = new HashMap<>();
			PropertyDefinitionImpl<?> name = new PropertyDefinitionImpl("cmis:name", "localName", "localNameSpace",
					"cmis:name", "cmis:name", "description", PropertyType.STRING, Cardinality.SINGLE,
					Updatability.READWRITE, false, false, false, false, null);
			list.put("cmis:name", name);
			PropertyDefinitionImpl<?> objectId = new PropertyDefinitionImpl("cmis:objectId", "localName",
					"localNameSpace", "cmis:objectId", "cmis:objectId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.ONCREATE, false, false, false, false, null);
			list.put("cmis:objectId", objectId);
			PropertyDefinitionImpl<?> objectTypeId = new PropertyDefinitionImpl("cmis:objectTypeId", "objectTypeId",
					"objectTypeId", "cmis:objectTypeId", "cmis:objectTypeId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.ONCREATE, false, true, false, false, null);
			list.put("cmis:objectTypeId", objectTypeId);
			PropertyDefinitionImpl<?> baseTypeId = new PropertyDefinitionImpl("cmis:baseTypeId", "baseTypeId",
					"baseTypeId", "cmis:baseTypeId", "cmis:baseTypeId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			list.put("cmis:baseTypeId", baseTypeId);
			PropertyDefinitionImpl<?> createdBy = new PropertyDefinitionImpl("cmis:createdBy", "localName",
					"localNameSpace", "cmis:createdBy", "cmis:createdBy", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READONLY, false, false, true, true, null);
			list.put("cmis:createdBy", createdBy);
			PropertyDefinitionImpl<?> creationDate = new PropertyDefinitionImpl("cmis:creationDate", "localName",
					"localNameSpace", "cmis:creationDate", "cmis:creationDate", "description", PropertyType.DATETIME,
					Cardinality.SINGLE, Updatability.READONLY, false, false, true, true, null);
			list.put("cmis:creationDate", creationDate);
			PropertyDefinitionImpl<?> lastModifiedBy = new PropertyDefinitionImpl("cmis:lastModifiedBy",
					"lastModifiedBy", "lastModifiedBy", "cmis:lastModifiedBy", "cmis:lastModifiedBy", "description",
					PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false, true, true, null);
			list.put("cmis:lastModifiedBy", lastModifiedBy);
			PropertyDefinitionImpl<?> lastModificationDate = new PropertyDefinitionImpl("cmis:lastModificationDate",
					"localName", "localNameSpace", "cmis:lastModificationDate", "cmis:lastModificationDate",
					"description", PropertyType.DATETIME, Cardinality.SINGLE, Updatability.READONLY, false, false, true,
					true, null);
			list.put("cmis:lastModificationDate", lastModificationDate);
			PropertyDefinitionImpl<?> changeToken = new PropertyDefinitionImpl("cmis:changeToken", "changeToken",
					"changeToken", "cmis:changeToken", "cmis:changeToken", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			list.put("cmis:changeToken", changeToken);
			PropertyDefinitionImpl<?> secondaryObjectTypeIds = new PropertyDefinitionImpl("cmis:secondaryObjectTypeIds",
					"secondaryObjectTypeIds", "secondaryObjectTypeIds", "cmis:secondaryObjectTypeIds",
					"cmis:secondaryObjectTypeIds", "description", PropertyType.ID, Cardinality.MULTI,
					Updatability.READWRITE, false, false, false, false, null);
			list.put("cmis:secondaryObjectTypeIds", secondaryObjectTypeIds);
			PropertyDefinitionImpl<?> description = new PropertyDefinitionImpl("cmis:description", "description",
					"description", "cmis:description", "cmis:description", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READWRITE, false, false, false, false, null);
			list.put("cmis:description", description);

			return list;

		}

		/**
		 * returns the BaseFolderPropertyDefinition
		 */
		@SuppressWarnings("rawtypes")
		private static Map<String, PropertyDefinitionImpl<?>> getBaseFolderProperty() {
			Map<String, PropertyDefinitionImpl<?>> folderList = getBaseProperty();
			PropertyDefinitionImpl<?> parentId = new PropertyDefinitionImpl("cmis:parentId", "localName",
					"localNameSpace", "cmis:parentId", "cmis:parentId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.READONLY, false, false, true, false, null);
			folderList.put("cmis:parentId", parentId);
			PropertyDefinitionImpl<?> allowedChildObjectTypeIds = new PropertyDefinitionImpl(
					"cmis:allowedChildObjectTypeIds", "localName", "localNameSpace", "cmis:allowedChildObjectTypeIds",
					"cmis:allowedChildObjectTypeIds", "description", PropertyType.ID, Cardinality.MULTI,
					Updatability.READONLY, false, false, true, false, null);
			folderList.put("cmis:allowedChildObjectTypeIds", allowedChildObjectTypeIds);
			PropertyDefinitionImpl<?> path = new PropertyDefinitionImpl("cmis:path", "localName", "localNameSpace",
					"cmis:path", "cmis:path", "description", PropertyType.STRING, Cardinality.SINGLE,
					Updatability.READONLY, false, false, true, false, null);
			folderList.put("cmis:path", path);
			return folderList;
		}

		/**
		 * returns the BaseDocumentPropertyDefinition
		 */
		@SuppressWarnings("rawtypes")
		private static Map<String, PropertyDefinitionImpl<?>> getBaseDocumentProperty() {
			Map<String, PropertyDefinitionImpl<?>> documentList = getBaseProperty();
			PropertyDefinitionImpl<?> isImmutable = new PropertyDefinitionImpl("cmis:isImmutable", "localName",
					"localNameSpace", "cmis:isImmutable", "cmis:isImmutable", "description", PropertyType.BOOLEAN,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:isImmutable", isImmutable);
			PropertyDefinitionImpl<?> isLatestVersion = new PropertyDefinitionImpl("cmis:isLatestVersion", "localName",
					"localNameSpace", "cmis:isLatestVersion", "cmis:isLatestVersion", "description",
					PropertyType.BOOLEAN, Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:isLatestVersion", isLatestVersion);
			PropertyDefinitionImpl<?> isMajorVersion = new PropertyDefinitionImpl("cmis:isMajorVersion",
					"Is Major Version", "Is Major Version", "cmis:isMajorVersion", "cmis:isMajorVersion", "description",
					PropertyType.BOOLEAN, Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:isMajorVersion", isMajorVersion);
			PropertyDefinitionImpl<?> isLatestMajorVersion = new PropertyDefinitionImpl("cmis:isLatestMajorVersion",
					"Is Latest Major Version", "Is Latest Major Version", "cmis:isLatestMajorVersion",
					"cmis:isLatestMajorVersion", "description", PropertyType.BOOLEAN, Cardinality.SINGLE,
					Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:isLatestMajorVersion", isLatestMajorVersion);
			PropertyDefinitionImpl<?> isPrivateWorkingCopy = new PropertyDefinitionImpl("cmis:isPrivateWorkingCopy",
					"isPrivateWorkingCopy", "isPrivateWorkingCopy", "cmis:isPrivateWorkingCopy",
					"cmis:isLatestMajorVersion", "description", PropertyType.BOOLEAN, Cardinality.SINGLE,
					Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:isPrivateWorkingCopy", isPrivateWorkingCopy);
			PropertyDefinitionImpl<?> versionLabel = new PropertyDefinitionImpl("cmis:versionLabel", "localName",
					"localNameSpace", "cmis:versionLabel", "cmis:versionLabel", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:versionLabel", versionLabel);
			PropertyDefinitionImpl<?> versionSeriesId = new PropertyDefinitionImpl("cmis:versionSeriesId", "localName",
					"localNameSpace", "cmis:versionSeriesId", "cmis:versionSeriesId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:versionSeriesId", versionSeriesId);
			PropertyDefinitionImpl<?> isVersionSeriesCheckedOut = new PropertyDefinitionImpl(
					"cmis:isVersionSeriesCheckedOut", "Is Verison Series Checked Out", "Is Verison Series Checked Out",
					"cmis:isVersionSeriesCheckedOut", "cmis:isVersionSeriesCheckedOut", "description",
					PropertyType.BOOLEAN, Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:isVersionSeriesCheckedOut", isVersionSeriesCheckedOut);
			PropertyDefinitionImpl<?> versionSeriesCheckedOutBy = new PropertyDefinitionImpl(
					"cmis:versionSeriesCheckedOutBy", "localName", "localNameSpace", "cmis:versionSeriesCheckedOutBy",
					"cmis:versionSeriesCheckedOutBy", "description", PropertyType.STRING, Cardinality.SINGLE,
					Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:versionSeriesCheckedOutBy", versionSeriesCheckedOutBy);
			PropertyDefinitionImpl<?> versionSeriesCheckedOutId = new PropertyDefinitionImpl(
					"cmis:versionSeriesCheckedOutId", "Version Series Checked Out Id", "Version Series Checked Out Id",
					"cmis:versionSeriesCheckedOutId", "cmis:versionSeriesCheckedOutId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:versionSeriesCheckedOutId", versionSeriesCheckedOutId);
			PropertyDefinitionImpl<?> checkinComment = new PropertyDefinitionImpl("cmis:checkinComment",
					"Checkin Comment", "Checkin Comment", "cmis:checkinComment", "cmis:checkinComment", "description",
					PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:checkinComment", checkinComment);
			PropertyDefinitionImpl<?> contentStreamLength = new PropertyDefinitionImpl("cmis:contentStreamLength",
					"Content Stream Length", "Content Stream Length", "cmis:contentStreamLength",
					"cmis:contentStreamLength", "description", PropertyType.INTEGER, Cardinality.SINGLE,
					Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:contentStreamLength", contentStreamLength);
			PropertyDefinitionImpl<?> contentStreamMimeType = new PropertyDefinitionImpl("cmis:contentStreamMimeType",
					"MIME Type", "MIME Type", "cmis:contentStreamMimeType", "cmis:contentStreamMimeType", "description",
					PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:contentStreamMimeType", contentStreamMimeType);
			PropertyDefinitionImpl<?> contentStreamFileName = new PropertyDefinitionImpl("cmis:contentStreamFileName",
					"Filename", "Filename", "cmis:contentStreamFileName", "cmis:contentStreamFileName", "description",
					PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:contentStreamFileName", contentStreamFileName);
			PropertyDefinitionImpl<?> contentStreamId = new PropertyDefinitionImpl("cmis:contentStreamId",
					"Content Stream Id", "Content Stream Id", "cmis:contentStreamId", "cmis:contentStreamId",
					"description", PropertyType.ID, Cardinality.SINGLE, Updatability.READONLY, false, false, false,
					false, null);
			documentList.put("cmis:contentStreamId", contentStreamId);
			PropertyDefinitionImpl<?> previousVersionObjectId = new PropertyDefinitionImpl(
					"cmis:previousVersionObjectId", "previous Version ObjectId", "previous Version ObjectId",
					"cmis:previousVersionObjectId", "cmis:previousVersionObjectId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			documentList.put("cmis:previousVersionObjectId", previousVersionObjectId);
			PropertyDefinitionImpl<?> path = new PropertyDefinitionImpl("cmis:path", "localName", "localNameSpace",
					"cmis:path", "cmis:path", "description", PropertyType.STRING, Cardinality.SINGLE,
					Updatability.READONLY, false, false, true, false, null);
			documentList.put("cmis:path", path);
			return documentList;
		}

		/**
		 * returns the BasePolicyPropertyDefinition
		 */
		@SuppressWarnings("rawtypes")
		public static Map<String, PropertyDefinitionImpl<?>> getBasepolicyProperty() {
			Map<String, PropertyDefinitionImpl<?>> policy = getBaseProperty();
			PropertyDefinitionImpl<?> policyText = new PropertyDefinitionImpl("cmis:policyText", "policyText",
					"policyText", "cmis:policyText", "cmis:policyText", "policyText", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READWRITE, false, false, false, false, null);
			policy.put("cmis:policyText", policyText);

			return policy;
		}

		/**
		 * returns the BaseRealtionShipPropertyDefinition
		 */
		@SuppressWarnings("rawtypes")
		private static Map<String, PropertyDefinitionImpl<?>> getBaserelationShipProperty() {
			Map<String, PropertyDefinitionImpl<?>> relationship = getBaseProperty();
			PropertyDefinitionImpl<?> sourceId = new PropertyDefinitionImpl("cmis:sourceId", "sourceId", "sourceId",
					"cmis:sourceId", "cmis:sourceId", "description", PropertyType.ID, Cardinality.SINGLE,
					Updatability.READWRITE, false, true, false, false, null);
			relationship.put("cmis:sourceId", sourceId);
			PropertyDefinitionImpl<?> targetId = new PropertyDefinitionImpl("cmis:targetId", "targetId", "targetId",
					"cmis:targetId", "cmis:targetId", "description", PropertyType.ID, Cardinality.SINGLE,
					Updatability.READWRITE, false, true, false, false, null);
			relationship.put("cmis:targetId", targetId);

			return relationship;
		}

		@SuppressWarnings("rawtypes")
		private static Map<String, PropertyDefinitionImpl<?>> getRelationShipPropertyExt() {
			Map<String, PropertyDefinitionImpl<?>> relationship = getBaserelationShipProperty();
			PropertyDefinitionImpl<?> relation_name = new PropertyDefinitionImpl("relation_name", "relation_name",
					"relation_name", "relation_name", "relation_name", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READWRITE, false, true, false, false, null);
			relationship.put("relation_name", relation_name);
			return relationship;
		}

		@SuppressWarnings("rawtypes")
		public static Map<String, PropertyDefinitionImpl<?>> getBaseSecondaryTypeProperty() {
			Map<String, PropertyDefinitionImpl<?>> list = new HashMap<>();
			PropertyDefinitionImpl<?> name = new PropertyDefinitionImpl("cmis:name", "localName", "localNameSpace",
					"cmis:name", "cmis:name", "description", PropertyType.STRING, Cardinality.SINGLE,
					Updatability.READWRITE, false, false, true, false, null);
			list.put("cmis:name", name);
			PropertyDefinitionImpl<?> objectId = new PropertyDefinitionImpl("cmis:objectId", "localName",
					"localNameSpace", "cmis:objectId", "cmis:objectId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.ONCREATE, false, false, true, false, null);
			list.put("cmis:objectId", objectId);
			PropertyDefinitionImpl<?> objectTypeId = new PropertyDefinitionImpl("cmis:objectTypeId", "objectTypeId",
					"objectTypeId", "cmis:objectTypeId", "cmis:objectTypeId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.ONCREATE, false, false, false, false, null);
			list.put("cmis:objectTypeId", objectTypeId);
			PropertyDefinitionImpl<?> baseTypeId = new PropertyDefinitionImpl("cmis:baseTypeId", "baseTypeId",
					"baseTypeId", "cmis:baseTypeId", "cmis:baseTypeId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			list.put("cmis:baseTypeId", baseTypeId);
			PropertyDefinitionImpl<?> createdBy = new PropertyDefinitionImpl("cmis:createdBy", "localName",
					"localNameSpace", "cmis:createdBy", "cmis:createdBy", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READONLY, false, false, true, false, null);
			list.put("cmis:createdBy", createdBy);
			PropertyDefinitionImpl<?> creationDate = new PropertyDefinitionImpl("cmis:creationDate", "localName",
					"localNameSpace", "cmis:creationDate", "cmis:creationDate", "description", PropertyType.DATETIME,
					Cardinality.SINGLE, Updatability.READONLY, false, false, true, false, null);
			list.put("cmis:creationDate", creationDate);
			PropertyDefinitionImpl<?> lastModifiedBy = new PropertyDefinitionImpl("cmis:lastModifiedBy",
					"lastModifiedBy", "lastModifiedBy", "cmis:lastModifiedBy", "cmis:lastModifiedBy", "description",
					PropertyType.STRING, Cardinality.SINGLE, Updatability.READONLY, false, false, true, false, null);
			list.put("cmis:lastModifiedBy", lastModifiedBy);
			PropertyDefinitionImpl<?> lastModificationDate = new PropertyDefinitionImpl("cmis:lastModificationDate",
					"localName", "localNameSpace", "cmis:lastModificationDate", "cmis:lastModificationDate",
					"description", PropertyType.DATETIME, Cardinality.SINGLE, Updatability.READWRITE, false, false,
					true, false, null);
			list.put("cmis:lastModificationDate", lastModificationDate);
			PropertyDefinitionImpl<?> changeToken = new PropertyDefinitionImpl("cmis:changeToken", "changeToken",
					"changeToken", "cmis:changeToken", "cmis:changeToken", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			list.put("cmis:changeToken", changeToken);
			PropertyDefinitionImpl<?> secondaryObjectTypeIds = new PropertyDefinitionImpl("cmis:secondaryObjectTypeIds",
					"secondaryObjectTypeIds", "secondaryObjectTypeIds", "cmis:secondaryObjectTypeIds",
					"cmis:secondaryObjectTypeIds", "description", PropertyType.STRING, Cardinality.MULTI,
					Updatability.READWRITE, false, false, false, false, null);
			list.put("cmis:secondaryObjectTypeIds", secondaryObjectTypeIds);
			PropertyDefinitionImpl<?> description = new PropertyDefinitionImpl("cmis:description", "description",
					"description", "cmis:description", "cmis:description", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
			list.put("cmis:description", description);
			return list;
		}

		@SuppressWarnings("rawtypes")
		public static Map<String, PropertyDefinitionImpl<?>> getRelationExt() {
			Map<String, PropertyDefinitionImpl<?>> list = getBaseProperty();
			PropertyDefinitionImpl<?> source_table = new PropertyDefinitionImpl("source_table", "localName",
					"localNameSpace", "source_table", "source_table", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READWRITE, false, false, true, false, null);
			list.put("source_table", source_table);
			PropertyDefinitionImpl<?> target_table = new PropertyDefinitionImpl("target_table", "localName",
					"localNameSpace", "target_table", "target_table", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.ONCREATE, false, false, true, false, null);
			list.put("target_table", target_table);
			PropertyDefinitionImpl<?> source_column = new PropertyDefinitionImpl("source_column", "localName",
					"localNameSpace", "source_column", "source_column", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READWRITE, false, false, true, false, null);
			list.put("source_column", source_column);
			PropertyDefinitionImpl<?> target_column = new PropertyDefinitionImpl("target_column", "localName",
					"localNameSpace", "target_column", "target_column", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.ONCREATE, false, false, true, false, null);
			list.put("target_column", target_column);
			PropertyDefinitionImpl<?> copper_relationType = new PropertyDefinitionImpl("copper_relationType",
					"localName", "localNameSpace", "copper_relationType", "copper_relationType", "description",
					PropertyType.STRING, Cardinality.SINGLE, Updatability.ONCREATE, false, false, true, false, null);
			list.put("copper_relationType", copper_relationType);
			return list;
		}

		@SuppressWarnings("rawtypes")
		public static Map<String, PropertyDefinitionImpl<?>> getConfigExt() {
			Map<String, PropertyDefinitionImpl<?>> list = getBaseProperty();
			PropertyDefinitionImpl<?> configDetails = new PropertyDefinitionImpl("configDetails", "localName",
					"localNameSpace", "configDetails", "configDetails", "description", PropertyType.STRING,
					Cardinality.SINGLE, Updatability.READWRITE, false, false, true, false, null);
			list.put("configDetails", configDetails);
			return list;
		}

		/**
		 * create a custom type
		 * 
		 * @throws CmisException
		 */
		public static TypeDefinition createType(String repositoryId, TypeDefinition type, ExtensionsData extension,
				IUserObject userObject) throws IllegalArgumentException {
			if (type == null) {
				LOG.error("Type must be set! in repository: {}", repositoryId);
				throw new IllegalArgumentException("Type must be set!");
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = checkCrudPermission(typePermissionFlow, repositoryId,
					userObject == null ? null : userObject.getGroups(), type.getId(), TypePermissionType.WRITE);
			if (permission) {
				TypeMutabilityImpl typeMutability = null;
				Map<String, PropertyDefinitionImpl<?>> Mproperty = null;
				List<TypeDefinition> innerChild = new ArrayList<TypeDefinition>();
				innerChild.clear();
				MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MTypeManagerDAO.class);
				TypeDefinition object = null;
				if (type.getId() == null || type.getId().trim().length() == 0) {
					LOG.error("Type must have a valid id! in repository: {}", repositoryId);
					throw new IllegalArgumentException("Type must have a valid id!");
				}
				if (type.getParentTypeId() == null || type.getParentTypeId().trim().length() == 0) {
					LOG.error("Type must have a valid parent id! in repository: {}", repositoryId);
					throw new IllegalArgumentException("Type must have a valid parent id!");
				}

				List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
						Arrays.asList(type.getId()), null);
				if (typeDef != null && typeDef.size() > 0) {
					object = typeDef.get(0);
				}
				if (object != null) {
					LOG.error(type.getId() + ": {}, repository: {}", " is already present!", repositoryId);
					throw new IllegalArgumentException(type.getId() + " is already present");
				}
				if (type.getPropertyDefinitions() != null) {
					Map<String, PropertyDefinition<?>> property = type.getPropertyDefinitions();
					// Set<String> propId = property.keySet();
					// for (String id : propId) {
					// Map<String, PropertyDefinition<?>> propValues =
					// typeManagerDAO.getAllPropertyById(id);
					// if (propValues != null) {
					// LOG.error("Property" + id, " duplicate there!");
					// throw new IllegalArgumentException("Property" + id +
					// "duplicate there");
					// }
					// }
					Mproperty = property.entrySet().stream().filter(t -> t.getValue().getId() != null)
							.collect(Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null),
									(u, v) -> {
										throw new IllegalStateException(String.format("Duplicate key %s", u));
									}, LinkedHashMap::new));
				}
				typeMutability = new TypeMutabilityImpl(true, false, true);
				LOG.info("Successfully added new type: {}", type.getId());
				addIndex(repositoryId, Mproperty);
				if (type.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {
					DocumentTypeDefinition doctype = (DocumentTypeDefinition) type;
					DocumentTypeDefinition newType = getDocumentTypeDefinition(typeManagerDAO, doctype, Mproperty,
							typeMutability);
					typeManagerDAO.commit(newType);
					CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, newType.getId(),
							newType);
					try {
						createFolderForType(type, userObject, repositoryId);
					} catch (IOException e) {
						typeManagerDAO.delete(type.getId());
						LOG.error("Type folder creation exception:  {}, repository: {}", e, repositoryId);
						throw new IllegalArgumentException(e.getMessage());
					}
					TypeDefinition getType = gettingAllTypeDefinition(repositoryId, newType, null, userObject);
					return getType;
				} else {
					TypeDefinition newType = getTypeDefinitionManager(typeManagerDAO, type, Mproperty, typeMutability);
					typeManagerDAO.commit(newType);
					CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, newType.getId(),
							newType);
					try {
						if (type.getBaseTypeId() != BaseTypeId.CMIS_FOLDER) {
							createFolderForType(type, userObject, repositoryId);
						}
					} catch (IOException e) {
						typeManagerDAO.delete(type.getId());
						LOG.error("Type  folder creation exception:  {}, repository: {}", e, repositoryId);
						throw new IllegalArgumentException(e.getMessage());
					}
					TypeDefinition getType = gettingAllTypeDefinition(repositoryId, newType, null, userObject);
					return getType;
				}
			} else {
				LOG.error("Create type permission denied for this user: {}, repository: {}", userObject.getUserDN(),
						repositoryId);
				throw new CmisPermissionDeniedException(
						"Create type permission denied for this userId" + userObject.getUserDN());
			}

		}

		/**
		 * Update a type
		 * 
		 * @throws CmisException
		 */
		public static TypeDefinition updateType(String repositoryId, TypeDefinition type, ExtensionsData extension,
				IUserObject userObject) throws IllegalArgumentException {
			if (type == null) {
				LOG.error("Type must be set in repository: {}", repositoryId);
				throw new IllegalArgumentException("Type must be set!");
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = checkCrudPermission(typePermissionFlow, repositoryId,
					userObject == null ? null : userObject.getGroups(), type.getId(), TypePermissionType.WRITE);
			if (permission) {
				TypeMutabilityImpl typeMutability = null;
				Map<String, PropertyDefinitionImpl<?>> Mproperty = null;
				TypeDefinition object = null;

				if (type.getId() == null || type.getId().trim().length() == 0) {
					LOG.error("Type must have a valid id in  repository: {}", repositoryId);
					throw new IllegalArgumentException("Type must have a valid id!");
				}
				if (type.getParentTypeId() == null || type.getParentTypeId().trim().length() == 0) {
					LOG.error("Type must have a valid parent id repository: {}", repositoryId);
					throw new IllegalArgumentException("Type must have a valid parent id!");
				}
				MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MTypeManagerDAO.class);

				List<? extends TypeDefinition> tyeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
						Arrays.asList(type.getId()), null);
				if (tyeDef.size() > 0) {
					object = tyeDef.get(0);
				}
				if (object == null) {
					LOG.error(type.getId() + ": {}, repository: {}", " is unknown", repositoryId);
					throw new IllegalArgumentException("Unknown TypeId" + type.getId());
				}
				if (type.getPropertyDefinitions() != null) {
					Map<String, PropertyDefinition<?>> property = type.getPropertyDefinitions();
					Mproperty = property.entrySet().stream().filter(t -> t.getValue().getId() != null)
							.collect(Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null),
									(u, v) -> {
										throw new IllegalStateException(String.format("Duplicate key %s", u));
									}, LinkedHashMap::new));
				}

				typeMutability = new TypeMutabilityImpl(false, true, true);
				LOG.info("Successfully updated type: {}", type.getId());
				addIndex(repositoryId, Mproperty);
				if (type.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {
					DocumentTypeDefinition doctype = (DocumentTypeDefinition) type;
					DocumentTypeDefinition newType = getDocumentTypeDefinition(typeManagerDAO, doctype, Mproperty,
							typeMutability);
					typeManagerDAO.commit(newType);
					CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, newType.getId(),
							newType);
				} else {
					TypeDefinition newType = getTypeDefinitionManager(typeManagerDAO, type, Mproperty, typeMutability);
					typeManagerDAO.commit(newType);
					CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId, newType.getId(),
							newType);
				}
				TypeDefinition getType = getTypeDefinition(repositoryId, type.getId(), extension, userObject);
				return getType;
			} else {
				LOG.error("Update type permission denied for this user: {}, repository: {}", userObject.getUserDN(),
						repositoryId);
				throw new CmisPermissionDeniedException(
						"Update type permission denied for this userId" + userObject.getUserDN());
			}

		}

		/**
		 * delete a type
		 * 
		 * @throws CmisException
		 */
		public static void deleteType(String repositoryId, String type, ExtensionsData extension,
				IUserObject userObject) throws IllegalArgumentException {

			if (type == null) {
				LOG.error("Type is not available to delete: {}, repository: {}", type, repositoryId);
				throw new IllegalArgumentException("Type must be set!");
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			boolean permission = checkCrudPermission(typePermissionFlow, repositoryId,
					userObject == null ? null : userObject.getGroups(), type, TypePermissionType.DELETE);
			if (permission) {
				TypeDefinition object = null;

				MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MTypeManagerDAO.class);
				MBaseObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MBaseObjectDAO.class);

				List<? extends TypeDefinition> tyeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
						Arrays.asList(type), null);
				if (tyeDef.size() > 0) {
					object = tyeDef.get(0);
				}

				if (object == null) {
					LOG.error(type + ": {}, repository: {}", " does not exists", repositoryId);
					throw new IllegalArgumentException("Unknown TypeId " + type);
				}
				// Map<String, String> parameters =
				// RepositoryManager.get().getFileDetails(repositoryId);
				// IStorageService localService =
				// MongoStorageDocument.createStorageService(parameters,
				// repositoryId, type);
				// localService.deleteFolder(parameters, repositoryId, type);
				IBaseObject folderObject = DBUtils.BaseDAO.getByPath(repositoryId, "/" + type);
				if (folderObject != null) {
					baseMorphiaDAO.delete(folderObject.getId(), true, null);
				}
				typeManagerDAO.delete(type);
				CacheProviderServiceFactory.getTypeCacheServiceProvider().remove(repositoryId, type);
				LOG.info("Successfully deleted type: {}", type);
			} else {
				LOG.error("Delete type permission denied for this user: {}, repository: {}", userObject.getUserDN(),
						repositoryId);
				throw new CmisPermissionDeniedException(
						"Delete type permission denied for this userId" + userObject.getUserDN());
			}
		}

		public static TypeDefinition getTypeDefinition(String repositoryId, String typeId, ExtensionsData extension,
				IUserObject userObject) {
			if (typeId == null) {
				LOG.error("getTypeDefinition typeId should not be null in repository: {}", repositoryId);
				throw new IllegalArgumentException("Type must be set!");
			}
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			TypeDefinition typeDefinition = null;
			List<? extends TypeDefinition> typeDef = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(), typeId);
			if (typeDef != null && typeDef.size() > 0) {
				typeDefinition = typeDef.get(0);
			}

			if (typeDefinition == null) {
				// MTypeMutability typeMutability = new MTypeMutability(false,
				// false, false);
				// Map<String, MPropertyDefinition<?>> propertyDefinition = new
				// HashMap<>();
				// MCmisDocumentTypeDefinition TypeDefinition = new
				// MCmisDocumentTypeDefinition("", "", "", "", "", "",
				// BaseTypeId.CMIS_DOCUMENT, "", false, false, false, false,
				// false, false, false, false,
				// typeMutability, propertyDefinition, null, null);
				// resultDocument = getDocumentTypeDefinition(TypeDefinition);
				// typeDefinitionContainer = new
				// TypeDefinitionContainerImpl(resultDocument);
				// TypeDefinition emptyTypeDefinition =
				// typeDefinitionContainer.getTypeDefinition();
				// return emptyTypeDefinition;
				LOG.error("getTypeDefinition typeId should not be null in repository: {}", repositoryId);
				throw new CmisObjectNotFoundException("Type must be set!");

			}
			return gettingAllTypeDefinition(repositoryId, typeDefinition, typePermissionFlow, userObject);

		}

		private static TypeDefinition gettingAllTypeDefinition(String repositoryId, TypeDefinition typeDefinition,
				ITypePermissionService typePermissionFlow, IUserObject userObject) {

			LOG.debug("gettingAllTypeDefinition for this typeId: {}", typeDefinition.getId());
			CmisDocumentTypeDefinitionImpl resultDocument = null;
			CmisFolderTypeDefinitionImpl resultFolder = null;
			ItemTypeDefinitionImpl resultItem = null;
			CmisPolicyTypeDefinitionImpl resultPolicy = null;
			CmisRelationshipTypeDefinitionImpl resultRelationship = null;
			CmisSecondaryTypeDefinitionImpl resultSecondary = null;
			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			List<TypeDefinition> innerChild = new ArrayList<TypeDefinition>();
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			innerChild.clear();
			if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {

				DocumentTypeDefinition docType = getDocumentDefinitionWithTypePermission(typePermissionFlow,
						repositoryId, userObject == null ? null : userObject.getGroups(),
						typeDefinition.getId().toString());
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null, typePermissionFlow, userObject);
				CmisDocumentTypeDefinitionImpl documentType = getTypeDocumentObjectInstance(docType, list);

				resultDocument = getDocumentTypeDefinition(documentType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultDocument);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_FOLDER) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null, typePermissionFlow, userObject);
				TypeDefinition folderType = getTypeObjectInstance(typeDefinition, list, typeManagerDAO);

				resultFolder = getFolderTypeDefinition(folderType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultFolder);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_ITEM) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null, typePermissionFlow, userObject);
				TypeDefinition itemType = getTypeObjectInstance(typeDefinition, list, typeManagerDAO);
				resultItem = getItemTypeDefinition(itemType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultItem);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_POLICY) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null, typePermissionFlow, userObject);
				TypeDefinition policyType = getTypeObjectInstance(typeDefinition, list, typeManagerDAO);
				resultPolicy = getPolicyTypeDefinition(policyType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultPolicy);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_SECONDARY) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null, typePermissionFlow, userObject);
				TypeDefinition secondaryType = getTypeObjectInstance(typeDefinition, list, typeManagerDAO);
				resultSecondary = getSecondaryTypeDefinition(secondaryType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultSecondary);
				typeDefinitionContainer.setChildren(null);
			}
			if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_RELATIONSHIP) {

				List<String> sourceIds = new ArrayList<>();
				List<String> targetIds = new ArrayList<>();
				Map<String, PropertyDefinitionImpl<?>> list = new HashMap<>();
				if (typeDefinition.getPropertyDefinitions() != null) {
					Map<String, PropertyDefinition<?>> property = typeDefinition.getPropertyDefinitions();
					for (PropertyDefinition<?> pro : property.values()) {
						PropertyDefinitionImpl<?> propertyDefinition = getPropertyDefinition(pro, null);
						if (propertyDefinition.getLocalNamespace().equalsIgnoreCase("sourceId")) {
							if (!propertyDefinition.getId().equals(PropertyIds.SOURCE_ID)) {
								sourceIds.add(propertyDefinition.getId());
							}

						} else if (propertyDefinition.getLocalNamespace().equalsIgnoreCase("targetId")) {
							if (!propertyDefinition.getId().equals(PropertyIds.TARGET_ID)) {
								targetIds.add(propertyDefinition.getId());
							}
						}
						list.put(pro.getId(), propertyDefinition);
					}
				}

				if (typeDefinition.getParentTypeId() != null) {
					List<TypeDefinition> childTypes = getTypeParent(repositoryId, typeDefinition.getParentTypeId(),
							innerChild, typePermissionFlow, userObject);
					if (childTypes != null) {
						for (TypeDefinition parentObject : childTypes) {
							if (parentObject.getPropertyDefinitions() != null) {
								Map<String, PropertyDefinition<?>> property = parentObject.getPropertyDefinitions();
								for (PropertyDefinition<?> pro : property.values()) {
									PropertyDefinitionImpl<?> propertyDefinition = getPropertyDefinition(pro, true);
									list.put(pro.getId(), propertyDefinition);
								}
							}
						}
					}
				}

				TypeDefinition resultrelationshipType = getTypeObjectInstance(typeDefinition, list, typeManagerDAO);

				resultRelationship = getRelationshipTypeDefinitionWithSourceTarget(resultrelationshipType, sourceIds,
						targetIds);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultRelationship);
				typeDefinitionContainer.setChildren(null);
			}
			if (typeDefinitionContainer != null) {
				typeDefinitionContainer.getTypeDefinition().getPropertyDefinitions().size();
				return typeDefinitionContainer.getTypeDefinition();
			} else {
				LOG.error("gettingAllTypeDefinition unknown typeId: {}, repository: {}", typeDefinition.getId(),
						repositoryId);
				throw new CmisObjectNotFoundException("unknown typeId: " + typeDefinition.getId());
			}
		}

		private static Map<String, PropertyDefinitionImpl<?>> getTypeProperties(TypeDefinition typeDefinition,
				String repositoryId, List<TypeDefinition> innerChild, Boolean includeProperty,
				ITypePermissionService typePermissionFlow, IUserObject userObject) {

			LOG.debug("getting type properties for: {}", typeDefinition != null ? typeDefinition.getId() : null);

			boolean incluePro = includeProperty == null ? true : includeProperty;
			Map<String, PropertyDefinitionImpl<?>> listProperty = null;

			if (incluePro) {
				Map<String, PropertyDefinitionImpl<?>> parentPropertyDefinition = new HashMap<>();
				Map<String, PropertyDefinitionImpl<?>> ownPropertyDefinition = null;
				if (typeDefinition.getPropertyDefinitions() != null) {
					Map<String, PropertyDefinition<?>> property = typeDefinition.getPropertyDefinitions();
					ownPropertyDefinition = property.entrySet().stream().collect(Collectors.toMap(p -> p.getKey(),
							p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
								throw new IllegalStateException(String.format("Duplicate key %s", u));
							}, LinkedHashMap::new));
				}
				if (typeDefinition.getParentTypeId() != null) {
					if (innerChild != null) {
						List<TypeDefinition> childTypes = getTypeParent(repositoryId, typeDefinition.getParentTypeId(),
								innerChild, typePermissionFlow, userObject);
						if (childTypes != null) {
							for (TypeDefinition parentObject : childTypes) {
								if (parentObject.getPropertyDefinitions() != null) {
									Map<String, PropertyDefinition<?>> property = parentObject.getPropertyDefinitions();
									Map<String, PropertyDefinitionImpl<?>> parentPropertyDefinitions = property
											.entrySet().stream().collect(Collectors.toMap(p -> p.getKey(),
													p -> getPropertyDefinition(p.getValue(), true), (u, v) -> {
														throw new IllegalStateException(
																String.format("Duplicate key %s", u));
													}, LinkedHashMap::new));
									parentPropertyDefinition = Stream
											.of(parentPropertyDefinition, parentPropertyDefinitions)
											.flatMap(m -> m.entrySet().stream())
											.collect(Collectors.toMap(Entry::getKey, Entry::getValue, (u, v) -> {
												throw new IllegalStateException(String.format("Duplicate key %s", u));
											}, LinkedHashMap::new));
								}
							}
						}
					}
				}
				if (parentPropertyDefinition == null) {
					listProperty = new LinkedHashMap<>(ownPropertyDefinition);
				} else if (ownPropertyDefinition == null) {
					listProperty = new LinkedHashMap<>(parentPropertyDefinition);
				} else {
					listProperty = Stream.of(ownPropertyDefinition, parentPropertyDefinition)
							.flatMap(m -> m.entrySet().stream())
							.collect(Collectors.toMap(Entry::getKey, Entry::getValue, (u, v) -> {
								throw new IllegalStateException(String.format("Duplicate key %s", u));
							}, LinkedHashMap::new));
				}

			}
			return listProperty;
		}

		private static TypeDefinition getTypeObjectInstance(TypeDefinition typeDefinition,
				Map<String, PropertyDefinitionImpl<?>> list, MTypeManagerDAO typeManagerDAO) {
			TypeDefinition type = typeManagerDAO.createObjectFacade(typeDefinition.getId(),
					typeDefinition.getLocalName(), typeDefinition.getLocalNamespace(), typeDefinition.getDisplayName(),
					typeDefinition.getQueryName(), typeDefinition.getDescription(), typeDefinition.getBaseTypeId(),
					typeDefinition.getParentTypeId(), typeDefinition.isCreatable(), typeDefinition.isFileable(),
					typeDefinition.isQueryable(), typeDefinition.isFulltextIndexed(),
					typeDefinition.isIncludedInSupertypeQuery(), typeDefinition.isControllablePolicy(),
					typeDefinition.isControllableAcl(), typeDefinition.getTypeMutability(), list, null, null);
			return type;

		}

		private static CmisDocumentTypeDefinitionImpl getTypeDocumentObjectInstance(
				DocumentTypeDefinition typeDefinition, Map<String, PropertyDefinitionImpl<?>> list) {
			CmisDocumentTypeDefinitionImpl type = new CmisDocumentTypeDefinitionImpl(typeDefinition.getId(),
					typeDefinition.getLocalName(), typeDefinition.getLocalNamespace(), typeDefinition.getDisplayName(),
					typeDefinition.getQueryName(), typeDefinition.getDescription(), typeDefinition.getBaseTypeId(),
					typeDefinition.getParentTypeId(), typeDefinition.isCreatable(), typeDefinition.isFileable(),
					typeDefinition.isQueryable(), typeDefinition.isFulltextIndexed(),
					typeDefinition.isIncludedInSupertypeQuery(), typeDefinition.isControllablePolicy(),
					typeDefinition.isControllableAcl(), (TypeMutabilityImpl) typeDefinition.getTypeMutability(), list,
					typeDefinition.isVersionable(), typeDefinition.getContentStreamAllowed());
			return type;
		}

		/**
		 * get TypeDefinition using properties
		 */
		public static TypeDefinition getTypeRelationshipDefinition(String repositoryId, String typeId, boolean cmis11,
				IUserObject userObject) {
			LOG.info("getTypeRelationshipDefinition for type: {}, repository: {}", typeId, repositoryId);
			if (typeId == null) {
				LOG.error("getTypeRelationshipDefinition typeId should not be null in repository: {}", repositoryId);
				throw new IllegalArgumentException("Type must be set!");
			}
			List<TypeDefinition> innerChild = new ArrayList<TypeDefinition>();
			innerChild.clear();
			TypeDefinition typeDefinition = null;
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			List<? extends TypeDefinition> typeDef = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(), typeId);
			if (typeDef != null && typeDef.size() > 0) {
				typeDefinition = typeDef.get(0);
			}
			return typeDefinition;
		}

		/**
		 * get Type children for particular TypeID
		 */
		public static TypeDefinitionList getTypeChildren(String repositoryId, String typeId,
				Boolean includePropertyDefinitions, BigInteger maxItems, BigInteger skipCount, ExtensionsData extension,
				IUserObject userObject) throws IllegalArgumentException {
			boolean inclPropDefs = includePropertyDefinitions == null ? false : includePropertyDefinitions;
			int skip = skipCount == null ? 0 : skipCount.intValue();
			int max = maxItems == null ? -1 : maxItems.intValue();
			TypeDefinition object = null;
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			if (typeId != null) {
				List<? extends TypeDefinition> typeDef = getTypeDefinitionWithTypePermission(typePermissionFlow,
						repositoryId, userObject == null ? null : userObject.getGroups(), typeId);
				if (typeDef != null && typeDef.size() > 0) {
					object = typeDef.get(0);
				}
				if (object == null) {
					LOG.error("getTypeChildren unknown TypeId : {}, repository: {}", typeId, repositoryId);
					throw new IllegalArgumentException("Unknown TypeID " + typeId);
				}
			}
			return getTypeChildrenIntern(repositoryId, typeId, inclPropDefs, max, skip, object, typePermissionFlow,
					userObject);
		}

		public static TypeDefinitionListImpl getTypeChildrenIntern(String repositoryId, String typeId,
				Boolean includePropertyDefinitions, int maxItems, int skipCount, TypeDefinition object,
				ITypePermissionService typePermissionFlow, IUserObject userObject) {
			TypeDefinitionListImpl result = new TypeDefinitionListImpl();
			if (typeId != null) {
				List<? extends TypeDefinition> childrenList = DBUtils.TypeServiceDAO.getChildrenIds(repositoryId,
						typeId, maxItems, skipCount);
				if (childrenList.size() > 0) {
					result.setNumItems(BigInteger.valueOf(childrenList.size()));
					result.setHasMoreItems(childrenList.size() > maxItems - skipCount);
					List<TypeDefinition> resultTypes = childrenList.stream()
							.filter(t -> typePermissionFlow != null
									? typePermissionFlow.checkTypeAccess(repositoryId,
											userObject.getGroups() == null ? null : userObject.getGroups(), t.getId())
									: true)
							.map(t -> getPropertyIncludeObject(repositoryId, t, includePropertyDefinitions,
									typePermissionFlow, userObject))
							.collect(Collectors.<TypeDefinition>toList());
					result.setList(resultTypes);
				} else {
					result.setHasMoreItems(false);
					result.setNumItems(BigInteger.valueOf(childrenList.size()));
					result.setList(Collections.<TypeDefinition>emptyList());
				}

			} else {
				if (skipCount >= 6) {
					result.setHasMoreItems(false);
					result.setNumItems(BigInteger.valueOf(0));
					result.setList(Collections.<TypeDefinition>emptyList());
				} else {
					List<TypeDefinition> resultTypes = new ArrayList<>();
					List<? extends TypeDefinition> folderType = getTypeDefinitionWithTypePermission(typePermissionFlow,
							repositoryId, userObject == null ? null : userObject.getGroups(),
							BaseTypeId.CMIS_FOLDER.value());
					if (folderType != null) {
						resultTypes.add(getPropertyIncludeObject(repositoryId, folderType.get(0),
								includePropertyDefinitions, typePermissionFlow, userObject));
					}
					DocumentTypeDefinition documentType = getDocumentDefinitionWithTypePermission(typePermissionFlow,
							repositoryId, userObject == null ? null : userObject.getGroups(),
							BaseTypeId.CMIS_DOCUMENT.value());
					if (documentType != null) {
						resultTypes.add(getPropertyIncludeObject(repositoryId, documentType, includePropertyDefinitions,
								typePermissionFlow, userObject));
					}
					List<? extends TypeDefinition> itemType = getTypeDefinitionWithTypePermission(typePermissionFlow,
							repositoryId, userObject == null ? null : userObject.getGroups(),
							BaseTypeId.CMIS_ITEM.value());
					if (itemType != null) {
						resultTypes.add(getPropertyIncludeObject(repositoryId, itemType.get(0),
								includePropertyDefinitions, typePermissionFlow, userObject));
					}
					List<? extends TypeDefinition> policyType = getTypeDefinitionWithTypePermission(typePermissionFlow,
							repositoryId, userObject == null ? null : userObject.getGroups(),
							BaseTypeId.CMIS_POLICY.value());
					if (policyType != null) {
						resultTypes.add(getPropertyIncludeObject(repositoryId, policyType.get(0),
								includePropertyDefinitions, typePermissionFlow, userObject));
					}
					List<? extends TypeDefinition> relationshipType = getTypeDefinitionWithTypePermission(
							typePermissionFlow, repositoryId, userObject == null ? null : userObject.getGroups(),
							BaseTypeId.CMIS_RELATIONSHIP.value());
					if (relationshipType != null) {
						resultTypes.add(getPropertyIncludeObject(repositoryId, relationshipType.get(0),
								includePropertyDefinitions, typePermissionFlow, userObject));
					}
					List<? extends TypeDefinition> secondaryType = getTypeDefinitionWithTypePermission(
							typePermissionFlow, repositoryId, userObject == null ? null : userObject.getGroups(),
							BaseTypeId.CMIS_SECONDARY.value());
					if (secondaryType != null) {
						resultTypes.add(getPropertyIncludeObject(repositoryId, secondaryType.get(0),
								includePropertyDefinitions, typePermissionFlow, userObject));
					}
					result.setNumItems(BigInteger.valueOf(resultTypes.size()));
					result.setHasMoreItems(true);
					result.setList(resultTypes);
				}
			}

			return result;
		}

		private static TypeDefinition getPropertyIncludeObject(String repositoryId, TypeDefinition type,
				Boolean includePropertyDefinition, ITypePermissionService typePermission, IUserObject userObject) {
			return getInnerTypeDefinitionContainerImpl(repositoryId, type, includePropertyDefinition, typePermission,
					userObject).getTypeDefinition();
		}

		/**
		 * getting the TypeDescendants for particular TypeID
		 */
		public static List<TypeDefinitionContainer> getTypeDescendants(String repositoryId, String typeId,
				BigInteger depth, Boolean includePropertyDefinitions, ExtensionsData extension, IUserObject userObject)
				throws IllegalArgumentException, CmisInvalidArgumentException {
			boolean inclPropDefs = includePropertyDefinitions == null ? true : includePropertyDefinitions;
			MDocumentTypeManagerDAO docTypeMorphia = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
			ITypePermissionService typePermissionFlow = TypeServiceFactory
					.createTypePermissionFlowService(repositoryId);
			if (typeId != null) {
				TypeDefinition object = null;
				List<? extends TypeDefinition> typeDef = getTypeDefinitionWithTypePermission(typePermissionFlow,
						repositoryId, userObject == null ? null : userObject.getGroups(), typeId);
				if (typeDef != null && typeDef.size() > 0) {
					object = typeDef.get(0);
				}
				if (object == null) {
					LOG.error("getTypeDescendants unknown typeID : {}, repository: {}", typeId, repositoryId);
					throw new IllegalArgumentException("Unknown typeID " + typeId);
				}

			}

			List<TypeDefinitionContainer> result = null;
			// TODO:implement vresion
			// boolean cmis11 = context.getCmisVersion() !=
			// CmisVersion.CMIS_1_0;
			boolean cmis11 = false;
			if (typeId != null) {
				TypeDefinitionContainer tc = getTypeById(repositoryId, typeId, inclPropDefs,
						depth == null ? -1 : depth.intValue(), cmis11, typePermissionFlow, userObject);
				if (tc == null) {
					throw new CmisInvalidArgumentException("unknown type id: " + typeId);
				} else {
					result = tc.getChildren();
				}
			} else {
				result = getBaseTyeDefCon(repositoryId, docTypeMorphia, depth == null ? -1 : depth.intValue(),
						includePropertyDefinitions, typePermissionFlow, userObject);
			}
			if (result == null) {
				LOG.error("getTypeDescendants unknown typeId: {}, repository: {}", typeId, repositoryId);
				throw new CmisInvalidArgumentException("unknown typeId: " + typeId);
			}

			return result;
		}

		/**
		 * getting TypeDefinition for particular TypeID
		 */
		public static TypeDefinitionContainerImpl getTypeById(String repositoryId, String typeId,
				boolean includePropertyDefinitions, int depthParam, boolean cmis11,
				ITypePermissionService typePermissionFlow, IUserObject userObject) {
			LOG.info("getTypeById for type: {} , repository: {}", typeId, repositoryId);
			List<TypeDefinitionContainer> innerChild = new ArrayList<TypeDefinitionContainer>();
			innerChild.clear();
			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			TypeDefinition result = null;

			List<? extends TypeDefinition> typeDef = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(), typeId);
			if (typeDef != null && typeDef.size() > 0) {
				result = typeDef.get(0);
				if (result.getBaseTypeId().value().equals(BaseTypeId.CMIS_DOCUMENT.value())) {
					DocumentTypeDefinition docResult = getDocumentDefinitionWithTypePermission(typePermissionFlow,
							repositoryId, userObject == null ? null : userObject.getGroups(), typeId);
					Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(docResult, repositoryId, null, null,
							typePermissionFlow, userObject);
					typeDefinitionContainer = getDocTypeDefContainer(
							getDocTypeObject(repositoryId, getTypeDocumentObjectInstance(docResult, list),
									includePropertyDefinitions, typePermissionFlow, userObject));
				} else {
					typeDefinitionContainer = getTypeDefContainer(getTypeObject(repositoryId, result,
							includePropertyDefinitions, typePermissionFlow, userObject),
							result.getBaseTypeId().value());
				}
				typeDefinitionContainer.setChildren(getChildTypeDefContainer(repositoryId, typeId, innerChild,
						depthParam, includePropertyDefinitions, typePermissionFlow, userObject));

			}
			return typeDefinitionContainer;

		}

		private static List<TypeDefinitionContainer> getChildTypeDefContainer(String repositoryId, String typeId,
				List<TypeDefinitionContainer> innerChild, int depth, Boolean includePropertyDefinitions,
				ITypePermissionService typePermissionFlow, IUserObject userObject) {
			List<TypeDefinitionContainer> childTypes = null;
			List<? extends TypeDefinition> childrenList = DBUtils.TypeServiceDAO.getChildrenIds(repositoryId, typeId,
					depth, -1);
			for (TypeDefinition child : childrenList) {
				if (child.getId() != null) {
					if (typePermissionFlow != null
							? typePermissionFlow.checkTypeAccess(repositoryId,
									userObject.getGroups() != null ? userObject.getGroups() : null, child.getId())
							: true) {
						childTypes = getTypeDesChildrens(repositoryId, child, innerChild, depth,
								includePropertyDefinitions, typePermissionFlow, userObject);
					}

				}
			}

			return childTypes;
		}

		private static List<TypeDefinitionContainer> getBaseTyeDefCon(String repositoryId,
				MDocumentTypeManagerDAO docTypeMorphia, int depth, Boolean includePropertyDefinitions,
				ITypePermissionService typePermissionFlow, IUserObject userObject) {
			List<TypeDefinitionContainer> object = new ArrayList<TypeDefinitionContainer>();
			List<? extends TypeDefinition> folder = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(), BaseTypeId.CMIS_FOLDER.value());
			DocumentTypeDefinition document = getDocumentDefinitionWithTypePermission(typePermissionFlow, repositoryId,
					userObject == null ? null : userObject.getGroups(), BaseTypeId.CMIS_DOCUMENT.value());
			List<? extends TypeDefinition> policy = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(), BaseTypeId.CMIS_POLICY.value());
			List<? extends TypeDefinition> relationship = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(),
					BaseTypeId.CMIS_RELATIONSHIP.value());
			List<? extends TypeDefinition> item = getTypeDefinitionWithTypePermission(typePermissionFlow, repositoryId,
					userObject == null ? null : userObject.getGroups(), BaseTypeId.CMIS_ITEM.value());
			List<? extends TypeDefinition> secondary = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(),
					BaseTypeId.CMIS_SECONDARY.value());
			if (folder != null) {
				TypeDefinitionContainerImpl typeFolderDefinitionContainer = getTypeDefinitionContainerImpl(repositoryId,
						folder.get(0), docTypeMorphia, depth, includePropertyDefinitions, typePermissionFlow,
						userObject);
				object.add(typeFolderDefinitionContainer);
			}
			if (document != null) {
				TypeDefinitionContainerImpl typeDocumentDefinitionContainer = getDocTypeDefinitionContainerImpl(
						repositoryId, document, docTypeMorphia, depth, includePropertyDefinitions, typePermissionFlow,
						userObject);
				object.add(typeDocumentDefinitionContainer);
			}
			if (policy != null) {
				TypeDefinitionContainerImpl typePolicyDefinitionContainer = getTypeDefinitionContainerImpl(repositoryId,
						policy.get(0), docTypeMorphia, depth, includePropertyDefinitions, typePermissionFlow,
						userObject);
				object.add(typePolicyDefinitionContainer);
			}
			if (relationship != null) {
				TypeDefinitionContainerImpl typeRelationshipDefinitionContainer = getTypeDefinitionContainerImpl(
						repositoryId, relationship.get(0), docTypeMorphia, depth, includePropertyDefinitions,
						typePermissionFlow, userObject);
				object.add(typeRelationshipDefinitionContainer);
			}
			if (item != null) {
				TypeDefinitionContainerImpl typeItemDefinitionContainer = getTypeDefinitionContainerImpl(repositoryId,
						item.get(0), docTypeMorphia, depth, includePropertyDefinitions, typePermissionFlow, userObject);
				object.add(typeItemDefinitionContainer);
			}

			if (secondary != null) {
				TypeDefinitionContainerImpl typesecondaryDefinitionContainer = getTypeDefinitionContainerImpl(
						repositoryId, secondary.get(0), docTypeMorphia, depth, includePropertyDefinitions,
						typePermissionFlow, userObject);
				object.add(typesecondaryDefinitionContainer);
			}

			return object;
		}

		private static TypeDefinitionContainerImpl getTypeDefinitionContainerImpl(String repositoryId,
				TypeDefinition object, MDocumentTypeManagerDAO docTypeMorphia, int depth,
				Boolean includePropertyDefinitions, ITypePermissionService typePermissionFlow, IUserObject userObject) {
			TypeDefinitionContainerImpl typeItemDefinitionContainer = getTypeDefContainer(
					getTypeObject(repositoryId, object, includePropertyDefinitions, typePermissionFlow, userObject),
					object.getBaseTypeId().value());
			List<TypeDefinitionContainer> innerItemChild = new ArrayList<TypeDefinitionContainer>();
			innerItemChild.clear();
			typeItemDefinitionContainer.setChildren(getChildTypeDefContainer(repositoryId, object.getId(),
					innerItemChild, depth, includePropertyDefinitions, typePermissionFlow, userObject));
			return typeItemDefinitionContainer;
		}

		private static TypeDefinitionContainerImpl getDocTypeDefinitionContainerImpl(String repositoryId,
				DocumentTypeDefinition object, MDocumentTypeManagerDAO docTypeMorphia, int depth,
				Boolean includePropertyDefinitions, ITypePermissionService typePermissionFlow, IUserObject userObject) {
			TypeDefinitionContainerImpl typeItemDefinitionContainer = getDocTypeDefContainer(
					getDocTypeObject(repositoryId, object, includePropertyDefinitions, typePermissionFlow, userObject));
			List<TypeDefinitionContainer> innerItemChild = new ArrayList<TypeDefinitionContainer>();
			innerItemChild.clear();
			typeItemDefinitionContainer.setChildren(getChildTypeDefContainer(repositoryId, object.getId(),
					innerItemChild, depth, includePropertyDefinitions, typePermissionFlow, userObject));
			return typeItemDefinitionContainer;
		}

		/**
		 * getting TypeDescendants InnerChild for particular TypeID
		 */

		private static List<TypeDefinitionContainer> getTypeDesChildrens(String repositoryId, TypeDefinition child,
				List<TypeDefinitionContainer> innerChild, int depth, Boolean includePropertyDefinitions,
				ITypePermissionService typePermissionFlow, IUserObject userObject) {
			List<TypeDefinitionContainer> innerTypeChild = new ArrayList<>();
			TypeDefinitionContainerImpl typeDefinitionContainer = getInnerTypeDefinitionContainerImpl(repositoryId,
					child, includePropertyDefinitions, typePermissionFlow, userObject);
			List<? extends TypeDefinition> childrenList = DBUtils.TypeServiceDAO.getChildrenIds(repositoryId,
					child.getId(), depth, -1);
			if (childrenList.isEmpty()) {
				innerChild.add(typeDefinitionContainer);
			} else {
				for (TypeDefinition childType : childrenList) {
					if (childType != null) {
						if (typePermissionFlow != null ? typePermissionFlow.checkTypeAccess(repositoryId,
								userObject.getGroups() != null ? userObject.getGroups() : null, childType.getId())
								: true) {
							List<TypeDefinitionContainer> TypeChild = new ArrayList<>();
							TypeChild.clear();
							TypeDefinitionContainerImpl typeInnerDefinitionContainer = getInnerTypeDefinitionContainerImpl(
									repositoryId, childType, includePropertyDefinitions, typePermissionFlow,
									userObject);
							innerTypeChild.add(
									getTypeDesInnerChild(repositoryId, childType, depth, includePropertyDefinitions,
											TypeChild, typeInnerDefinitionContainer, typePermissionFlow, userObject));
						}

					}
				}
				typeDefinitionContainer.setChildren(innerTypeChild);
				innerChild.add(typeDefinitionContainer);
			}
			return innerChild;
		}

		private static TypeDefinitionContainerImpl getInnerTypeDefinitionContainerImpl(String repositoryId,
				TypeDefinition child, Boolean includePropertyDefinitions, ITypePermissionService typePermission,
				IUserObject userObject) {
			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			if (child.getBaseTypeId().value().equals(BaseTypeId.CMIS_DOCUMENT.value())) {
				DocumentTypeDefinition docType = getDocumentDefinitionWithTypePermission(typePermission, repositoryId,
						userObject == null ? null : userObject.getGroups(), child.getId());
				typeDefinitionContainer = getDocTypeDefContainer(getDocTypeObject(repositoryId, docType,
						includePropertyDefinitions, typePermission, userObject));
			} else {
				typeDefinitionContainer = getTypeDefContainer(
						getTypeObject(repositoryId, child, includePropertyDefinitions, typePermission, userObject),
						child.getBaseTypeId().value());
			}
			return typeDefinitionContainer;

		}

		private static TypeDefinitionContainer getTypeDesInnerChild(String repositoryId, TypeDefinition child,
				int depth, Boolean includePropertyDefinitions, List<TypeDefinitionContainer> typeChild,
				TypeDefinitionContainerImpl typeInnerDefinitionContainer, ITypePermissionService typePermissionFlow,
				IUserObject userObject) {
			List<? extends TypeDefinition> childrenList = DBUtils.TypeServiceDAO.getChildrenIds(repositoryId,
					child.getId(), depth, -1);
			if (childrenList.isEmpty()) {
				return typeInnerDefinitionContainer;
			} else {
				for (TypeDefinition childType : childrenList) {
					if (childType != null) {
						if (typePermissionFlow != null ? typePermissionFlow.checkTypeAccess(repositoryId,
								userObject.getGroups() != null ? userObject.getGroups() : null, childType.getId())
								: true) {
							List<TypeDefinitionContainer> typeInnerChild = new ArrayList<>();
							typeInnerChild.clear();
							TypeDefinitionContainerImpl typeInnerChildDefinitionContainer = getInnerTypeDefinitionContainerImpl(
									repositoryId, childType, includePropertyDefinitions, typePermissionFlow,
									userObject);
							typeChild.add(getTypeDesInnerChild(repositoryId, childType, depth,
									includePropertyDefinitions, typeInnerChild, typeInnerChildDefinitionContainer,
									typePermissionFlow, userObject));
						}

					}
				}
				typeInnerDefinitionContainer.setChildren(typeChild);
				return typeInnerDefinitionContainer;
			}

		}

		private static TypeDefinition getTypeObject(String repositoryId, TypeDefinition object,
				Boolean includePropertyDefinitions, ITypePermissionService typePermissionFlow, IUserObject userObject) {
			List<TypeDefinition> innerChildObject = new ArrayList<TypeDefinition>();
			innerChildObject.clear();
			Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(object, repositoryId, innerChildObject,
					includePropertyDefinitions, typePermissionFlow, userObject);
			MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			TypeDefinition resultType = getTypeObjectInstance(object, list, typeManagerDAO);
			return resultType;

		}

		private static CmisDocumentTypeDefinitionImpl getDocTypeObject(String repositoryId,
				DocumentTypeDefinition object, Boolean includePropertyDefinitions,
				ITypePermissionService typePermission, IUserObject userObject) {
			List<TypeDefinition> innerChildObject = new ArrayList<TypeDefinition>();
			innerChildObject.clear();
			Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(object, repositoryId, innerChildObject,
					includePropertyDefinitions, typePermission, userObject);
			CmisDocumentTypeDefinitionImpl resultType = getTypeDocumentObjectInstance(object, list);
			return resultType;

		}

		private static TypeDefinitionContainerImpl getTypeDefContainer(TypeDefinition resultType, String type) {

			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			CmisFolderTypeDefinitionImpl resultFolder = null;
			ItemTypeDefinitionImpl resultItem = null;
			CmisPolicyTypeDefinitionImpl resultPolicy = null;
			CmisSecondaryTypeDefinitionImpl resultSecondary = null;
			CmisRelationshipTypeDefinitionImpl resultRelationship = null;
			if (type.equalsIgnoreCase(BaseTypeId.CMIS_FOLDER.value())) {
				resultFolder = getFolderTypeDefinition(resultType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultFolder);
			} else if (type.equalsIgnoreCase(BaseTypeId.CMIS_ITEM.value())) {
				resultItem = getItemTypeDefinition(resultType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultItem);
			} else if (type.equalsIgnoreCase(BaseTypeId.CMIS_POLICY.value())) {
				resultPolicy = getPolicyTypeDefinition(resultType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultPolicy);
			} else if (type.equalsIgnoreCase(BaseTypeId.CMIS_RELATIONSHIP.value())) {
				resultRelationship = getRelationshipTypeDefinitionWithSourceTarget(resultType, null, null);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultRelationship);
			} else if (type.equalsIgnoreCase(BaseTypeId.CMIS_SECONDARY.value())) {
				resultSecondary = getSecondaryTypeDefinition(resultType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultSecondary);
			}
			return typeDefinitionContainer;

		}

		private static TypeDefinitionContainerImpl getDocTypeDefContainer(CmisDocumentTypeDefinitionImpl resultType) {
			CmisDocumentTypeDefinitionImpl resultDocument = getDocumentTypeDefinition(resultType);
			TypeDefinitionContainerImpl typeDefinitionContainer = new TypeDefinitionContainerImpl(resultDocument);
			return typeDefinitionContainer;
		}

		/**
		 * getting parent for particular TypeID
		 */
		public static List<TypeDefinition> getTypeParent(String repositoryId, String parentId,
				List<TypeDefinition> innerChild, ITypePermissionService typePermissionFlow, IUserObject userObject) {

			TypeDefinition parent = null;
			List<? extends TypeDefinition> typeDef = getTypeDefinitionWithTypePermission(typePermissionFlow,
					repositoryId, userObject == null ? null : userObject.getGroups(), parentId);
			if (typeDef != null && typeDef.size() > 0) {
				parent = typeDef.get(0);
			}
			if (parent != null) {
				LOG.debug("getTypeParent from repository: {}, parentTypeId: {}", repositoryId, parent.getId());
				if (parent.getParentTypeId() == null) {
					innerChild.add(parent);
				} else {
					innerChild.add(parent);
					return getTypeParent(repositoryId, parent.getParentTypeId(), innerChild, typePermissionFlow,
							userObject);
				}
			}

			return innerChild;
		}

		private static CmisDocumentTypeDefinitionImpl getDocumentTypeDefinition(CmisDocumentTypeDefinitionImpl result) {

			LOG.debug("getting CmisDocumentTypeDefinitionImpl for: {}", result != null ? result.getId() : null);
			CmisDocumentTypeDefinitionImpl resultDocument = new CmisDocumentTypeDefinitionImpl();
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> listPropertyDefinition = null;
			if (result.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = result.getPropertyDefinitions();
				listPropertyDefinition = property.entrySet().stream().collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}
			if (result.getTypeMutability() != null) {
				TypeMutability typeM = result.getTypeMutability();
				typeMutability = new TypeMutabilityImpl(typeM.canCreate(), typeM.canUpdate(), typeM.canDelete());
			}
			resultDocument = new CmisDocumentTypeDefinitionImpl(result.getId(), result.getLocalName(),
					result.getLocalNamespace(), result.getDisplayName(), result.getQueryName(), result.getDescription(),
					result.getBaseTypeId(), result.getParentTypeId(), result.isCreatable(), result.isFileable(),
					result.isQueryable(), result.isFulltextIndexed(), result.isIncludedInSupertypeQuery(),
					result.isControllablePolicy(), result.isControllableAcl(), typeMutability, listPropertyDefinition,
					result.isVersionable(), result.getContentStreamAllowed());
			return resultDocument;
		}

		/**
		 * returns the typeDefinition into CmisFolderTypeDefinition
		 */
		private static CmisFolderTypeDefinitionImpl getFolderTypeDefinition(TypeDefinition result) {

			LOG.debug("getting CmisFolderTypeDefinitionImpl for: {}", result != null ? result.getId() : null);
			CmisFolderTypeDefinitionImpl resultFolder = new CmisFolderTypeDefinitionImpl();
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> listPropertyDefinition = null;
			if (result.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = result.getPropertyDefinitions();
				listPropertyDefinition = property.entrySet().stream().collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}
			if (result.getTypeMutability() != null) {
				TypeMutability typeM = result.getTypeMutability();
				typeMutability = new TypeMutabilityImpl(typeM.canCreate(), typeM.canUpdate(), typeM.canDelete());
			}
			resultFolder = new CmisFolderTypeDefinitionImpl(result.getId(), result.getLocalName(),
					result.getLocalNamespace(), result.getDisplayName(), result.getQueryName(), result.getDescription(),
					result.getBaseTypeId(), result.getParentTypeId(), result.isCreatable(), result.isFileable(),
					result.isQueryable(), result.isFulltextIndexed(), result.isIncludedInSupertypeQuery(),
					result.isControllablePolicy(), result.isControllableAcl(), typeMutability, listPropertyDefinition);
			return resultFolder;
		}

		/**
		 * returns the typeDefinition into CmisItemTypeDefinition
		 */
		private static ItemTypeDefinitionImpl getItemTypeDefinition(TypeDefinition result) {
			LOG.debug("getting ItemTypeDefinitionImpl for: {}", result != null ? result.getId() : null);
			ItemTypeDefinitionImpl resultItem = new ItemTypeDefinitionImpl();
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> listPropertyDefinition = null;
			if (result.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = result.getPropertyDefinitions();
				listPropertyDefinition = property.entrySet().stream().collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}
			if (result.getTypeMutability() != null) {
				TypeMutability typeM = result.getTypeMutability();
				typeMutability = new TypeMutabilityImpl(typeM.canCreate(), typeM.canUpdate(), typeM.canDelete());
			}
			resultItem = new ItemTypeDefinitionImpl(result.getId(), result.getLocalName(), result.getLocalNamespace(),
					result.getDisplayName(), result.getQueryName(), result.getDescription(), result.getBaseTypeId(),
					result.getParentTypeId(), result.isCreatable(), result.isFileable(), result.isQueryable(),
					result.isFulltextIndexed(), result.isIncludedInSupertypeQuery(), result.isControllablePolicy(),
					result.isControllableAcl(), typeMutability, listPropertyDefinition);
			return resultItem;
		}

		/**
		 * returns the typeDefinition into MCmisRelationshipTypeDefinition
		 */
		private static CmisRelationshipTypeDefinitionImpl getRelationshipTypeDefinitionWithSourceTarget(
				TypeDefinition result, List<String> source, List<String> target) {

			LOG.debug("getting CmisRelationshipTypeDefinitionImpl for: {}", result != null ? result.getId() : null);

			CmisRelationshipTypeDefinitionImpl resultDocument = new CmisRelationshipTypeDefinitionImpl();
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> listPropertyDefinition = null;
			if (result.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = result.getPropertyDefinitions();
				listPropertyDefinition = property.entrySet().stream().collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}
			if (result.getTypeMutability() != null) {
				TypeMutability typeM = result.getTypeMutability();
				typeMutability = new TypeMutabilityImpl(typeM.canCreate(), typeM.canUpdate(), typeM.canDelete());
			}
			resultDocument = new CmisRelationshipTypeDefinitionImpl(result.getId(), result.getLocalName(),
					result.getLocalNamespace(), result.getDisplayName(), result.getQueryName(), result.getDescription(),
					result.getBaseTypeId(), result.getParentTypeId(), result.isCreatable(), result.isFileable(),
					result.isQueryable(), result.isFulltextIndexed(), result.isIncludedInSupertypeQuery(),
					result.isControllablePolicy(), result.isControllableAcl(), typeMutability, listPropertyDefinition,
					source, target);
			return resultDocument;
		}

		/**
		 * returns the typeDefinition into CmisPolicyTypeDefinition
		 */
		private static CmisPolicyTypeDefinitionImpl getPolicyTypeDefinition(TypeDefinition result) {
			LOG.debug("getting CmisPolicyTypeDefinitionImpl for: {}", result != null ? result.getId() : null);
			CmisPolicyTypeDefinitionImpl resultPolicy = new CmisPolicyTypeDefinitionImpl();
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> listPropertyDefinition = null;
			if (result.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = result.getPropertyDefinitions();
				listPropertyDefinition = property.entrySet().stream().collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}
			if (result.getTypeMutability() != null) {
				TypeMutability typeM = result.getTypeMutability();
				typeMutability = new TypeMutabilityImpl(typeM.canCreate(), typeM.canUpdate(), typeM.canDelete());
			}
			resultPolicy = new CmisPolicyTypeDefinitionImpl(result.getId(), result.getLocalName(),
					result.getLocalNamespace(), result.getDisplayName(), result.getQueryName(), result.getDescription(),
					result.getBaseTypeId(), result.getParentTypeId(), result.isCreatable(), result.isFileable(),
					result.isQueryable(), result.isFulltextIndexed(), result.isIncludedInSupertypeQuery(),
					result.isControllablePolicy(), result.isControllableAcl(), typeMutability, listPropertyDefinition);
			return resultPolicy;
		}

		/**
		 * returns the typeDefinition into CmisSecondaryTypeDefinition
		 */
		private static CmisSecondaryTypeDefinitionImpl getSecondaryTypeDefinition(TypeDefinition result) {

			LOG.debug("getting CmisSecondaryTypeDefinitionImpl for: {}", result != null ? result.getId() : null);
			CmisSecondaryTypeDefinitionImpl resultSecondary = new CmisSecondaryTypeDefinitionImpl();
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> listPropertyDefinition = null;
			if (result.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = result.getPropertyDefinitions();
				listPropertyDefinition = property.entrySet().stream().collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}
			if (result.getTypeMutability() != null) {
				TypeMutability typeM = result.getTypeMutability();
				typeMutability = new TypeMutabilityImpl(typeM.canCreate(), typeM.canUpdate(), typeM.canDelete());
			}
			resultSecondary = new CmisSecondaryTypeDefinitionImpl(result.getId(), result.getLocalName(),
					result.getLocalNamespace(), result.getDisplayName(), result.getQueryName(), result.getDescription(),
					result.getBaseTypeId(), result.getParentTypeId(), result.isCreatable(), result.isFileable(),
					result.isQueryable(), result.isFulltextIndexed(), result.isIncludedInSupertypeQuery(),
					result.isControllablePolicy(), result.isControllableAcl(), typeMutability, listPropertyDefinition);
			return resultSecondary;
		}

		@SuppressWarnings({ "rawtypes" })
		public static PropertyDefinitionImpl<?> getPropertyDefinition(PropertyDefinition<?> pro, Boolean inherited) {
			// LOG.info("getPropertyDefinition from {}", pro);
			PropertyDefinitionImpl<?> propertyDefinition = null;
			propertyDefinition = new PropertyDefinitionImpl(pro.getId(), pro.getLocalName(), pro.getLocalNamespace(),
					pro.getDisplayName(), pro.getQueryName(), pro.getDescription(), pro.getPropertyType(),
					pro.getCardinality(), pro.getUpdatability(), inherited == null ? pro.isInherited() : inherited,
					pro.isRequired(), pro.isQueryable(), pro.isOrderable(), pro.isOpenChoice());
			if (pro.getPropertyType().value().equals("string")) {
				PropertyStringDefinition ps = (PropertyStringDefinition) pro;
				propertyDefinition.setMaxLength(ps.getMaxLength() == null ? null : ps.getMaxLength().intValue());
			} else if (pro.getPropertyType().value().equals("integer")) {
				PropertyIntegerDefinition pi = (PropertyIntegerDefinition) pro;
				propertyDefinition.setMinValue(pi.getMinValue() == null ? null : pi.getMinValue().intValue());
				propertyDefinition.setMaxValue(pi.getMaxValue() == null ? null : pi.getMaxValue().intValue());
			}
			return propertyDefinition;
		}

		private static void createFolderForType(TypeDefinition type, IUserObject userObject, String repositoryId)
				throws IOException, IllegalArgumentException, CmisInvalidArgumentException {
			PropertiesImpl result = new PropertiesImpl();
			PropertyData<?> propertyIDData = new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID,
					BaseTypeId.CMIS_FOLDER.value());
			PropertyData<?> propertyNameData = new PropertyIdImpl(PropertyIds.NAME, type.getId());
			result.addProperty(propertyIDData);
			result.addProperty(propertyNameData);
			CmisObjectService.Impl.createTypeFolder(repositoryId, result, userObject);
		}

		private static TypeDefinition getTypeDefinitionManager(MTypeManagerDAO typeManagerDAO, TypeDefinition type,
				Map<String, PropertyDefinitionImpl<?>> Mproperty, TypeMutabilityImpl typeMutability) {
			TypeDefinition newType = typeManagerDAO.createObjectFacade(type.getId(),
					type.getLocalName() == null ? "localName" : type.getLocalName(),
					type.getLocalNamespace() == null ? "localNameSpace" : type.getLocalNamespace(),
					type.getDisplayName() == null ? "displayName" : type.getDisplayName(),
					type.getQueryName() == null ? "queryName" : type.getQueryName(),
					type.getDescription() == null ? "description" : type.getDescription(), type.getBaseTypeId(),
					type.getParentTypeId(), type.isCreatable(), type.isFileable(),
					type.isQueryable() == null ? false : type.isQueryable(),
					type.isFulltextIndexed() == null ? false : type.isFulltextIndexed(),
					type.isIncludedInSupertypeQuery() == null ? false : type.isIncludedInSupertypeQuery(),
					type.isControllablePolicy(), type.isControllableAcl(), typeMutability, Mproperty, null, null);
			return newType;
		}

		private static DocumentTypeDefinition getDocumentTypeDefinition(MTypeManagerDAO typeManagerDAO,
				DocumentTypeDefinition type, Map<String, PropertyDefinitionImpl<?>> Mproperty,
				TypeMutabilityImpl typeMutability) {
			DocumentTypeDefinition newType = (DocumentTypeDefinition) typeManagerDAO.createObjectFacade(type.getId(),
					type.getLocalName() == null ? "localName" : type.getLocalName(),
					type.getLocalNamespace() == null ? "localNameSpace" : type.getLocalNamespace(),
					type.getDisplayName() == null ? "displayName" : type.getDisplayName(),
					type.getQueryName() == null ? "queryName" : type.getQueryName(),
					type.getDescription() == null ? "description" : type.getDescription(), type.getBaseTypeId(),
					type.getParentTypeId(), type.isCreatable(), type.isFileable(),
					type.isQueryable() == null ? false : type.isQueryable(),
					type.isFulltextIndexed() == null ? false : type.isFulltextIndexed(),
					type.isIncludedInSupertypeQuery() == null ? false : type.isIncludedInSupertypeQuery(),
					type.isControllablePolicy(), type.isControllableAcl(), typeMutability, Mproperty,
					type.isVersionable() == null ? false : type.isVersionable(),
					type.getContentStreamAllowed() == null ? ContentStreamAllowed.NOTALLOWED
							: type.getContentStreamAllowed());
			return newType;
		}

		private static void addIndex(String repositoryId,
				Map<String, PropertyDefinitionImpl<?>> getPropertyDefinitions) {
			List<String> primaryIndex = getPropertyDefinitions.entrySet().stream()
					.filter(map -> map.getValue() != null && map.getValue().getLocalName() != null
							&& map.getValue().getLocalName().equalsIgnoreCase("primaryKey"))
					.map(t -> "properties." + t.getValue().getId()).collect(Collectors.<String>toList());
			List<String> secondaryIndex = getPropertyDefinitions.entrySet().stream()
					.filter(map -> map.getValue() != null && map.getValue().getLocalName() != null
							&& map.getValue().getLocalName().equalsIgnoreCase("lk_" + map.getValue().getId()))
					.map(t -> "properties." + t.getValue().getId()).collect(Collectors.<String>toList());
			secondaryIndex.parallelStream().collect(Collectors.toCollection(() -> primaryIndex));
			String[] columnsToIndex = primaryIndex.toArray(new String[primaryIndex.size()]);
			if (columnsToIndex.length > 0) {
				DatabaseServiceFactory.getInstance(repositoryId).addIndex(repositoryId, columnsToIndex);
			}
		}

		public static List<? extends TypeDefinition> getTypeDefinitionWithTypePermission(
				ITypePermissionService typePermissionFlow, String repositoryId, IUserGroupObject[] role,
				String typeId) {
			LOG.info("className: {},  methodName: {}, repositoryId: {}, ITypePermissionService: {}, user role: {}",
					"CmisTypeServices", "getTypeDefinitionWithTypePermission", repositoryId, typePermissionFlow, role);
			List<? extends TypeDefinition> typeDef = null;
			if (typePermissionFlow != null) {
				if (typePermissionFlow.checkPermissionAccess(repositoryId, role, typeId, TypePermissionType.READ)) {
					if (typePermissionFlow.checkTypeAccess(repositoryId, role, typeId)) {
						List<String> fieldsAcess = typePermissionFlow.getFieldAccess(repositoryId, role, typeId);
						if (fieldsAcess != null) {
							typeDef = DBUtils.TypeServiceDAO.getById(repositoryId, Arrays.asList(typeId),
									Helpers.getTypeMappedColumns(fieldsAcess));
						}

					}
				}
			} else {
				typeDef = DBUtils.TypeServiceDAO.getById(repositoryId, Arrays.asList(typeId), null);
			}
			return typeDef;
		}

		public static DocumentTypeDefinition getDocumentDefinitionWithTypePermission(
				ITypePermissionService typePermissionFlow, String repositoryId, IUserGroupObject[] role,
				String typeId) {
			LOG.info("className: {},  methodName: {}, repositoryId: {}, ITypePermissionService: {}, user role: {}",
					"CmisTypeServices", "getDocumentDefinitionWithTypePermission", repositoryId, typePermissionFlow,
					role);
			DocumentTypeDefinition docType = null;
			if (typePermissionFlow != null) {
				if (typePermissionFlow.checkPermissionAccess(repositoryId, role, typeId, TypePermissionType.READ)) {
					if (typePermissionFlow.checkTypeAccess(repositoryId, role, typeId)) {
						List<String> fieldsAcess = typePermissionFlow.getFieldAccess(repositoryId, role, typeId);
						if (fieldsAcess != null) {
							docType = DBUtils.DocumentTypeManagerDAO.getByTypeId(repositoryId, typeId,
									Helpers.getTypeMappedColumns(fieldsAcess));
						}
					}
				}
			} else {
				docType = DBUtils.DocumentTypeManagerDAO.getByTypeId(repositoryId, typeId, null);
			}
			return docType;
		}

		public static List<? extends TypeDefinition> checkTypePermissionList(ITypePermissionService typePermissionFlow,
				String repositoryId, IUserGroupObject[] role, List<?> typeId) {
			LOG.info("className: {},  methodName: {}, repositoryId: {}, ITypePermissionService: {}, user role: {}",
					"CmisTypeServices", "checkTypePermissionList", repositoryId, typePermissionFlow, role);
			List<? extends TypeDefinition> typeDef = null;
			List<TypeDefinition> typeSecDef = new ArrayList<>();
			if (typePermissionFlow != null) {
				for (Object id : typeId) {
					if (typePermissionFlow.checkPermissionAccess(repositoryId, role, id.toString(),
							TypePermissionType.READ)) {
						if (typePermissionFlow.checkTypeAccess(repositoryId, role, id.toString())) {
							List<String> fieldsAcess = typePermissionFlow.getFieldAccess(repositoryId, role,
									id.toString());
							if (fieldsAcess != null) {
								TypeDefinition typeProp = DBUtils.TypeServiceDAO.getById(repositoryId,
										Arrays.asList(id.toString()), Helpers.getTypeMappedColumns(fieldsAcess)).get(0);
								typeSecDef.add(typeProp);
							}

						}
					}
				}
				typeDef = typeSecDef;

			} else {
				typeDef = DBUtils.TypeServiceDAO.getById(repositoryId, typeId, null);
			}
			return typeDef;
		}

		private static Boolean checkCrudPermission(ITypePermissionService typePermissionFlow, String repositoryId,
				IUserGroupObject[] role, String typeId, TypePermissionType permission) {
			LOG.info("className: {},  methodName: {}, repositoryId: {}, ITypePermissionService: {}, user role: {}",
					"CmisTypeServices", "checkCrudPermission", repositoryId, typePermissionFlow, role);
			if (typePermissionFlow != null) {
				if (typePermissionFlow.checkPermissionAccess(repositoryId, role, typeId, permission)) {
					return true;
				}
			} else {
				return true;
			}
			return false;
		}
	}
}
