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
package com.pogeyan.cmis.services;

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
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.data.ExtensionsData;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
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
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.TypeDefinitionContainerImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.TypeDefinitionListImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.data.CmisDocumentTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.CmisFolderTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.CmisPolicyTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.CmisRelationshipTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.CmisSecondaryTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.ItemTypeDefinitionImpl;
import com.pogeyan.cmis.api.data.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.TypeMutabilityImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.data.objects.MBaseObject;
import com.pogeyan.cmis.data.objects.MTypeObject;
import com.pogeyan.cmis.service.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.utils.DBUtils;

public class CmisTypeServices {
	private static final Logger LOG = LoggerFactory.getLogger(CmisTypeServices.class);

	public static class Impl {

		public static void addBaseType(String repositoryId) throws MongoException {
			LOG.info("addBaseType for {}", repositoryId);
			try {
				MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MTypeManagerDAO.class);
				List<MTypeObject> getTypeObject = typeMorphiaDAO.getById(null);
				if (getTypeObject != null) {
					LOG.info("Added BaseType Successfully for repository: {}", repositoryId);
				} else {
					List<MTypeObject> baseType = upset();
					for (MTypeObject tm : baseType) {
						typeMorphiaDAO.commit(tm);
						LOG.info("Added BaseType Successfully");
					}
				}
			} catch (MongoException e) {
				LOG.error("MongoObject shouldnot be null");
				throw new MongoException("MongoObject shouldnot be null");
			}
		}

		/**
		 * returns the Morphia BaseTypeObject
		 */
		private static List<MTypeObject> upset() {
			List<MTypeObject> typeList = new ArrayList<>();
			TypeMutabilityImpl type = new TypeMutabilityImpl(true, true, true);
			Map<String, PropertyDefinitionImpl<?>> folderProperty = getBaseFolderProperty();
			MTypeObject folderType = new MTypeObject("cmis:folder", "cmis:folder", "cmis:folder", "cmis:folder",
					"cmis:folder", "Folder", BaseTypeId.CMIS_FOLDER, null, true, true, true, true, true, true, true,
					type, folderProperty);
			Map<String, PropertyDefinitionImpl<?>> documentProperty = getBaseDocumentProperty();
			CmisDocumentTypeDefinitionImpl documentType = new CmisDocumentTypeDefinitionImpl("cmis:document",
					"cmis:document", "cmis:document", "cmis:document", "cmis:document", "Document",
					BaseTypeId.CMIS_DOCUMENT, null, true, true, true, true, true, true, true, type, documentProperty,
					true, ContentStreamAllowed.ALLOWED);

			MTypeObject itemType = new MTypeObject("cmis:item", "cmis:item", "cmis:item", "cmis:item", "cmis:item",
					"Item", BaseTypeId.CMIS_ITEM, null, true, true, true, true, true, true, true, type,
					getBaseProperty());

			Map<String, PropertyDefinitionImpl<?>> relationShipProperty = getBaserelationShipProperty();
			MTypeObject realtionShipType = new MTypeObject("cmis:relationship", "cmis:relationship",
					"cmis:relationship", "cmis:relationship", "cmis:relationship", "Relationship",
					BaseTypeId.CMIS_RELATIONSHIP, null, true, false, true, true, true, true, true, type,
					relationShipProperty);

			Map<String, PropertyDefinitionImpl<?>> policyProperty = getBasepolicyProperty();
			MTypeObject policyType = new MTypeObject("cmis:policy", "cmis:policy", "cmis:policy", "cmis:policy",
					"cmis:policy", "Policy", BaseTypeId.CMIS_POLICY, null, true, true, true, true, true, true, true,
					type, policyProperty);

			Map<String, PropertyDefinitionImpl<?>> SecondaryTypeProperty = getBaseSecondaryTypeProperty();
			MTypeObject secondaryType = new MTypeObject("cmis:secondary", "cmis:secondary", "cmis:secondary",
					"cmis:secondary", "cmis:secondary", "Secondary Type", BaseTypeId.CMIS_SECONDARY, null, false, false,
					true, true, true, false, false, type, SecondaryTypeProperty);

			// Map<String, MPropertyDefinition<?>>
			// customMgtSecondaryTypeProperty =
			// getclientMgtRetentionSecondaryProperty();
			// MTypeObject customClientMgtsecondaryType = new
			// MTypeObject("cmis:rm:clientMgtRetention",
			// "cmis:rm:clientMgtRetention", "cmis:rm:clientMgtRetention",
			// "cmis:rm:clientMgtRetention",
			// "cmis:rm:clientMgtRetention", "Custom Secondary Type",
			// BaseTypeId.CMIS_SECONDARY, "cmis:secondary",
			// false, false, true, true, true, true, false, false, type,
			// customMgtSecondaryTypeProperty);
			//
			// Map<String, MPropertyDefinition<?>>
			// customDestructionSecondaryTypeProperty =
			// getdestructionRetentionSecondaryProperty();
			// MTypeObject customDestructionsecondaryType = new
			// MTypeObject("cmis:rm:destructionRetention",
			// "cmis:rm:destructionRetention", "cmis:rm:destructionRetention",
			// "cmis:rm:destructionRetention",
			// "cmis:rm:destructionRetention", "Custom Secondary Type",
			// BaseTypeId.CMIS_SECONDARY,
			// "cmis:secondary", false, false, true, true, true, true, false,
			// false, type,
			// customDestructionSecondaryTypeProperty);
			//
			// MTypeObject holdsecondaryType = new MTypeObject("cmis:rm:hold",
			// "cmis:rm:hold", "cmis:rm:hold",
			// "cmis:rm:hold", "cmis:rm:hold", "Custom Secondary Type",
			// BaseTypeId.CMIS_SECONDARY,
			// "cmis:secondary", false, false, true, true, true, true, false,
			// false, type, null);
			//
			// MTypeObject repsecondaryType = new
			// MTypeObject("cmis:rm:repMgtRetention", "cmis:rm:repMgtRetention",
			// "cmis:rm:repMgtRetention", "cmis:rm:repMgtRetention",
			// "cmis:rm:repMgtRetention",
			// "Custom Secondary Type", BaseTypeId.CMIS_SECONDARY,
			// "cmis:secondary", false, false, true, true,
			// true, true, false, false, type, null);

			typeList.add(folderType);
			typeList.add(documentType);
			typeList.add(itemType);
			typeList.add(realtionShipType);
			typeList.add(policyType);
			typeList.add(secondaryType);
			// typeList.add(customClientMgtsecondaryType);
			// typeList.add(customDestructionsecondaryType);
			// typeList.add(holdsecondaryType);
			// typeList.add(repsecondaryType);
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
					Cardinality.SINGLE, Updatability.READONLY, false, false, false, false, null);
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
			PropertyDefinitionImpl parentId = new PropertyDefinitionImpl("cmis:parentId", "localName", "localNameSpace",
					"cmis:parentId", "cmis:parentId", "description", PropertyType.ID, Cardinality.SINGLE,
					Updatability.READONLY, false, false, true, false, null);
			folderList.put("cmis:parentId", parentId);
			PropertyDefinitionImpl allowedChildObjectTypeIds = new PropertyDefinitionImpl(
					"cmis:allowedChildObjectTypeIds", "localName", "localNameSpace", "cmis:allowedChildObjectTypeIds",
					"cmis:allowedChildObjectTypeIds", "description", PropertyType.ID, Cardinality.MULTI,
					Updatability.READONLY, false, false, true, false, null);
			folderList.put("cmis:allowedChildObjectTypeIds", allowedChildObjectTypeIds);
			PropertyDefinitionImpl path = new PropertyDefinitionImpl("cmis:path", "localName", "localNameSpace",
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
		public static Map<String, PropertyDefinitionImpl<?>> getBaseSecondaryTypeProperty() {
			Map<String, PropertyDefinitionImpl<?>> list = new HashMap<>();
			PropertyDefinitionImpl<?> name = new PropertyDefinitionImpl("cmis:name", "localName", "localNameSpace",
					"cmis:name", "cmis:name", "description", PropertyType.STRING, Cardinality.SINGLE,
					Updatability.READWRITE, false, false, true, false, null);
			list.put("cmis:name", name);
			PropertyDefinitionImpl<?> objectId = new PropertyDefinitionImpl("cmis:objectId", "localName",
					"localNameSpace", "cmis:objectId", "cmis:objectId", "description", PropertyType.ID,
					Cardinality.SINGLE, Updatability.READONLY, false, false, true, false, null);
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
			// MPropertyDefinition parentId = new
			// MPropertyDefinition("cmis:parentId", "localName",
			// "localNameSpace",
			// "cmis:parentId", "cmis:parentId", "description", PropertyType.ID,
			// Cardinality.SINGLE,
			// Updatability.READONLY, false, false, true, false, null);
			// list.put("cmis:parentId", parentId);
			// MPropertyDefinition path = new MPropertyDefinition("cmis:path",
			// "localName", "localNameSpace", "cmis:path",
			// "cmis:path", "description", PropertyType.STRING,
			// Cardinality.SINGLE, Updatability.READONLY, true,
			// false, true, false, null);
			// list.put("cmis:path", path);
			// MPropertyDefinition allowedChildObjectTypeIds = new
			// MPropertyDefinition("cmis:allowedChildObjectTypeIds",
			// "localName", "localNameSpace", "cmis:allowedChildObjectTypeIds",
			// "cmis:allowedChildObjectTypeIds",
			// "description", PropertyType.ID, Cardinality.MULTI,
			// Updatability.READONLY, false, false, true, false,
			// null);
			// list.put("cmis:allowedChildObjectTypeIds",
			// allowedChildObjectTypeIds);

			return list;
		}

		/**
		 * create a custom type
		 */
		public static TypeDefinition createType(String repositoryId, TypeDefinition type, ExtensionsData extension,
				String userName) throws IllegalArgumentException {
			LOG.info("createType for type: {} , repository: {} ", type, repositoryId);
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> Mproperty = null;
			List<MTypeObject> innerChild = new ArrayList<MTypeObject>();
			innerChild.clear();
			MTypeObject object = null;
			if (type == null) {
				LOG.error("Type must be set!");
				throw new IllegalArgumentException("Type must be set!");
			}
			if (type.getId() == null || type.getId().trim().length() == 0) {
				LOG.error("Type must have a valid id!");
				throw new IllegalArgumentException("Type must have a valid id!");
			}
			if (type.getParentTypeId() == null || type.getParentTypeId().trim().length() == 0) {
				LOG.error("Type must have a valid parent id!");
				throw new IllegalArgumentException("Type must have a valid parent id!");
			}

			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			if (typeMorphiaDAO.getById(Arrays.asList(type.getId())).size() > 0) {
				object = typeMorphiaDAO.getById(Arrays.asList(type.getId())).get(0);
			}
			if (object != null) {
				LOG.error(type.getId(), " is already present!");
				throw new IllegalArgumentException(type.getId() + " is already present");
			}
			if (type.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = type.getPropertyDefinitions();
				Set<String> propId = property.keySet();
				for (String id : propId) {
					Map<String, PropertyDefinition<?>> propValues = typeMorphiaDAO.getAllPropertyById(id);
					if (propValues != null) {
						LOG.error("Property" + id, " duplicate there!");
						throw new IllegalArgumentException("Property" + id + "duplicate there");
					}
				}
				Mproperty = property.entrySet().stream().filter(t -> t.getValue().getId() != null).collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}
			typeMutability = new TypeMutabilityImpl(true, false, true);
			LOG.info("Added new type {}", type.getId());
			if (LOG.isDebugEnabled()) {
				LOG.debug("Added type {}", type.getId());
			}
			addIndex(repositoryId, Mproperty);
			if (type.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {
				DocumentTypeDefinition doctype = (DocumentTypeDefinition) type;
				CmisDocumentTypeDefinitionImpl newType = getDocumentTypeDefinition(doctype, Mproperty, typeMutability);
				typeMorphiaDAO.commit(newType);
				try {
					createFolderForType(type, userName, repositoryId);
				} catch (IOException e) {
					typeMorphiaDAO.delete(type.getId());
					LOG.error("Folder creation exception:  {}", e.getMessage());
					throw new IllegalArgumentException(e.getMessage());
				}
				TypeDefinition getType = gettingAllTypeDefinition(repositoryId, newType);
				return getType;
			} else {
				MTypeObject newType = getTypeDefinitionManager(type, Mproperty, typeMutability);
				typeMorphiaDAO.commit(newType);
				try {
					if (type.getBaseTypeId() != BaseTypeId.CMIS_FOLDER) {
						createFolderForType(type, userName, repositoryId);
					}
				} catch (IOException e) {
					typeMorphiaDAO.delete(type.getId());
					LOG.error("Folder creation exception:  {}", e.getMessage());
					throw new IllegalArgumentException(e.getMessage());
				}
				TypeDefinition getType = gettingAllTypeDefinition(repositoryId, newType);
				return getType;
			}

		}

		/**
		 * Update a type
		 */
		public static TypeDefinition updateType(String repositoryId, TypeDefinition type, ExtensionsData extension)
				throws IllegalArgumentException {
			LOG.info("updateType for type: {} , repository: {}", type, repositoryId);
			TypeMutabilityImpl typeMutability = null;
			Map<String, PropertyDefinitionImpl<?>> Mproperty = null;
			MTypeObject object = null;
			if (type == null) {
				LOG.error("Type must be set!");
				throw new IllegalArgumentException("Type must be set!");
			}
			if (type.getId() == null || type.getId().trim().length() == 0) {
				LOG.error("Type must have a valid id!");
				throw new IllegalArgumentException("Type must have a valid id!");
			}
			if (type.getParentTypeId() == null || type.getParentTypeId().trim().length() == 0) {
				LOG.error("Type must have a valid parent id!");
				throw new IllegalArgumentException("Type must have a valid parent id!");
			}
			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			if (typeMorphiaDAO.getById(Arrays.asList(type.getId())).size() > 0) {
				object = typeMorphiaDAO.getById(Arrays.asList(type.getId())).get(0);
			}
			if (object == null) {
				LOG.error(type.getId(), " is unknown");
				throw new IllegalArgumentException("Unknown TypeId" + type.getId());
			}
			if (type.getPropertyDefinitions() != null) {
				Map<String, PropertyDefinition<?>> property = type.getPropertyDefinitions();
				Mproperty = property.entrySet().stream().filter(t -> t.getValue().getId() != null).collect(
						Collectors.toMap(p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}, LinkedHashMap::new));
			}

			typeMutability = new TypeMutabilityImpl(false, true, true);

			if (LOG.isDebugEnabled()) {
				LOG.info("Updated type {}", type.getId());
			}
			addIndex(repositoryId, Mproperty);
			if (type.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {
				DocumentTypeDefinition doctype = (DocumentTypeDefinition) type;
				CmisDocumentTypeDefinitionImpl newType = getDocumentTypeDefinition(doctype, Mproperty, typeMutability);
				typeMorphiaDAO.commit(newType);
			} else {
				MTypeObject newType = getTypeDefinitionManager(type, Mproperty, typeMutability);
				typeMorphiaDAO.commit(newType);
			}
			TypeDefinition getType = getTypeDefinition(repositoryId, type.getId(), extension);
			return getType;

		}

		/**
		 * delete a type
		 */
		public static void deleteType(String repositoryId, String type, ExtensionsData extension)
				throws IllegalArgumentException {
			LOG.info("deleteType for type: {} , repository: {}", type, repositoryId);
			MTypeObject object = null;

			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			MBaseObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MBaseObjectDAO.class);

			if (type == null) {
				LOG.error(type, "Type is not available to delete");
				throw new IllegalArgumentException("Type must be set!");
			}
			if (typeMorphiaDAO.getById(Arrays.asList(type)).size() > 0) {
				object = typeMorphiaDAO.getById(Arrays.asList(type)).get(0);
			}
			if (object == null) {
				LOG.error(type, " does not exists");
				throw new IllegalArgumentException("Unknown TypeId " + type);
			}
			// Map<String, String> parameters =
			// RepositoryManager.get().getFileDetails(repositoryId);
			// IStorageService localService =
			// MongoStorageDocument.createStorageService(parameters,
			// repositoryId, type);
			// localService.deleteFolder(parameters, repositoryId, type);
			MBaseObject folderObject = DBUtils.BaseDAO.getByPath(repositoryId, "/" + type);
			baseMorphiaDAO.delete(folderObject.getId(), true, null);
			typeMorphiaDAO.delete(type);
			if (LOG.isDebugEnabled()) {
				LOG.info("Deleted type: {}", type);
			}
		}

		public static TypeDefinition getTypeDefinition(String repositoryId, String typeId, ExtensionsData extension) {
			LOG.info("getTypeDefinition for type: {} , repository: {}", typeId, repositoryId);

			if (typeId == null) {
				LOG.error("typeId should not be null");
				throw new IllegalArgumentException("Type must be set!");
			}

			MTypeObject typeDefinition = null;
			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			if (typeMorphiaDAO.getById(Arrays.asList(typeId)).size() > 0) {
				typeDefinition = typeMorphiaDAO.getById(Arrays.asList(typeId)).get(0);
			}

			if (typeDefinition == null) {
				// MTypeMutability typeMutability = new MTypeMutability(false,
				// false, false);
				// Map<String, MPropertyDefinition<?>> propertyDefinition = new
				// HashMap<>();
				// MCmisDocumentTypeDefinition mTypeObject = new
				// MCmisDocumentTypeDefinition("", "", "", "", "", "",
				// BaseTypeId.CMIS_DOCUMENT, "", false, false, false, false,
				// false, false, false, false,
				// typeMutability, propertyDefinition, null, null);
				// resultDocument = getDocumentTypeDefinition(mTypeObject);
				// typeDefinitionContainer = new
				// TypeDefinitionContainerImpl(resultDocument);
				// TypeDefinition emptyTypeDefinition =
				// typeDefinitionContainer.getTypeDefinition();
				// return emptyTypeDefinition;
				LOG.error("typeId should not be null");
				throw new CmisObjectNotFoundException("Type must be set!");

			}
			return gettingAllTypeDefinition(repositoryId, typeDefinition);

		}

		@SuppressWarnings("rawtypes")
		private static TypeDefinition gettingAllTypeDefinition(String repositoryId, MTypeObject typeDefinition) {
			CmisDocumentTypeDefinitionImpl resultDocument = null;
			CmisFolderTypeDefinitionImpl resultFolder = null;
			ItemTypeDefinitionImpl resultItem = null;
			CmisPolicyTypeDefinitionImpl resultPolicy = null;
			CmisRelationshipTypeDefinitionImpl resultRelationship = null;
			CmisSecondaryTypeDefinitionImpl resultSecondary = null;
			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			List<MTypeObject> innerChild = new ArrayList<MTypeObject>();
			innerChild.clear();
			if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_DOCUMENT) {
				MDocumentTypeManagerDAO docTypeMorphia = DatabaseServiceFactory.getInstance(repositoryId)
						.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
				CmisDocumentTypeDefinitionImpl docType = docTypeMorphia.getByTypeId(typeDefinition.getId().toString());
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null);
				CmisDocumentTypeDefinitionImpl documentType = getTypeDocumentObjectInstance(docType, list);

				resultDocument = getDocumentTypeDefinition(documentType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultDocument);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_FOLDER) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null);
				MTypeObject folderType = getTypeObjectInstance(typeDefinition, list);

				resultFolder = getFolderTypeDefinition(folderType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultFolder);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_ITEM) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null);
				MTypeObject itemType = getTypeObjectInstance(typeDefinition, list);
				resultItem = getItemTypeDefinition(itemType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultItem);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_POLICY) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null);
				MTypeObject policyType = getTypeObjectInstance(typeDefinition, list);
				resultPolicy = getPolicyTypeDefinition(policyType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultPolicy);
				typeDefinitionContainer.setChildren(null);
			} else if (typeDefinition.getBaseTypeId() == BaseTypeId.CMIS_SECONDARY) {
				Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(typeDefinition, repositoryId,
						innerChild, null);
				MTypeObject secondaryType = getTypeObjectInstance(typeDefinition, list);
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
					for (PropertyDefinition pro : property.values()) {
						PropertyDefinitionImpl<?> propertyDefinition = getPropertyDefinition(pro, null);
						if (propertyDefinition.getLocalNamespace().equalsIgnoreCase("sourceId")) {
							if (!propertyDefinition.getId().equals("cmis:sourceId")) {
								sourceIds.add(propertyDefinition.getId());
							}

						} else if (propertyDefinition.getLocalNamespace().equalsIgnoreCase("targetId")) {
							if (!propertyDefinition.getId().equals("cmis:targetId")) {
								targetIds.add(propertyDefinition.getId());
							}
						}
						list.put(pro.getId(), propertyDefinition);
					}
				}

				if (typeDefinition.getParentTypeId() != null) {
					List<MTypeObject> childTypes = getTypeParent(repositoryId, typeDefinition.getParentTypeId(),
							innerChild);
					if (childTypes != null) {
						for (MTypeObject parentObject : childTypes) {
							if (parentObject.getPropertyDefinitions() != null) {
								Map<String, PropertyDefinition<?>> property = parentObject.getPropertyDefinitions();
								for (PropertyDefinition pro : property.values()) {
									PropertyDefinitionImpl<?> propertyDefinition = getPropertyDefinition(pro, true);
									list.put(pro.getId(), propertyDefinition);
								}
							}
						}
					}
				}

				MTypeObject resultrelationshipType = getTypeObjectInstance(typeDefinition, list);

				resultRelationship = getRelationshipTypeDefinitionWithSourceTarget(resultrelationshipType, sourceIds,
						targetIds);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultRelationship);
				typeDefinitionContainer.setChildren(null);
			}
			if (typeDefinitionContainer != null) {
				typeDefinitionContainer.getTypeDefinition().getPropertyDefinitions().size();
				return typeDefinitionContainer.getTypeDefinition();
			} else {
				LOG.error("unknown typeId {}", typeDefinition.getId());
				throw new CmisObjectNotFoundException("unknown typeId: " + typeDefinition.getId());
			}
		}

		private static Map<String, PropertyDefinitionImpl<?>> getTypeProperties(MTypeObject typeDefinition,
				String repositoryId, List<MTypeObject> innerChild, Boolean includeProperty) {
			boolean incluePro = includeProperty == null ? true : includeProperty;
			Map<String, PropertyDefinitionImpl<?>> listProperty = null;

			if (incluePro) {
				Map<String, PropertyDefinitionImpl<?>> parentPropertyDefinition = null;
				Map<String, PropertyDefinitionImpl<?>> ownPropertyDefinition = null;
				if (typeDefinition.getPropertyDefinitions() != null) {
					Map<String, PropertyDefinition<?>> property = typeDefinition.getPropertyDefinitions();
					ownPropertyDefinition = property.entrySet().stream().collect(Collectors.toMap(p -> p.getKey(),
							p -> getPropertyDefinition(p.getValue(), null), (u, v) -> {
								throw new IllegalStateException(String.format("Duplicate key %s", u));
							}, LinkedHashMap::new));
				}
				if (typeDefinition.getParentTypeId() != null) {
					List<MTypeObject> childTypes = getTypeParent(repositoryId, typeDefinition.getParentTypeId(),
							innerChild);
					if (childTypes != null) {
						for (MTypeObject parentObject : childTypes) {
							if (parentObject.getPropertyDefinitions() != null) {
								Map<String, PropertyDefinition<?>> property = parentObject.getPropertyDefinitions();
								parentPropertyDefinition = property.entrySet().stream().collect(Collectors.toMap(
										p -> p.getKey(), p -> getPropertyDefinition(p.getValue(), true), (u, v) -> {
											throw new IllegalStateException(String.format("Duplicate key %s", u));
										}, LinkedHashMap::new));
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

		private static MTypeObject getTypeObjectInstance(MTypeObject typeDefinition,
				Map<String, PropertyDefinitionImpl<?>> list) {

			MTypeObject type = new MTypeObject(typeDefinition.getId(), typeDefinition.getLocalName(),
					typeDefinition.getLocalNamespace(), typeDefinition.getDisplayName(), typeDefinition.getQueryName(),
					typeDefinition.getDescription(), typeDefinition.getBaseTypeId(), typeDefinition.getParentTypeId(),
					typeDefinition.isCreatable(), typeDefinition.isFileable(), typeDefinition.isQueryable(),
					typeDefinition.isFulltextIndexed(), typeDefinition.isIncludedInSupertypeQuery(),
					typeDefinition.isControllablePolicy(), typeDefinition.isControllableAcl(),
					typeDefinition.getTypeMutability(), list);
			return type;

		}

		private static CmisDocumentTypeDefinitionImpl getTypeDocumentObjectInstance(
				CmisDocumentTypeDefinitionImpl typeDefinition, Map<String, PropertyDefinitionImpl<?>> list) {
			CmisDocumentTypeDefinitionImpl type = new CmisDocumentTypeDefinitionImpl(typeDefinition.getId(),
					typeDefinition.getLocalName(), typeDefinition.getLocalNamespace(), typeDefinition.getDisplayName(),
					typeDefinition.getQueryName(), typeDefinition.getDescription(), typeDefinition.getBaseTypeId(),
					typeDefinition.getParentTypeId(), typeDefinition.isCreatable(), typeDefinition.isFileable(),
					typeDefinition.isQueryable(), typeDefinition.isFulltextIndexed(),
					typeDefinition.isIncludedInSupertypeQuery(), typeDefinition.isControllablePolicy(),
					typeDefinition.isControllableAcl(), typeDefinition.getTypeMutability(), list,
					typeDefinition.isVersionable(), typeDefinition.getContentStreamAllowed());
			return type;

		}

		/**
		 * get TypeDefinition using properties
		 */
		public static TypeDefinition getTypeRelationshipDefinition(String repositoryId, String typeId, boolean cmis11) {
			LOG.info("getTypeRelationshipDefinition for type: {} , repository: {}", typeId, repositoryId);
			if (typeId == null) {
				LOG.error("typeId should not be null");
				throw new IllegalArgumentException("Type must be set!");
			}
			List<MTypeObject> innerChild = new ArrayList<MTypeObject>();
			innerChild.clear();
			MTypeObject typeDefinition = null;
			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			if (typeMorphiaDAO.getById(Arrays.asList(typeId)).size() > 0) {
				typeDefinition = typeMorphiaDAO.getById(Arrays.asList(typeId)).get(0);
			}
			return typeDefinition;
		}

		/**
		 * get Type children for particular TypeID
		 */
		public static TypeDefinitionList getTypeChildren(String repositoryId, String typeId,
				Boolean includePropertyDefinitions, BigInteger maxItems, BigInteger skipCount, ExtensionsData extension)
				throws IllegalArgumentException {
			LOG.info("getTypeChildren for type: {} , repository: {}", typeId, repositoryId);
			boolean inclPropDefs = includePropertyDefinitions == null ? false : includePropertyDefinitions;
			int skip = skipCount == null ? 0 : skipCount.intValue();
			int max = maxItems == null ? -1 : maxItems.intValue();
			MTypeObject object = null;
			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			if (typeId != null) {
				if (typeMorphiaDAO.getById(Arrays.asList(typeId)).size() > 0) {
					object = typeMorphiaDAO.getById(Arrays.asList(typeId)).get(0);
				}
				if (object == null) {
					LOG.error("Unknown TypeId {}", typeId);
					throw new IllegalArgumentException("Unknown TypeID " + typeId);
				}
			}
			return getTypeChildrenIntern(repositoryId, typeId, inclPropDefs, max, skip, typeMorphiaDAO, object);
		}

		public static TypeDefinitionListImpl getTypeChildrenIntern(String repositoryId, String typeId,
				Boolean includePropertyDefinitions, int maxItems, int skipCount, MTypeManagerDAO typeMorphiaDAO,
				MTypeObject object) {
			TypeDefinitionListImpl result = new TypeDefinitionListImpl();
			MDocumentTypeManagerDAO docTypeMorphia = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
			if (typeId != null) {
				List<MTypeObject> childrenList = typeMorphiaDAO.getChildrenIds(typeId, maxItems, skipCount);
				if (childrenList.size() > 0) {
					result.setNumItems(BigInteger.valueOf(childrenList.size()));
					result.setHasMoreItems(childrenList.size() > maxItems - skipCount);
					List<TypeDefinition> resultTypes = childrenList.stream().map(
							t -> getPropertyIncludeObject(repositoryId, t, docTypeMorphia, includePropertyDefinitions))
							.collect(Collectors.<TypeDefinition> toList());
					result.setList(resultTypes);
				} else {
					result.setHasMoreItems(false);
					result.setNumItems(BigInteger.valueOf(childrenList.size()));
					result.setList(Collections.<TypeDefinition> emptyList());
				}

			} else {
				if (skipCount >= 6) {
					result.setHasMoreItems(false);
					result.setNumItems(BigInteger.valueOf(0));
					result.setList(Collections.<TypeDefinition> emptyList());
				} else {
					List<TypeDefinition> resultTypes = new ArrayList<>();
					resultTypes.add(getPropertyIncludeObject(repositoryId,
							typeMorphiaDAO.getById(Arrays.asList("cmis:folder")).get(0), docTypeMorphia,
							includePropertyDefinitions));
					resultTypes.add(getPropertyIncludeObject(repositoryId, docTypeMorphia.getByTypeId("cmis:document"),
							docTypeMorphia, includePropertyDefinitions));
					resultTypes.add(getPropertyIncludeObject(repositoryId,
							typeMorphiaDAO.getById(Arrays.asList("cmis:item")).get(0), docTypeMorphia,
							includePropertyDefinitions));
					resultTypes.add(getPropertyIncludeObject(repositoryId,
							typeMorphiaDAO.getById(Arrays.asList("cmis:policy")).get(0), docTypeMorphia,
							includePropertyDefinitions));
					resultTypes.add(getPropertyIncludeObject(repositoryId,
							typeMorphiaDAO.getById(Arrays.asList("cmis:relationship")).get(0), docTypeMorphia,
							includePropertyDefinitions));
					resultTypes.add(getPropertyIncludeObject(repositoryId,
							typeMorphiaDAO.getById(Arrays.asList("cmis:secondary")).get(0), docTypeMorphia,
							includePropertyDefinitions));
					result.setNumItems(BigInteger.valueOf(resultTypes.size()));
					result.setHasMoreItems(true);
					result.setList(resultTypes);
				}
			}

			return result;
		}

		private static TypeDefinition getPropertyIncludeObject(String repositoryId, MTypeObject type,
				MDocumentTypeManagerDAO docTypeMorphia, Boolean includePropertyDefinition) {
			return getInnerTypeDefinitionContainerImpl(repositoryId, type, docTypeMorphia, includePropertyDefinition)
					.getTypeDefinition();
		}

		/**
		 * getting the TypeDescendants for particular TypeID
		 */
		public static List<TypeDefinitionContainer> getTypeDescendants(String repositoryId, String typeId,
				BigInteger depth, Boolean includePropertyDefinitions, ExtensionsData extension)
				throws IllegalArgumentException, CmisInvalidArgumentException {
			LOG.info("getTypeDescendants for type: {} , repository: {}", typeId, repositoryId);
			boolean inclPropDefs = includePropertyDefinitions == null ? true : includePropertyDefinitions;
			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			MDocumentTypeManagerDAO docTypeMorphia = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MDocumentTypeManagerDAO.class);
			if (typeId != null) {
				MTypeObject object = null;
				if (typeMorphiaDAO.getById(Arrays.asList(typeId)).size() > 0) {
					object = typeMorphiaDAO.getById(Arrays.asList(typeId)).get(0);
				}
				if (object == null) {
					LOG.error("Unknown TypeID {}", typeId);
					throw new IllegalArgumentException("Unknown TypeID " + typeId);
				}

			}

			List<TypeDefinitionContainer> result = null;
			// TODO:implement vresion
			// boolean cmis11 = context.getCmisVersion() !=
			// CmisVersion.CMIS_1_0;
			boolean cmis11 = false;
			if (typeId != null) {
				TypeDefinitionContainer tc = getTypeById(repositoryId, typeId, inclPropDefs,
						depth == null ? -1 : depth.intValue(), cmis11, typeMorphiaDAO, docTypeMorphia);
				if (tc == null) {
					throw new CmisInvalidArgumentException("unknown type id: " + typeId);
				} else {
					result = tc.getChildren();
				}
			} else {
				result = getBaseTyeDefCon(repositoryId, typeMorphiaDAO, docTypeMorphia,
						depth == null ? -1 : depth.intValue(), includePropertyDefinitions);
			}
			if (result == null) {
				LOG.error("unknown typeId {}", typeId);
				throw new CmisInvalidArgumentException("unknown typeId: " + typeId);
			}

			return result;
		}

		/**
		 * getting TypeDefinition for particular TypeID
		 */
		public static TypeDefinitionContainerImpl getTypeById(String repositoryId, String typeId,
				boolean includePropertyDefinitions, int depthParam, boolean cmis11, MTypeManagerDAO typeMorphiaDAO,
				MDocumentTypeManagerDAO docTypeMorphia) {
			LOG.info("getTypeById for type: {} , repository: {}", typeId, repositoryId);
			List<TypeDefinitionContainer> innerChild = new ArrayList<TypeDefinitionContainer>();
			innerChild.clear();
			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			MTypeObject result = null;
			if (typeMorphiaDAO.getById(Arrays.asList(typeId)).size() > 0) {
				result = typeMorphiaDAO.getById(Arrays.asList(typeId)).get(0);
				if (result.getBaseTypeId().value().equals("cmis:document")) {
					CmisDocumentTypeDefinitionImpl docResult = docTypeMorphia.getByTypeId(typeId);
					typeDefinitionContainer = getDocTypeDefContainer(
							getDocTypeObject(repositoryId, docResult, includePropertyDefinitions));
				} else {
					typeDefinitionContainer = getTypeDefContainer(
							getTypeObject(repositoryId, result, includePropertyDefinitions),
							result.getBaseTypeId().value());
				}
				typeDefinitionContainer.setChildren(getChildTypeDefContainer(repositoryId, typeMorphiaDAO,
						docTypeMorphia, typeId, innerChild, depthParam, includePropertyDefinitions));

			}
			return typeDefinitionContainer;

		}

		private static List<TypeDefinitionContainer> getChildTypeDefContainer(String repositoryId,
				MTypeManagerDAO typeMorphiaDAO, MDocumentTypeManagerDAO docTypeMorphia, String typeId,
				List<TypeDefinitionContainer> innerChild, int depth, Boolean includePropertyDefinitions) {
			List<TypeDefinitionContainer> childTypes = null;
			List<MTypeObject> childrenList = typeMorphiaDAO.getChildrenIds(typeId, depth, -1);
			for (MTypeObject child : childrenList) {
				if (child.getId() != null) {
					childTypes = getTypeDesChildrens(repositoryId, child, innerChild, typeMorphiaDAO, docTypeMorphia,
							depth, includePropertyDefinitions);
				}
			}

			return childTypes;
		}

		private static List<TypeDefinitionContainer> getBaseTyeDefCon(String repositoryId,
				MTypeManagerDAO typeMorphiaDAO, MDocumentTypeManagerDAO docTypeMorphia, int depth,
				Boolean includePropertyDefinitions) {
			List<TypeDefinitionContainer> object = new ArrayList<TypeDefinitionContainer>();
			MTypeObject folder = typeMorphiaDAO.getById(Arrays.asList("cmis:folder")).get(0);
			CmisDocumentTypeDefinitionImpl document = docTypeMorphia.getByTypeId("cmis:document");
			MTypeObject policy = typeMorphiaDAO.getById(Arrays.asList("cmis:policy")).get(0);
			MTypeObject relationship = typeMorphiaDAO.getById(Arrays.asList("cmis:relationship")).get(0);
			MTypeObject item = typeMorphiaDAO.getById(Arrays.asList("cmis:item")).get(0);
			MTypeObject secondary = typeMorphiaDAO.getById(Arrays.asList("cmis:secondary")).get(0);

			TypeDefinitionContainerImpl typeFolderDefinitionContainer = getTypeDefinitionContainerImpl(repositoryId,
					folder, typeMorphiaDAO, docTypeMorphia, depth, includePropertyDefinitions);
			TypeDefinitionContainerImpl typeDocumentDefinitionContainer = getDocTypeDefinitionContainerImpl(
					repositoryId, document, typeMorphiaDAO, docTypeMorphia, depth, includePropertyDefinitions);
			TypeDefinitionContainerImpl typePolicyDefinitionContainer = getTypeDefinitionContainerImpl(repositoryId,
					policy, typeMorphiaDAO, docTypeMorphia, depth, includePropertyDefinitions);
			TypeDefinitionContainerImpl typeRelationshipDefinitionContainer = getTypeDefinitionContainerImpl(
					repositoryId, relationship, typeMorphiaDAO, docTypeMorphia, depth, includePropertyDefinitions);
			TypeDefinitionContainerImpl typeItemDefinitionContainer = getTypeDefinitionContainerImpl(repositoryId, item,
					typeMorphiaDAO, docTypeMorphia, depth, includePropertyDefinitions);
			TypeDefinitionContainerImpl typesecondaryDefinitionContainer = getTypeDefinitionContainerImpl(repositoryId,
					secondary, typeMorphiaDAO, docTypeMorphia, depth, includePropertyDefinitions);
			object.add(typeFolderDefinitionContainer);
			object.add(typeDocumentDefinitionContainer);
			object.add(typeItemDefinitionContainer);
			object.add(typePolicyDefinitionContainer);
			object.add(typeRelationshipDefinitionContainer);
			object.add(typesecondaryDefinitionContainer);
			return object;
		}

		private static TypeDefinitionContainerImpl getTypeDefinitionContainerImpl(String repositoryId,
				MTypeObject object, MTypeManagerDAO typeMorphiaDAO, MDocumentTypeManagerDAO docTypeMorphia, int depth,
				Boolean includePropertyDefinitions) {
			TypeDefinitionContainerImpl typeItemDefinitionContainer = getTypeDefContainer(
					getTypeObject(repositoryId, object, includePropertyDefinitions), object.getBaseTypeId().value());
			List<TypeDefinitionContainer> innerItemChild = new ArrayList<TypeDefinitionContainer>();
			innerItemChild.clear();
			typeItemDefinitionContainer.setChildren(getChildTypeDefContainer(repositoryId, typeMorphiaDAO,
					docTypeMorphia, object.getId(), innerItemChild, depth, includePropertyDefinitions));
			return typeItemDefinitionContainer;
		}

		private static TypeDefinitionContainerImpl getDocTypeDefinitionContainerImpl(String repositoryId,
				CmisDocumentTypeDefinitionImpl object, MTypeManagerDAO typeMorphiaDAO,
				MDocumentTypeManagerDAO docTypeMorphia, int depth, Boolean includePropertyDefinitions) {
			TypeDefinitionContainerImpl typeItemDefinitionContainer = getDocTypeDefContainer(
					getDocTypeObject(repositoryId, object, includePropertyDefinitions));
			List<TypeDefinitionContainer> innerItemChild = new ArrayList<TypeDefinitionContainer>();
			innerItemChild.clear();
			typeItemDefinitionContainer.setChildren(getChildTypeDefContainer(repositoryId, typeMorphiaDAO,
					docTypeMorphia, object.getId(), innerItemChild, depth, includePropertyDefinitions));
			return typeItemDefinitionContainer;
		}

		/**
		 * getting TypeDescendants InnerChild for particular TypeID
		 */

		private static List<TypeDefinitionContainer> getTypeDesChildrens(String repositoryId, MTypeObject child,
				List<TypeDefinitionContainer> innerChild, MTypeManagerDAO typeMorphiaDAO,
				MDocumentTypeManagerDAO docTypeMorphia, int depth, Boolean includePropertyDefinitions) {
			List<MTypeObject> childrenList = null;
			List<TypeDefinitionContainer> innerTypeChild = new ArrayList<>();
			TypeDefinitionContainerImpl typeDefinitionContainer = getInnerTypeDefinitionContainerImpl(repositoryId,
					child, docTypeMorphia, includePropertyDefinitions);
			childrenList = typeMorphiaDAO.getChildrenIds(child.getId(), depth, -1);
			if (childrenList.isEmpty()) {
				innerChild.add(typeDefinitionContainer);
			} else {
				for (MTypeObject childType : childrenList) {
					if (childType != null) {
						List<TypeDefinitionContainer> TypeChild = new ArrayList<>();
						TypeChild.clear();
						TypeDefinitionContainerImpl typeInnerDefinitionContainer = getInnerTypeDefinitionContainerImpl(
								repositoryId, childType, docTypeMorphia, includePropertyDefinitions);
						innerTypeChild.add(getTypeDesInnerChild(repositoryId, childType, typeMorphiaDAO, docTypeMorphia,
								depth, includePropertyDefinitions, TypeChild, typeInnerDefinitionContainer));
					}
				}
				typeDefinitionContainer.setChildren(innerTypeChild);
				innerChild.add(typeDefinitionContainer);
			}
			return innerChild;
		}

		private static TypeDefinitionContainerImpl getInnerTypeDefinitionContainerImpl(String repositoryId,
				MTypeObject child, MDocumentTypeManagerDAO docTypeMorphia, Boolean includePropertyDefinitions) {
			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			if (child.getBaseTypeId().value().equals("cmis:document")) {
				CmisDocumentTypeDefinitionImpl docType = docTypeMorphia.getByTypeId(child.getId());
				typeDefinitionContainer = getDocTypeDefContainer(
						getDocTypeObject(repositoryId, docType, includePropertyDefinitions));
			} else {
				typeDefinitionContainer = getTypeDefContainer(
						getTypeObject(repositoryId, child, includePropertyDefinitions), child.getBaseTypeId().value());
			}
			return typeDefinitionContainer;

		}

		private static TypeDefinitionContainer getTypeDesInnerChild(String repositoryId, MTypeObject child,
				MTypeManagerDAO typeMorphiaDAO, MDocumentTypeManagerDAO docTypeMorphia, int depth,
				Boolean includePropertyDefinitions, List<TypeDefinitionContainer> typeChild,
				TypeDefinitionContainerImpl typeInnerDefinitionContainer) {
			List<MTypeObject> childrenList = null;

			childrenList = typeMorphiaDAO.getChildrenIds(child.getId(), depth, -1);
			if (childrenList.isEmpty()) {
				return typeInnerDefinitionContainer;
			} else {
				for (MTypeObject childType : childrenList) {
					if (childType != null) {
						List<TypeDefinitionContainer> typeInnerChild = new ArrayList<>();
						typeInnerChild.clear();
						TypeDefinitionContainerImpl typeInnerChildDefinitionContainer = getInnerTypeDefinitionContainerImpl(
								repositoryId, childType, docTypeMorphia, includePropertyDefinitions);
						typeChild.add(getTypeDesInnerChild(repositoryId, childType, typeMorphiaDAO, docTypeMorphia,
								depth, includePropertyDefinitions, typeInnerChild, typeInnerChildDefinitionContainer));
					}
				}
				typeInnerDefinitionContainer.setChildren(typeChild);
				return typeInnerDefinitionContainer;
			}

		}

		private static MTypeObject getTypeObject(String repositoryId, MTypeObject object,
				Boolean includePropertyDefinitions) {
			List<MTypeObject> innerChildObject = new ArrayList<MTypeObject>();
			innerChildObject.clear();
			Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(object, repositoryId, innerChildObject,
					includePropertyDefinitions);
			MTypeObject resultType = getTypeObjectInstance(object, list);
			return resultType;

		}

		private static CmisDocumentTypeDefinitionImpl getDocTypeObject(String repositoryId,
				CmisDocumentTypeDefinitionImpl object, Boolean includePropertyDefinitions) {
			List<MTypeObject> innerChildObject = new ArrayList<MTypeObject>();
			innerChildObject.clear();
			Map<String, PropertyDefinitionImpl<?>> list = getTypeProperties(object, repositoryId, innerChildObject,
					includePropertyDefinitions);
			CmisDocumentTypeDefinitionImpl resultType = getTypeDocumentObjectInstance(object, list);
			return resultType;

		}

		private static TypeDefinitionContainerImpl getTypeDefContainer(MTypeObject resultType, String type) {

			TypeDefinitionContainerImpl typeDefinitionContainer = null;
			CmisFolderTypeDefinitionImpl resultFolder = null;
			ItemTypeDefinitionImpl resultItem = null;
			CmisPolicyTypeDefinitionImpl resultPolicy = null;
			CmisSecondaryTypeDefinitionImpl resultSecondary = null;
			CmisRelationshipTypeDefinitionImpl resultRelationship = null;
			if (type.equalsIgnoreCase("cmis:folder")) {
				resultFolder = getFolderTypeDefinition(resultType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultFolder);
			} else if (type.equalsIgnoreCase("cmis:item")) {
				resultItem = getItemTypeDefinition(resultType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultItem);
			} else if (type.equalsIgnoreCase("cmis:policy")) {
				resultPolicy = getPolicyTypeDefinition(resultType);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultPolicy);
			} else if (type.equalsIgnoreCase("cmis:relationship")) {
				resultRelationship = getRelationshipTypeDefinitionWithSourceTarget(resultType, null, null);
				typeDefinitionContainer = new TypeDefinitionContainerImpl(resultRelationship);
			} else if (type.equalsIgnoreCase("cmis:secondary")) {
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
		public static List<MTypeObject> getTypeParent(String repositoryId, String parentId,
				List<MTypeObject> innerChild) {
			LOG.info("getTypeParent from repository: {}", repositoryId);
			MTypeManagerDAO typeMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
					.getObjectService(repositoryId, MTypeManagerDAO.class);
			MTypeObject parent = null;
			if (typeMorphiaDAO.getById(Arrays.asList(parentId)).size() > 0) {
				parent = typeMorphiaDAO.getById(Arrays.asList(parentId)).get(0);
			}
			if (parent != null) {
				if (parent.getParentTypeId() == null) {
					LOG.info("Parent Type {}", parent.getId());
					innerChild.add(parent);
				} else {
					LOG.info("Parent Type {}", parent.getId());
					innerChild.add(parent);
					return getTypeParent(repositoryId, parent.getParentTypeId(), innerChild);
				}
			}
			return innerChild;
		}

		private static CmisDocumentTypeDefinitionImpl getDocumentTypeDefinition(CmisDocumentTypeDefinitionImpl result) {
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
		private static CmisFolderTypeDefinitionImpl getFolderTypeDefinition(MTypeObject result) {
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
		private static ItemTypeDefinitionImpl getItemTypeDefinition(MTypeObject result) {
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
				MTypeObject result, List<String> source, List<String> target) {
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
		private static CmisPolicyTypeDefinitionImpl getPolicyTypeDefinition(MTypeObject result) {
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
		private static CmisSecondaryTypeDefinitionImpl getSecondaryTypeDefinition(MTypeObject result) {
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

		@SuppressWarnings("rawtypes")
		public static PropertyDefinitionImpl getPropertyDefinition(PropertyDefinition pro, Boolean inherited) {
			// LOG.info("getPropertyDefinition from {}", pro);
			PropertyDefinitionImpl<?> propertyDefinition = new PropertyDefinitionImpl(pro.getId(), pro.getLocalName(),
					pro.getLocalNamespace(), pro.getDisplayName(), pro.getQueryName(), pro.getDescription(),
					pro.getPropertyType(), pro.getCardinality(), pro.getUpdatability(),
					inherited == null ? pro.isInherited() : inherited, pro.isRequired(), pro.isQueryable(),
					pro.isOrderable(), pro.isOpenChoice());
			return propertyDefinition;
		}

		private static void createFolderForType(TypeDefinition type, String userName, String repositoryId)
				throws IOException, IllegalArgumentException, CmisInvalidArgumentException {
			PropertiesImpl result = new PropertiesImpl();
			PropertyData<?> propertyIDData = new PropertyIdImpl("cmis:objectTypeId", "cmis:folder");
			PropertyData<?> propertyNameData = new PropertyIdImpl("cmis:name", type.getId());
			result.addProperty(propertyIDData);
			result.addProperty(propertyNameData);
			CmisObjectService.Impl.createTypeFolder(repositoryId, result, userName);
		}

		private static MTypeObject getTypeDefinitionManager(TypeDefinition type,
				Map<String, PropertyDefinitionImpl<?>> Mproperty, TypeMutabilityImpl typeMutability) {
			MTypeObject newType = new MTypeObject(type.getId(),
					type.getLocalName() == null ? "localName" : type.getLocalName(),
					type.getLocalNamespace() == null ? "localNameSpace" : type.getLocalNamespace(),
					type.getDisplayName() == null ? "displayName" : type.getDisplayName(),
					type.getQueryName() == null ? "queryName" : type.getQueryName(),
					type.getDescription() == null ? "description" : type.getDescription(), type.getBaseTypeId(),
					type.getParentTypeId(), type.isCreatable(), type.isFileable(),
					type.isQueryable() == null ? false : type.isQueryable(),
					type.isFulltextIndexed() == null ? false : type.isFulltextIndexed(),
					type.isIncludedInSupertypeQuery() == null ? false : type.isIncludedInSupertypeQuery(),
					type.isControllablePolicy(), type.isControllableAcl(), typeMutability, Mproperty);
			return newType;
		}

		private static CmisDocumentTypeDefinitionImpl getDocumentTypeDefinition(DocumentTypeDefinition type,
				Map<String, PropertyDefinitionImpl<?>> Mproperty, TypeMutabilityImpl typeMutability) {
			CmisDocumentTypeDefinitionImpl newType = new CmisDocumentTypeDefinitionImpl(type.getId(),
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
					type.isVersionable() == null ? false : type.isVersionable(), type.getContentStreamAllowed() == null
							? ContentStreamAllowed.NOTALLOWED : type.getContentStreamAllowed());
			return newType;
		}

		private static void addIndex(String repositoryId,
				Map<String, PropertyDefinitionImpl<?>> getPropertyDefinitions) {
			List<String> primaryIndex = getPropertyDefinitions.entrySet().stream()
					.filter(map -> map.getValue().getLocalName().equalsIgnoreCase("primaryKey"))
					.map(t -> "properties." + t.getValue().getId()).collect(Collectors.<String> toList());
			List<String> secondaryIndex = getPropertyDefinitions.entrySet().stream()
					.filter(map -> map.getValue().getLocalName().equalsIgnoreCase("lk_" + map.getValue().getId()))
					.map(t -> "properties." + t.getValue().getId()).collect(Collectors.<String> toList());
			secondaryIndex.parallelStream().collect(Collectors.toCollection(() -> primaryIndex));
			String[] columnsToIndex = primaryIndex.toArray(new String[primaryIndex.size()]);
			if (columnsToIndex.length > 0) {
				DatabaseServiceFactory.getInstance(repositoryId).addIndex(repositoryId, columnsToIndex);
			}
		}

	}
}