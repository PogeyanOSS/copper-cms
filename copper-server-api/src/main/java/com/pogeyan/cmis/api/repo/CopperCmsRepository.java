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
package com.pogeyan.cmis.api.repo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.chemistry.opencmis.commons.data.PermissionMapping;
import org.apache.chemistry.opencmis.commons.data.RepositoryInfo;
import org.apache.chemistry.opencmis.commons.definitions.PermissionDefinition;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.CapabilityAcl;
import org.apache.chemistry.opencmis.commons.enums.CapabilityChanges;
import org.apache.chemistry.opencmis.commons.enums.CapabilityContentStreamUpdates;
import org.apache.chemistry.opencmis.commons.enums.CapabilityJoin;
import org.apache.chemistry.opencmis.commons.enums.CapabilityOrderBy;
import org.apache.chemistry.opencmis.commons.enums.CapabilityQuery;
import org.apache.chemistry.opencmis.commons.enums.CapabilityRenditions;
import org.apache.chemistry.opencmis.commons.enums.CmisVersion;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.SupportedPermissions;
import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AclCapabilitiesDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.BindingsObjectFactoryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.CreatablePropertyTypesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.NewTypeSettableAttributesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PermissionDefinitionDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PermissionMappingDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RepositoryCapabilitiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RepositoryInfoImpl;
import org.apache.chemistry.opencmis.commons.server.CallContext;
import org.apache.chemistry.opencmis.commons.spi.BindingsObjectFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CopperCmsRepository {
	private static final Logger LOG = LoggerFactory.getLogger(CopperCmsRepository.class);
	private static final String CMIS_READ = "cmis:read";
	private static final String CMIS_WRITE = "cmis:write";
	private static final String CMIS_ALL = "cmis:all";
	private static final String OPENCMIS_VERSION;
	private static final String OPENCMIS_SERVER;

	public static final String ROOT_ID = "@ROOT@";
	/** Types. */
	// private final MongoTypeManager typeManager;
	/** Repository id. */
	private final String repositoryId;
	/** CMIS 1.0 repository info. */
	private final RepositoryInfo repositoryInfo10;
	/** CMIS 1.1 repository info. */
	private final RepositoryInfo repositoryInfo11;
	/** Users. */
	private final Map<String, Boolean> readWriteUserMap;
	private final BindingsObjectFactory fObjectFactory;

	static {
		Package p = Package.getPackage("com.pogeyan.cmis.mongo");
		if (p == null) {
			OPENCMIS_VERSION = "?";
			OPENCMIS_SERVER = "Copper CMS";
		} else {
			String ver = p.getImplementationVersion();
			OPENCMIS_VERSION = (null == ver ? "?" : ver);
			OPENCMIS_SERVER = "Copper CMS/" + OPENCMIS_VERSION;
		}
	}

	/**
	 * Gets a repository object by id.
	 */
	public CopperCmsRepository(final String repositoryId) {
		// check repository id
		if (repositoryId == null || repositoryId.trim().length() == 0) {
			throw new IllegalArgumentException("Invalid repository id!");
		}
		this.repositoryId = repositoryId;
		// set up read-write user map
		readWriteUserMap = new HashMap<String, Boolean>();
		// set up repository infos
		repositoryInfo10 = createRepositoryInfo(this.repositoryId, CmisVersion.CMIS_1_0);
		repositoryInfo11 = createRepositoryInfo(this.repositoryId, CmisVersion.CMIS_1_1);

		fObjectFactory = new BindingsObjectFactoryImpl();
		LOG.info("MongoRepository repositoryId:{}", repositoryId);
	}

	public BindingsObjectFactory getObjectFactory() {
		return fObjectFactory;
	}

	/**
	 * Returns the id of this repository.
	 */
	public String getRepositoryId() {
		return repositoryId;
	}

	/**
	 * Sets read-only flag for the given user.
	 */
	public void setUserReadOnly(String user) {
		if (user == null || user.length() == 0) {
			return;
		}

		readWriteUserMap.put(user, true);
	}

	/**
	 * Sets read-write flag for the given user.
	 */
	public void setUserReadWrite(String user) {
		if (user == null || user.length() == 0) {
			return;
		}

		readWriteUserMap.put(user, false);
	}

	// --- CMIS operations ---
	/**
	 * CMIS getRepositoryInfo.
	 */
	public RepositoryInfo getRepositoryInfo(CallContext context) {
		// checkUser(context, false);

		if (context.getCmisVersion() == CmisVersion.CMIS_1_0) {
			return repositoryInfo10;
		} else {
			return repositoryInfo11;
		}
	}

	/**
	 * Checks if the user in the given context is valid for this repository and
	 * if the user has the required permissions.
	 */
	@SuppressWarnings("unused")
	private boolean checkUser(CallContext context, boolean writeRequired) {
		if (context == null) {
			throw new CmisPermissionDeniedException("No user context!");
		}
		Boolean readOnly = readWriteUserMap.get(context.getUsername());

		if (readOnly.booleanValue() && writeRequired) {
			LOG.error("No write permission!");
			throw new CmisPermissionDeniedException("No write permission!");
		}

		return readOnly.booleanValue();
	}

	@SuppressWarnings("serial")
	private RepositoryInfo createRepositoryInfo(String repositoryId, CmisVersion cmisVersion) {
		String rootFolderId = ROOT_ID;
		// repository info
		RepositoryInfoImpl repoInfo = new RepositoryInfoImpl();
		repoInfo.setId(repositoryId == null ? "inMem" : repositoryId);
		repoInfo.setName("Pogeyan MongoDB CMIS Repository");
		repoInfo.setDescription("Pogeyan MongoDB CMIS  Repository (Version: " + OPENCMIS_VERSION + ")");
		repoInfo.setRootFolder(rootFolderId);
		repoInfo.setPrincipalAnonymous("");
		repoInfo.setPrincipalAnyone("");
		repoInfo.setThinClientUri("");
		repoInfo.setChangesIncomplete(Boolean.TRUE);
		repoInfo.setLatestChangeLogToken("token-24");
		repoInfo.setVendorName("Apache Chemistry");
		repoInfo.setProductName(OPENCMIS_SERVER);
		repoInfo.setProductVersion(OPENCMIS_VERSION);

		// set capabilities
		RepositoryCapabilitiesImpl caps = new RepositoryCapabilitiesImpl();
		caps.setAllVersionsSearchable(false);
		caps.setCapabilityAcl(CapabilityAcl.MANAGE);
		caps.setCapabilityChanges(CapabilityChanges.OBJECTIDSONLY);
		caps.setCapabilityContentStreamUpdates(CapabilityContentStreamUpdates.ANYTIME);
		caps.setCapabilityJoin(CapabilityJoin.NONE);
		caps.setCapabilityQuery(CapabilityQuery.BOTHCOMBINED);
		caps.setCapabilityRendition(CapabilityRenditions.READ);
		caps.setIsPwcSearchable(false);
		caps.setIsPwcUpdatable(true);
		caps.setSupportsGetDescendants(true);
		caps.setSupportsGetFolderTree(true);
		caps.setSupportsMultifiling(true);
		caps.setSupportsUnfiling(true);
		caps.setSupportsVersionSpecificFiling(false);
		caps.setCapabilityAcl(CapabilityAcl.MANAGE);

		AclCapabilitiesDataImpl aclCaps = new AclCapabilitiesDataImpl();
		aclCaps.setAclPropagation(AclPropagation.OBJECTONLY);
		aclCaps.setSupportedPermissions(SupportedPermissions.BASIC);

		// permissions
		List<PermissionDefinition> permissions = new ArrayList<PermissionDefinition>();
		permissions.add(createPermission(CMIS_READ, "Read"));
		permissions.add(createPermission(CMIS_WRITE, "Write"));
		permissions.add(createPermission(CMIS_ALL, "All"));
		if (cmisVersion == CmisVersion.CMIS_1_1) {
			NewTypeSettableAttributesImpl typeAttrs = new NewTypeSettableAttributesImpl();
			typeAttrs.setCanSetControllableAcl(false);
			typeAttrs.setCanSetControllablePolicy(false);
			typeAttrs.setCanSetCreatable(true);
			typeAttrs.setCanSetDescription(true);
			typeAttrs.setCanSetDisplayName(true);
			typeAttrs.setCanSetFileable(false);
			typeAttrs.setCanSetFulltextIndexed(false);
			typeAttrs.setCanSetId(true);
			typeAttrs.setCanSetIncludedInSupertypeQuery(false);
			typeAttrs.setCanSetLocalName(true);
			typeAttrs.setCanSetLocalNamespace(true);
			typeAttrs.setCanSetQueryable(false);
			typeAttrs.setCanSetQueryName(true);
			caps.setNewTypeSettableAttributes(typeAttrs);
		}
		aclCaps.setPermissionDefinitionData(permissions);

		// mapping
		List<PermissionMapping> list = new ArrayList<PermissionMapping>();
		list.add(createMapping(PermissionMapping.CAN_GET_DESCENDENTS_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_CHILDREN_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_PARENTS_FOLDER, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_FOLDER_PARENT_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_CREATE_DOCUMENT_FOLDER, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_CREATE_FOLDER_FOLDER, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_CREATE_RELATIONSHIP_SOURCE, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_CREATE_RELATIONSHIP_TARGET, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_PROPERTIES_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_VIEW_CONTENT_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_UPDATE_PROPERTIES_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_MOVE_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_MOVE_TARGET, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_MOVE_SOURCE, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_DELETE_OBJECT, CMIS_WRITE));

		list.add(createMapping(PermissionMapping.CAN_DELETE_TREE_FOLDER, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_SET_CONTENT_DOCUMENT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_DELETE_CONTENT_DOCUMENT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_ADD_TO_FOLDER_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_REMOVE_FROM_FOLDER_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_CHECKOUT_DOCUMENT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_CANCEL_CHECKOUT_DOCUMENT, CMIS_WRITE));

		list.add(createMapping(PermissionMapping.CAN_CHECKIN_DOCUMENT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_GET_ALL_VERSIONS_VERSION_SERIES, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_OBJECT_RELATIONSHIPS_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_ADD_POLICY_OBJECT, CMIS_WRITE));
		list.add(createMapping(PermissionMapping.CAN_REMOVE_POLICY_OBJECT, CMIS_WRITE));

		list.add(createMapping(PermissionMapping.CAN_GET_APPLIED_POLICIES_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_GET_ACL_OBJECT, CMIS_READ));
		list.add(createMapping(PermissionMapping.CAN_APPLY_ACL_OBJECT, CMIS_ALL));

		Map<String, PermissionMapping> map = new LinkedHashMap<String, PermissionMapping>();
		for (PermissionMapping pm : list) {
			map.put(pm.getKey(), pm);
		}

		List<BaseTypeId> changesOnType;
		// CMIS 1.1 extensions
		if (cmisVersion == CmisVersion.CMIS_1_1) {
			repoInfo.setCmisVersionSupported(CmisVersion.CMIS_1_1.value());
			repoInfo.setCmisVersion(CmisVersion.CMIS_1_1);
			changesOnType = new ArrayList<BaseTypeId>() {
				{
					add(BaseTypeId.CMIS_DOCUMENT);
					add(BaseTypeId.CMIS_FOLDER);
					add(BaseTypeId.CMIS_ITEM);
				}
			};

			Set<PropertyType> propertyTypeSet = new HashSet<PropertyType>() {
				{
					add(PropertyType.BOOLEAN);
					add(PropertyType.DATETIME);
					add(PropertyType.DECIMAL);
					add(PropertyType.HTML);
					add(PropertyType.ID);
					add(PropertyType.INTEGER);
					add(PropertyType.STRING);
					add(PropertyType.URI);
				}
			};
			CreatablePropertyTypesImpl creatablePropertyTypes = new CreatablePropertyTypesImpl();
			creatablePropertyTypes.setCanCreate(propertyTypeSet);
			caps.setCreatablePropertyTypes(creatablePropertyTypes);
			caps.setCapabilityOrderBy(CapabilityOrderBy.COMMON);
		} else {
			repoInfo.setCmisVersionSupported(CmisVersion.CMIS_1_0.value());
			repoInfo.setCmisVersion(CmisVersion.CMIS_1_0);
			changesOnType = new ArrayList<BaseTypeId>() {
				{
					add(BaseTypeId.CMIS_DOCUMENT);
					add(BaseTypeId.CMIS_FOLDER);
				}
			};
		}

		repoInfo.setChangesOnType(changesOnType);
		aclCaps.setPermissionMappingData(map);
		repoInfo.setAclCapabilities(aclCaps);
		repoInfo.setCapabilities(caps);
		return repoInfo;
	}

	private static PermissionDefinition createPermission(String permission, String description) {
		PermissionDefinitionDataImpl pd = new PermissionDefinitionDataImpl();
		pd.setId(permission);
		pd.setDescription(description);
		return pd;
	}

	private static PermissionMapping createMapping(String key, String permission) {
		PermissionMappingDataImpl pm = new PermissionMappingDataImpl();
		pm.setKey(key);
		pm.setPermissions(Collections.singletonList(permission));
		return pm;
	}

	public void close() {
	}

}