package com.pogeyan.cmis.impl.TypePermission;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.auth.IUserGroupObject;
import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.common.TypePermissionType;
import com.pogeyan.cmis.api.data.services.MNavigationDocServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.impl.utils.DBUtils;

public class TypePermissionService implements ITypePermissionService {
	private static final Logger LOG = LoggerFactory.getLogger(TypePermissionService.class);

	@Override
	public Boolean checkTypeAccess(String repositoryId, IUserGroupObject[] role, String typeId) {
		if (typeId != null) {
			List<String> typeAccess = CacheProviderServiceFactory.getTypeCacheServiceProvider().get(repositoryId,
					Arrays.asList(typeId + "_permission"));
			if (typeAccess == null) {
				return setDetails(repositoryId, role, typeId, true) != null ? true : false;
			}
			return true;
		}
		return false;

	}

	@Override
	public List<String> getFieldAccess(String repositoryId, IUserGroupObject[] role, String typeId) {
		if (typeId != null) {
			List<String> fieldAccess = CacheProviderServiceFactory.getTypeCacheServiceProvider().get(repositoryId,
					Arrays.asList(typeId + "_field"));
			return fieldAccess != null ? fieldAccess : setDetails(repositoryId, role, typeId, false);
		}
		return null;
	}

	@Override
	public Boolean checkPermissionAccess(String repositoryId, IUserGroupObject[] role, String typeId,
			TypePermissionType permissionAccess) {
		if (typeId != null) {
			List<String> typeAccess = CacheProviderServiceFactory.getTypeCacheServiceProvider().get(repositoryId,
					Arrays.asList(typeId + "_permission"));
			if (typeAccess != null) {
				return typeAccess.contains(permissionAccess.value());
			} else {
				List<String> permission = setDetails(repositoryId, role, typeId, true);
				return permission != null ? permission.contains(permissionAccess.value()) : false;
			}
		}
		return false;

	}

	public List<String> setDetails(String repositoryId, IUserGroupObject[] role, String typeId, boolean getTypeAccess) {
		LOG.info("className: {},  methodName: {}, repositoryId: {}, user role: {}", "TypePermissionService",
				"setDetails", repositoryId, role);
		IBaseObject rootData = null;
		IBaseObject relationFolder = null;
		MNavigationDocServiceDAO navigationMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId)
				.getObjectService(repositoryId, MNavigationDocServiceDAO.class);
		MTypeManagerDAO typeManagerDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
				MTypeManagerDAO.class);
		try {
			rootData = DBUtils.BaseDAO.getByPath(repositoryId, "/");
			relationFolder = DBUtils.BaseDAO.getByPath(repositoryId, "/cmis_ext:relationship");
			String path = "," + rootData.getId() + ",";
			String relationMdPath = null;
			for (IUserGroupObject userRole : role) {
				List<? extends IDocumentObject> children = new ArrayList<>();
				children = navigationMorphiaDAO.getChildren(path, new String[] { "system" }, true, 0, 0, null, null,
						"roles eq " + userRole.getGroupDN(), typeManagerDAO, null);
				if (children.size() > 0) {
					String sourceId = children.get(0).getId();
					relationMdPath = relationFolder.getInternalPath() + relationFolder.getId() + ",";
					List<? extends IDocumentObject> permission_set_Children = navigationMorphiaDAO.getChildren(
							relationMdPath, new String[] { "system" }, true, 0, 0, null, null,
							"cmis:sourceId eq " + sourceId, typeManagerDAO, null);
					if (permission_set_Children.size() > 0) {
						Map<String, List<String>> typePermission = new HashMap<>();
						Map<String, List<String>> fieldPermission = new HashMap<>();
						for (IDocumentObject obj : permission_set_Children) {
							if (obj != null) {
								String targetId = obj.getProperties().get("cmis:targetId").toString();
								readPermissionSet(repositoryId, targetId, relationMdPath, navigationMorphiaDAO,
										typeManagerDAO, typePermission, fieldPermission, typeId);
							}
						}
						if (typePermission.get(typeId) != null && fieldPermission.get(typeId) != null) {
							CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId,
									typeId + "_permission", typePermission.get(typeId));
							CacheProviderServiceFactory.getTypeCacheServiceProvider().put(repositoryId,
									typeId + "_field", fieldPermission.get(typeId));
							return getTypeAccess ? typePermission.get(typeId) : fieldPermission.get(typeId);
						}
					}
				}
			}

		} catch (Exception e) {
			LOG.error("className: {}, methodName: {}, repository: {}, typeId: {}, exception: {}",
					"TypePermissionService", "setDetails", repositoryId, typeId, ExceptionUtils.getStackTrace(e));
			throw new MongoException(e.toString());
		}
		return null;

	}

	@SuppressWarnings("unchecked")
	private void readPermissionSet(String repositoryId, String id, String relationMdPath,
			MNavigationDocServiceDAO navigationMorphiaDAO, MTypeManagerDAO typeManagerDAO,
			Map<String, List<String>> typePermission, Map<String, List<String>> fieldPermission, String typeId) {
		try {
			LOG.info("className: {},  methodName: {}, repositoryId: {}, typeId: {}", "TypePermissionService",
					"readPermissionSet", repositoryId, typeId);
			List<? extends IDocumentObject> typeAccess_set_Children = navigationMorphiaDAO.getChildren(relationMdPath,
					new String[] { "system" }, true, 0, 0, null, null, "cmis:sourceId eq " + id, typeManagerDAO, null);
			if (typeAccess_set_Children.size() > 0) {
				for (IDocumentObject obj : typeAccess_set_Children) {
					if (obj != null) {
						String targetId = obj.getProperties().get("cmis:targetId").toString();
						IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, targetId, null, null);
						if (data != null) {
							String objectTypeId = data.getProperties().get("fv:typeId").toString();
							if (typeId.equalsIgnoreCase(objectTypeId)) {
								List<String> permissionAccess = (List<String>) data.getProperties()
										.get("fv:permission");
								if (typePermission.get(typeId) != null) {
									List<String> combinedStream = Stream
											.concat(permissionAccess.stream(), typePermission.get(typeId).stream())
											.distinct().collect(Collectors.toList());
									typePermission.put(typeId, combinedStream);
								} else {
									typePermission.put(typeId, permissionAccess);
								}

								readFieldAccess(repositoryId, targetId, typeId, relationMdPath, navigationMorphiaDAO,
										typeManagerDAO, fieldPermission);
								break;
							}
						}
					}

				}
			}
		} catch (Exception e) {
			LOG.error("className: {}, methodName: {}, repository: {}, typeId: {}, exception: {}",
					"TypePermissionService", "readPermissionSet", repositoryId, typeId,
					ExceptionUtils.getStackTrace(e));
			throw new MongoException(e.toString());
		}
	}

	@SuppressWarnings("unchecked")
	private void readFieldAccess(String repositoryId, String id, String typeId, String relationMdPath,
			MNavigationDocServiceDAO navigationMorphiaDAO, MTypeManagerDAO typeManagerDAO,
			Map<String, List<String>> fieldPermission) {
		try {
			LOG.info("className: {},  methodName: {}, repositoryId: {}, typeId: {}", "TypePermissionService",
					"readFleidAccess", repositoryId, typeId);
			List<? extends IDocumentObject> field_set_Children = navigationMorphiaDAO.getChildren(relationMdPath,
					new String[] { "system" }, true, 0, 0, null, null, "cmis:sourceId eq " + id, typeManagerDAO, null);
			if (field_set_Children.size() > 0) {
				for (IDocumentObject obj : field_set_Children) {
					if (obj != null) {
						String targetId = obj.getProperties().get("cmis:targetId").toString();
						IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, targetId, null, null);
						if (data != null) {
							List<String> fieldAccess = (List<String>) data.getProperties().get("fv:property");
							if (fieldPermission.get(typeId) != null) {
								List<String> combinedStream = Stream
										.concat(fieldAccess == null ? new ArrayList<String>().stream()
												: fieldAccess.stream(), fieldPermission.get(typeId).stream())
										.distinct().collect(Collectors.toList());
								fieldPermission.put(typeId, combinedStream);
							} else {
								if (fieldAccess == null || fieldAccess.isEmpty()) {
									fieldPermission.put(typeId, new ArrayList<String>());
								} else {
									fieldPermission.put(typeId, fieldAccess);
								}
							}
						}
					}
				}
			} else {
				fieldPermission.put(typeId, new ArrayList<>());
			}
		} catch (Exception e) {
			LOG.error("className: {}, methodName: {}, repository: {}, typeId: {}, exception: {}",
					"TypePermissionService", "readFleidAccess", repositoryId, typeId, ExceptionUtils.getStackTrace(e));
			throw new MongoException(e.toString());
		}

	}
}
