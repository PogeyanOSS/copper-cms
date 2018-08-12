package com.pogeyan.cmis.impl.TypePermission;

import java.util.ArrayList;
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
import com.pogeyan.cmis.api.data.services.MNavigationDocServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.impl.utils.DBUtils;

public class TypePermissionService implements ITypePermissionService {
	private static final Logger LOG = LoggerFactory.getLogger(TypePermissionService.class);
	private Map<String, Map<String, List<String>>> typeAccessData = new HashMap<>();
	private Map<String, Map<String, List<String>>> typePermissionData = new HashMap<>();

	@Override
	public Boolean checkTypeAccess(String repositoryId, String typeId) {
		if (typeAccessData.get(repositoryId) != null) {
			Map<String, List<String>> typeAccess = typeAccessData.get(repositoryId);
			if (typeAccess.get(typeId) != null) {
				return true;
			}
		}
		return false;
	}

	@Override
	public List<String> getTypeIdAccess(String repositoryId) {
		if (typeAccessData.get(repositoryId) != null) {
			Map<String, List<String>> typeAccess = typeAccessData.get(repositoryId);
			List<String> typeId = new ArrayList<>();
			typeAccess.entrySet().stream().forEach((k) -> {
				typeId.add(k.getKey().toString());
			});
			return typeId;
		}
		return null;
	}

	@Override
	public List<String> getFieldAccess(String repositoryId, String typeId) {
		if (typeAccessData.get(repositoryId) != null) {
			Map<String, List<String>> typeAccess = typeAccessData.get(repositoryId);
			if (typeAccess.get(typeId) != null) {
				return typeAccess.get(typeId);
			}
		}
		return null;
	}

	@Override
	public Boolean checkPermissionAccess(String repositoryId, String typeId, String access) {
		if (typePermissionData.get(repositoryId) != null) {
			Map<String, List<String>> typePermission = typePermissionData.get(repositoryId);
			if (typePermission.get(typeId) != null) {
				List<String> permission = typePermission.get(typeId);
				if (permission.contains(access)) {
					return true;
				}
			}
		}
		return false;
	}

	public void setDetails(String repositoryId, IUserGroupObject[] role) {
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
						"roles eq " + userRole.getGroupDN(), typeManagerDAO);
				if (children.size() > 0) {
					String sourceId = children.get(0).getId();
					relationMdPath = relationFolder.getInternalPath() + relationFolder.getId() + ",";
					List<? extends IDocumentObject> permission_set_Children = navigationMorphiaDAO.getChildren(
							relationMdPath, new String[] { "system" }, true, 0, 0, null, null,
							"cmis:sourceId eq " + sourceId, typeManagerDAO);
					if (permission_set_Children.size() > 0) {
						Map<String, List<String>> typePermission = new HashMap<>();
						Map<String, List<String>> fieldPermission = new HashMap<>();
						for (IDocumentObject obj : permission_set_Children) {
							if (obj != null) {
								String targetId = obj.getProperties().get("cmis:targetId").toString();
								readPermissionSet(repositoryId, targetId, relationMdPath, navigationMorphiaDAO,
										typeManagerDAO, typePermission, fieldPermission);
							}
						}
						this.typePermissionData.put(repositoryId, typePermission);
						this.typeAccessData.put(repositoryId, fieldPermission);
					}
				}
			}

		} catch (Exception e) {
			LOG.error("setDetails Exception: {}, repository: {}", ExceptionUtils.getStackTrace(e), repositoryId);
			throw new MongoException(e.toString());
		}

	}

	@SuppressWarnings("unchecked")
	private void readPermissionSet(String repositoryId, String id, String relationMdPath,
			MNavigationDocServiceDAO navigationMorphiaDAO, MTypeManagerDAO typeManagerDAO,
			Map<String, List<String>> typePermission, Map<String, List<String>> fieldPermission) {
		List<? extends IDocumentObject> typeAccess_set_Children = navigationMorphiaDAO.getChildren(relationMdPath,
				new String[] { "system" }, true, 0, 0, null, null, "cmis:sourceId eq " + id, typeManagerDAO);
		if (typeAccess_set_Children.size() > 0) {
			for (IDocumentObject obj : typeAccess_set_Children) {
				if (obj != null) {
					String targetId = obj.getProperties().get("cmis:targetId").toString();
					IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, targetId, null);
					if (data != null) {
						String typeId = data.getProperties().get("fv:typeId").toString();
						List<String> permissionAccess = (List<String>) data.getProperties().get("fv:permission");
						if (typePermission.get(typeId) != null) {
							List<String> combinedStream = Stream
									.concat(permissionAccess.stream(), typePermission.get(typeId).stream()).distinct()
									.collect(Collectors.toList());
							typePermission.put(typeId, combinedStream);
						} else {
							typePermission.put(typeId, permissionAccess);
						}

						readFleidAccess(repositoryId, targetId, typeId, relationMdPath, navigationMorphiaDAO,
								typeManagerDAO, fieldPermission);
					}
				}

			}
		}
	}

	@SuppressWarnings("unchecked")
	private void readFleidAccess(String repositoryId, String id, String typeId, String relationMdPath,
			MNavigationDocServiceDAO navigationMorphiaDAO, MTypeManagerDAO typeManagerDAO,
			Map<String, List<String>> fieldPermission) {
		List<? extends IDocumentObject> field_set_Children = navigationMorphiaDAO.getChildren(relationMdPath,
				new String[] { "system" }, true, 0, 0, null, null, "cmis:sourceId eq " + id, typeManagerDAO);
		if (field_set_Children.size() > 0) {
			for (IDocumentObject obj : field_set_Children) {
				if (obj != null) {
					String targetId = obj.getProperties().get("cmis:targetId").toString();
					IBaseObject data = DBUtils.BaseDAO.getByObjectId(repositoryId, targetId, null);
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

	}
}
