package com.pogeyan.cmis.data.services;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.TokenImpl;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;

public class JBaseObjectDAOImpl implements MBaseObjectDAO {

	@Override
	public IBaseObject getLatestToken(String repoistoryId) {
		return null;
	}

	@Override
	public void delete(String repoistoryId, String objectId, boolean forceDelete, TokenImpl token) {

	}

	@Override
	public void update(String repoistoryId, String objectId, Map<String, Object> updateProps) {
	}

	@Override
	public List<? extends IBaseObject> filter(String repoistoryId, Map<String, Object> fieldNames,
			boolean includePagination, int maxItems, int skipCount, String[] mappedColumns) {
		return null;
	}

	@Override
	public void commit(String repoistoryId, IBaseObject entity) {

	}

	@Override
	public IBaseObject createObjectFacade(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId) {
		JTypeManagerDAOImpl typeMorphia = new JTypeManagerDAOImpl();
		List<? extends TypeDefinition> typeDef = typeMorphia.getById(fRepositoryId, Arrays.asList(typeId));
		return null;
	}

}
