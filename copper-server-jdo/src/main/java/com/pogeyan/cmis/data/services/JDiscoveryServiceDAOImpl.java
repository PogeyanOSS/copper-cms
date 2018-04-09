package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.Extent;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.services.MDiscoveryServiceDAO;

public class JDiscoveryServiceDAOImpl implements MDiscoveryServiceDAO {

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends IBaseObject> getLatestChanges(String repositoryId, long changeLogToken, int maxItems,
			String[] mappedColumns) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		if (pm != null) {
			Class<?> objectClass = JDOHelper.Impl.load(repositoryId, "cmis:folder", true);
			Extent<?> QueryExtent = pm.getExtent(objectClass, true);
			Query<?> query = pm.newQuery(QueryExtent);
			query.declareParameters("Long changeLogToken");
			query.setFilter("token.time > changeLogToken");
			Map<String, Object> value = new HashMap<>();
			value.put("changeLogToken", changeLogToken);
			if (maxItems > 0) {
				query.setRange(0, maxItems);
			}
			if (mappedColumns != null && mappedColumns.length > 0) {
			}
			return (List<? extends IBaseObject>) query.executeWithMap(value);
		}
		return new ArrayList<>();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public long getLatestTokenChildrenSize(String repositoryId, long latestChangeToken) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		if (pm != null) {
			Class<?> objectClass = JDOHelper.Impl.load(repositoryId, "cmis:folder", true);
			Extent<?> QueryExtent = pm.getExtent(objectClass, true);
			Query<?> query = pm.newQuery(QueryExtent);
			query.declareParameters("Long changeLogToken");
			query.setFilter("token.time > changeLogToken");
			Map<String, Object> value = new HashMap<>();
			value.put("changeLogToken", latestChangeToken);
			List<IBaseObject> result = (List<IBaseObject>) query.executeWithMap(value);
			return result.size();
		}
		return 0;
	}

}
