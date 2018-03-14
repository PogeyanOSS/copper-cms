package com.pogeyan.cmis.data.services;

import java.util.List;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.services.MDiscoveryServiceDAO;

public class JDiscoveryServiceDAOImpl implements MDiscoveryServiceDAO {

	@Override
	public List<? extends IBaseObject> getLatestChanges(String repositoryId, long changeLogToken, int maxItems,
			String[] mappedColumns) {
		return null;
	}

	@Override
	public long getLatestTokenChildrenSize(String repositoryId, long latestChangeToken) {
		return 0;
	}

}
