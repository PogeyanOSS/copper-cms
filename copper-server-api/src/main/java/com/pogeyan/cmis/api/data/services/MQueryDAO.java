package com.pogeyan.cmis.api.data.services;

import java.util.List;

import com.pogeyan.cmis.data.query.IQueryResponse;
import com.pogeyan.cmis.data.query.QueryRequest;

public interface MQueryDAO {
	
	public List<IQueryResponse> query(QueryRequest request, String[] principalIds);

}
