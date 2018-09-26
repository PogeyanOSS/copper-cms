package com.pogeyan.cmis.api.data;

import java.util.List;

import org.apache.chemistry.opencmis.commons.data.Ace;

public interface IAclService {
	public List<Ace> beforeCreateAcl(String principalId);
}
