package com.pogeyan.cmis.data.mongo;

import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.mongodb.morphia.annotations.Entity;

@Entity(noClassnameStored = true)
public class MongoPrincipalImpl extends AccessControlPrincipalDataImpl {
	private static final long serialVersionUID = 1L;

	public MongoPrincipalImpl(String principalId) {
		super(principalId);
	}
}
