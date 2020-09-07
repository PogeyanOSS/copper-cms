package com.pogeyan.cmis.data.dynamo;

import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;

public class DynamoPrincipalImpl extends AccessControlPrincipalDataImpl {
	private static final long serialVersionUID = 1L;

	public DynamoPrincipalImpl(String principalId) {
		super(principalId);
	}
}
