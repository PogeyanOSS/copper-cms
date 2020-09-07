package com.pogeyan.cmis.data.dynamo;

import java.util.List;

import org.apache.chemistry.opencmis.commons.data.Ace;

import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;

public class DynamoAclImpl extends AccessControlListImplExt {
	
	public DynamoAclImpl() {
		super();
	}
	
	public DynamoAclImpl(List<Ace> aces) {
		super(aces);
	}

}
