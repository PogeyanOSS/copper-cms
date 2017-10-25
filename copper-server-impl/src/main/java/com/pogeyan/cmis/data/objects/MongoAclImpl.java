package com.pogeyan.cmis.data.objects;

import java.util.List;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.mongodb.morphia.annotations.Entity;

import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;

@Entity(noClassnameStored = true)
public class MongoAclImpl extends AccessControlListImplExt {
	private static final long serialVersionUID = 1L;
	
	public MongoAclImpl() {
		super();
	}
	
	public MongoAclImpl(List<Ace> aces) {
		super(aces);
	}
	
	
}
