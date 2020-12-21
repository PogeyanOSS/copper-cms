package com.pogeyan.cmis.data.mongo;

import java.io.Serializable;

import org.mongodb.morphia.annotations.Embedded;

import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;

@Embedded
public class MongoTypeMutability extends TypeMutabilityImpl implements Serializable {
	private static final long serialVersionUID = 8395769514668668414L;

	public MongoTypeMutability() {

	}

	public MongoTypeMutability(Boolean canCreate, Boolean canUpdate, Boolean canDelete) {
		super(canCreate, canUpdate, canDelete);
	}
}
