package com.pogeyan.cmis.data.mongo;

import org.mongodb.morphia.annotations.Entity;

import com.pogeyan.cmis.api.data.common.TokenImpl;

@Entity(noClassnameStored = true)
public class MongoToken extends TokenImpl {

	public MongoToken() {
		super();
	}
}
