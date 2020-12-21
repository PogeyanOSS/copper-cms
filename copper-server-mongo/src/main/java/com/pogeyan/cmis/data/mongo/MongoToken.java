package com.pogeyan.cmis.data.mongo;

import java.io.Serializable;

import org.mongodb.morphia.annotations.Entity;

import com.pogeyan.cmis.api.data.common.TokenImpl;

@Entity(noClassnameStored = true)
public class MongoToken extends TokenImpl implements Serializable {
	private static final long serialVersionUID = -1805174908977436578L;

	public MongoToken() {
		super();
	}
}
