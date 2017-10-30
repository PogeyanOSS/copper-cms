package com.pogeyan.cmis.data.mongo;

import org.mongodb.morphia.annotations.Embedded;

import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;

@Embedded
public class MongoPropertyDefinition<T> extends PropertyDefinitionImpl<T> {
	private static final long serialVersionUID = 1L;
}
