package com.pogeyan.cmis.data.mongo;

import java.io.Serializable;

import org.mongodb.morphia.annotations.Embedded;

import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;

@Embedded
public class MongoPropertyDefinition<T> extends PropertyDefinitionImpl<T> implements Serializable {

	private static final long serialVersionUID = -7010803221316208284L;

}
