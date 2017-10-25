package com.pogeyan.cmis.data.objects;

import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.mongodb.morphia.annotations.Entity;

@Entity(noClassnameStored = true)
public class MongoAceImpl extends AccessControlEntryImpl {

	private static final long serialVersionUID = 1L;

}
