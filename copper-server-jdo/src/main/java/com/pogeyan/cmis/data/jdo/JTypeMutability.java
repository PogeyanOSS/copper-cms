package com.pogeyan.cmis.data.jdo;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;

import com.pogeyan.cmis.api.data.common.TypeMutabilityImpl;
@Cacheable("false")
@PersistenceCapable(detachable="true")
@DatastoreIdentity(strategy=IdGeneratorStrategy.IDENTITY)
@Extension(vendorName="datanucleus", key="read-write", value="true")
public class JTypeMutability extends TypeMutabilityImpl {

}
