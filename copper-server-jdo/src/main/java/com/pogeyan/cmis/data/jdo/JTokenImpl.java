package com.pogeyan.cmis.data.jdo;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;

import com.pogeyan.cmis.api.data.common.TokenChangeType;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
@Extension(vendorName = "datanucleus", key = "read-write", value = "true")
public class JTokenImpl {
	TokenChangeType changeType;
	Long time;

	public JTokenImpl() {

	}

	public JTokenImpl(TokenChangeType changetype, Long time) {
		this.changeType = changetype;
		this.time = time;
	}

	public TokenChangeType getChangeType() {
		return changeType;
	}

	public void setChangeType(TokenChangeType changetype) {
		this.changeType = changetype;
	}

	public Long getTime() {
		return time;
	}

	public void setTime(Long time) {
		this.time = time;
	}
}
