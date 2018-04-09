package com.pogeyan.cmis.data.jdo;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
@Extension(vendorName = "datanucleus", key = "read-write", value = "true")
public class JRelationship {
	private String baseId;
	private String sourceId;
	private String targetId;

	public JRelationship(String baseId, String sourceId, String targetId) {
		this.baseId = baseId;
		this.sourceId = sourceId;
		this.targetId = targetId;
	}

	public String getSourceId() {
		return sourceId;
	}

	public void setSourceId(String sourceId) {
		this.sourceId = sourceId;
	}

	public String getTargetId() {
		return targetId;
	}

	public void setTargetId(String targetId) {
		this.targetId = targetId;
	}

	public String getBaseId() {
		return baseId;
	}

	public void setBaseId(String baseId) {
		this.baseId = baseId;
	}
}
