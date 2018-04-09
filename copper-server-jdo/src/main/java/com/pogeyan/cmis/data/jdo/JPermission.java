package com.pogeyan.cmis.data.jdo;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
public class JPermission {
	private String baseId;
	private String PermissionRule;

	public JPermission() {

	}

	public JPermission(String baseId, String PermissionRule) {
		this.setBaseId(baseId);
		this.PermissionRule = PermissionRule;
	}

	public String getPermissionRule() {
		return PermissionRule;
	}

	public void setPermissionRule(String permissionRule) {
		PermissionRule = permissionRule;
	}

	public String getBaseId() {
		return baseId;
	}

	public void setBaseId(String baseId) {
		this.baseId = baseId;
	}
}
