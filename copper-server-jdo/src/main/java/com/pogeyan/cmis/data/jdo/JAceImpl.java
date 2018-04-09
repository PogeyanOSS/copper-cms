package com.pogeyan.cmis.data.jdo;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Element;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.PrimaryKey;
import org.apache.chemistry.opencmis.commons.data.Principal;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
public class JAceImpl {
	@PrimaryKey
	private String aceId;

	public String getAceId() {
		return aceId;
	}

	public void setAceId(String aceId) {
		this.aceId = aceId;
	}

	private String baseId;
	@Element(column = "aceId")
	private List<JPermission> permissions;
	private String principal;
	private boolean isDirect = true;

	/**
	 * Constructor.
	 */
	public JAceImpl() {
	}

	/**
	 * Constructor.
	 */
	public JAceImpl(String principal, List<JPermission> permissions) {
		this.principal = principal;
		this.permissions = permissions;
	}

	public Principal getPrincipal() {
		return principal == null ? null : new AccessControlPrincipalDataImpl(principal);
	}

	public String getPrincipalId() {
		return principal == null ? null : principal;
	}

	public void setPrincipal(String principal) {
		this.principal = principal;
	}

	public List<String> getPermissions() {
		if (permissions == null) {
			return new ArrayList<String>(0);
		}
		List<String> permissionString = permissions.stream().map(t -> t.getPermissionRule())
				.collect(Collectors.toList());

		return permissionString;
	}

	public void setPermissions(List<JPermission> permissions) {
		this.permissions = permissions;
	}

	public boolean isDirect() {
		return isDirect;
	}

	public void setDirect(boolean direct) {
		this.isDirect = direct;
	}

	public String getBaseId() {
		return baseId;
	}

	public void setBaseId(String baseId) {
		this.baseId = baseId;
	}

}
