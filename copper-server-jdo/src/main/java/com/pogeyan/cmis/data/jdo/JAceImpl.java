package com.pogeyan.cmis.data.jdo;

import java.util.ArrayList;
import java.util.List;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.Serialized;

import org.apache.chemistry.opencmis.commons.data.Principal;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
public class JAceImpl {
	
	private List<String> permissions;
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
	public JAceImpl(String principal, List<String> permissions) {
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
			permissions = new ArrayList<String>(0);
		}

		return permissions;
	}

	public void setPermissions(List<String> permissions) {
		this.permissions = permissions;
	}

	public boolean isDirect() {
		return isDirect;
	}

	public void setDirect(boolean direct) {
		this.isDirect = direct;
	}

}
