package com.pogeyan.cmis.data.jdo;

import org.apache.chemistry.opencmis.commons.data.Acl;

public class JAclConverter {
	private Acl acl;
	private String id;

	public JAclConverter() {

	}

	public JAclConverter(String id, Acl acl) {
		this.id = id;
		this.acl = acl;
	}

	public Acl getAcl() {
		return this.acl;
	}

	public void setAcl(Acl acl) {
		this.acl = acl;
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}
}
