package com.pogeyan.cmis.data.jdo;

import java.util.List;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Embedded;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
@Extension(vendorName = "datanucleus", key = "read-write", value = "true")
public class JAclImpl {
	private String aclPropagation;
	@Embedded
	private List<JAceImpl> ace;
	private boolean extract;

	public JAclImpl() {

	}

	public JAclImpl(List<JAceImpl> acl, String aclPropagation, boolean extract) {
		this.aclPropagation = aclPropagation;
		this.setAce(acl);
		this.setExtract(extract);
	}

	public String getAclPropagation() {
		return aclPropagation;
	}

	public void setAclPropagation(String aclPropagation) {
		this.aclPropagation = aclPropagation;
	}

	public List<JAceImpl> getAce() {
		return ace;
	}

	public void setAce(List<JAceImpl> acl) {
		this.ace = acl;
	}

	public boolean isExtract() {
		return extract;
	}

	public void setExtract(boolean extract) {
		this.extract = extract;
	}
}
