package com.pogeyan.cmis.data.jdo;

import java.util.List;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Element;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.PrimaryKey;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
public class JAclImpl {
	@PrimaryKey
	private String aclId;
	private String baseId;
	private String aclPropagation;
	@Element(column = "aclId")
	private List<JAceImpl> ace;
	private boolean extract;

	public JAclImpl() {

	}

	public JAclImpl(List<JAceImpl> acl, String aclPropagation, boolean extract) {
		this.aclPropagation = aclPropagation;
		this.setAce(acl);
		this.setExtract(extract);
	}

	public String getAclId() {
		return aclId;
	}

	public void setAclId(String aclId) {
		this.aclId = aclId;
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

	public String getBaseId() {
		return baseId;
	}

	public void setBaseId(String baseId) {
		this.baseId = baseId;
	}

}
