package com.pogeyan.cmis.data.jdo;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.Embedded;
import javax.jdo.annotations.Extension;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.PrimaryKey;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.bson.types.ObjectId;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.ISettableBaseObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenImpl;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
@Extension(vendorName = "datanucleus", key = "read-write", value = "true")
public class JBaseObject implements IBaseObject, ISettableBaseObject {
	@PrimaryKey
	private String id;
	private String name;
	private BaseTypeId baseId;
	private String typeId;
	private String repositoryId;
	private List<String> secondaryTypeIds;
	private String description;
	private String createdBy;
	private String modifiedBy;
	private Long createdAt;
	private Long modifiedAt;
	@Embedded
	private JTokenImpl token;
	private String internalPath;
	private String path;
	private List<String> policies;
	private JAclImpl acl;
	private String parentId;

	private Map<String, Object> properties;

	public String getRepositoryId() {
		return repositoryId;
	}

	public void setRepositoryId(String repositoryId) {
		this.repositoryId = repositoryId;
	}

	public JBaseObject() {

	}

	public JBaseObject(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId) {
		super();
		this.id = (new ObjectId()).toString();
		this.name = name;
		this.baseId = baseId;
		this.typeId = typeId;
		this.repositoryId = fRepositoryId;
		this.secondaryTypeIds = secondaryTypeIds;
		this.description = description;
		this.createdBy = createdBy;
		this.modifiedBy = modifiedBy;
		this.token = convertJDOToken(token);
		this.internalPath = internalPath;
		this.path = path;
		this.properties = properties;
		this.policies = policies;
		this.acl = convertJDOAcl(acl);
		this.createdAt = System.currentTimeMillis();
		this.modifiedAt = System.currentTimeMillis();
		this.parentId = parentId;

	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public BaseTypeId getBaseId() {
		return this.baseId;
	}

	public void setBaseId(BaseTypeId baseId) {
		this.baseId = baseId;
	}

	public String getTypeId() {
		return this.typeId;
	}

	public void setTypeId(String type) {
		this.typeId = type;
	}

	public List<String> getSecondaryTypeIds() {
		return this.secondaryTypeIds;
	}

	public String getDescription() {
		return this.description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getCreatedBy() {
		return this.createdBy;
	}

	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}

	public String getModifiedBy() {
		return this.modifiedBy;
	}

	public void setModifiedBy(String modifiedBy) {
		this.modifiedBy = modifiedBy;
	}

	public Long getCreatedAt() {
		return this.createdAt;
	}

	public void setCreatedAt(Long createdAt) {
		this.createdAt = createdAt;
	}

	public Long getModifiedAt() {
		return this.modifiedAt;
	}

	public void setModifiedAt(Long calendar) {
		this.modifiedAt = calendar;
	}

	public Map<String, Object> getProperties() {
		return this.properties;
	}

	public void setProperties(Map<String, Object> props) {
		this.properties = props;
	}

	public TokenImpl getChangeToken() {
		TokenImpl m = new TokenImpl(token.getChangeType(), token.getTime());
		return m;
	}

	public void setChangeToken(JTokenImpl token) {
		this.token = token;
	}

	public boolean hasParent() {
		return false;
	}

	public String getInternalPath() {
		return this.internalPath;
	}

	public void setInternalPath(String path) {
		this.internalPath = path;
	}

	public List<String> getPolicies() {
		return policies;
	}

	public void setPolicies(List<String> policies) {
		this.policies = policies;
	}

	public AccessControlListImplExt getAcl() {
		List<Ace> list = new ArrayList<Ace>(acl.getAce().size());
		for (JAceImpl ace : acl.getAce()) {
			AccessControlEntryImpl aces = new AccessControlEntryImpl();
			aces.setDirect(true);
			aces.setPrincipal(new AccessControlPrincipalDataImpl(ace.getPrincipalId()));
			aces.setPermissions(ace.getPermissions());
			list.add(aces);
		}
		AccessControlListImplExt mAcl = new AccessControlListImplExt(list, acl.getAclPropagation(), true);
		return mAcl;
	}

	public void setAcl(JAclImpl acl) {
		this.acl = acl;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getParentId() {
		return parentId;
	}

	public void setParentId(String parentId) {
		this.parentId = parentId;
	}

	public static JAclImpl convertJDOAcl(Acl acl) {
		if (acl != null) {
			AccessControlListImplExt acessControl = (AccessControlListImplExt) acl;
			List<JAceImpl> list = new ArrayList<JAceImpl>(acl.getAces().size());
			for (Ace ace : acl.getAces()) {
				JAceImpl aces = new JAceImpl();
				aces.setDirect(true);
				aces.setPrincipal(ace.getPrincipalId());
				aces.setPermissions(ace.getPermissions());
				list.add(aces);
			}
			JAclImpl mAcl = new JAclImpl(list, acessControl.getAclPropagation(), true);
			return mAcl;
		}

		return null;
	}

	public static JTokenImpl convertJDOToken(TokenImpl token) {
		JTokenImpl mongoToken = new JTokenImpl();
		mongoToken.setChangeType(token.getChangeType());
		mongoToken.setTime(token.getTime());
		return mongoToken;

	}

}
