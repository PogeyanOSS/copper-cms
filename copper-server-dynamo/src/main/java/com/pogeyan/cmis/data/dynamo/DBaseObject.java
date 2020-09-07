package com.pogeyan.cmis.data.dynamo;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.data.Principal;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.common.TokenImpl;

import scala.util.parsing.combinator.PackratParsers.PackratParser;

import java.util.UUID;

public class DBaseObject implements IBaseObject {

	private String pk;
	private String sk;
	private String name;
	private String time;
	private String path;
	private String internalPath;
	private Map<String, Object> attributes = new HashMap<String, Object>();

	public DBaseObject() {

	}

	public DBaseObject(String pk, String sk, String time, String name, String path, String internalPath, Map<String, Object> attributes) {
		this.pk = pk;
		this.sk = sk;
		this.time = time;
		this.name = name;
		this.path = path;
		this.internalPath  = internalPath;
		this.attributes = attributes;
	}

	public DBaseObject(String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, TokenImpl token,
			String internalPath, Map<String, Object> properties, List<String> policies, Acl acl, String path,
			String parentId) {

		pk = typeId;
		sk = UUID.randomUUID().toString();
		this.time = token.getTime().toString();;
		this.name = name;
		this.path = path;
		this.internalPath = internalPath;
		setAttributes(baseId, fRepositoryId, secondaryTypeIds, description, createdBy, modifiedBy, token, properties,
				policies, acl, parentId);
	}

	public String getPk() {
		return pk;
	}

	public void setPk(String pk) {
		this.pk = pk;
	}

	public String getSk() {
		return sk;
	}

	public void setSk(String sk) {
		this.sk = sk;
	}

	public Map<String, Object> getAttributes() {
		return attributes;
	}

	public void setAttributes(BaseTypeId baseId, String fRepositoryId, List<String> secondaryTypeIds,
			String description, String createdBy, String modifiedBy,TokenImpl token, Map<String, Object> properties,
			List<String> policies, Acl acl, String parentId) {

		attributes.put("baseId", baseId.value());
		attributes.put("fRepositoryId", fRepositoryId);
		attributes.put("secondaryTypeIds", secondaryTypeIds);
		attributes.put("description", description);
		attributes.put("createdBy", createdBy);
		attributes.put("modifiedBy", modifiedBy);
		attributes.put("changeType", token.getChangeType().value());
		attributes.put("properties", properties);
		attributes.put("policies", policies);
		attributes.put("acl", convertDynamoAcl(acl));
		attributes.put("parentId", parentId);
		attributes.put("createdAt", System.currentTimeMillis());
		attributes.put("modifiedAt", System.currentTimeMillis());

	}

	public static Map<String,Object> convertDynamoAcl(Acl acl) {
		if (acl != null) {
			AccessControlListImplExt acessControl = (AccessControlListImplExt) acl;
			
			List<Map<String, Object>> list = new ArrayList<Map<String, Object>>(acl.getAces().size());
			//Object[] list = new Object[acl.getAces().size()];
			int count = 0;
			for (Ace ace : acl.getAces()) {
				Map<String, Object> aces = new HashMap<String,Object>();
				aces.put("direct", true);
				aces.put("principal", new DynamoPrincipalImpl(ace.getPrincipalId()));
				aces.put("permission", ace.getPermissions());
				list.add(aces);
				//count++;
				
			}
			Map<String,Object> dAcl = new HashMap<String,Object>();
			dAcl.put("aces",list);
			dAcl.put("aclPropogation",acessControl.getAclPropagation());
			dAcl.put("exact", true);
			
			return dAcl;
		}

		return null;
	}
	
	public String getTime() {
		return time;
	}

	@Override
	public String getId() {
		// TODO Auto-generated method stub
		return sk;
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return name;
	}

	@Override
	public BaseTypeId getBaseId() {
		return BaseTypeId.fromValue((String)attributes.get("baseId"));
	}

	@Override
	public String getTypeId() {
		// TODO Auto-generated method stub
		return pk;
	}

	@Override
	public String getRepositoryId() {
		// TODO Auto-generated method stub
		return (String)attributes.get("repositoryId");
	}

	@Override
	public List<String> getSecondaryTypeIds() {
		// TODO Auto-generated method stub
		return (List<String>)attributes.get("secondaryTypeIds");
	}

	@Override
	public String getDescription() {
		// TODO Auto-generated method stub
		return (String)attributes.get("description");
	}

	@Override
	public String getCreatedBy() {
		// TODO Auto-generated method stub
		return (String)attributes.get("createdBy");
	}

	@Override
	public String getModifiedBy() {
		// TODO Auto-generated method stub
		return (String)attributes.get("modifiedBy");
	}

	@Override
	public Long getCreatedAt() {
		// TODO Auto-generated method stub
		return (Long)attributes.get("createdAt");
	}

	@Override
	public Long getModifiedAt() {
		// TODO Auto-generated method stub
		return (Long)attributes.get("modifiedAt");
	}

	@Override
	public TokenImpl getChangeToken() {
		DynamoToken dToken = new DynamoToken();
		BigDecimal changeType = (BigDecimal) attributes.get("changeType");
		dToken.setChangeType(TokenChangeType.getTokenValue(changeType.intValue()));
		dToken.setTime(Long.parseLong(time));
		return dToken;
	}

	@Override
	public String getInternalPath() {
		// TODO Auto-generated method stub
		return internalPath;
	}

	@Override
	public String getPath() {
		// TODO Auto-generated method stub
		return path;
	}

	@Override
	public List<String> getPolicies() {
		// TODO Auto-generated method stub
		return (List<String>)attributes.get("policies");
	}

	@Override
	public AccessControlListImplExt getAcl() {
		// TODO Auto-generated method stub
		Map<String,Object> acl = (Map<String,Object>)attributes.get("acl");
		DynamoAclImpl dAcl = new DynamoAclImpl();
		
		List<Map<String,Object>> list = (List<Map<String, Object>>) acl.get("aces");
		List<Ace> aces = new ArrayList<Ace>();
		for(Map<String,Object> ace: list) {
			DynamoAceImpl dAce = new DynamoAceImpl();
			dAce.setDirect((Boolean)ace.get("direct"));
			dAce.setPermissions((List<String>) ace.get("permissions"));
			dAce.setPrincipal((DynamoPrincipalImpl) ace.get("principal"));
			aces.add(dAce);
		}
		
		
		dAcl.setAces(aces);
		dAcl.setAclPropagation((String) acl.get("aclPropagation"));
		dAcl.setExact((Boolean)acl.get("exact"));
		return dAcl;
	}

	@Override
	public String getParentId() {
		// TODO Auto-generated method stub
		return (String) attributes.get("parentId");
	}

	@Override
	public Map<String, Object> getProperties() {
		// TODO Auto-generated method stub
		return (Map<String, Object>) attributes.get("properties");
	}

	@Override
	public void setProperties(Map<String, Object> props) {
		attributes.replace("properties", props);
	}

	@Override
	public void setName(String name) {
		this.name = name;;

	}
	
	public void setId(String id) {
		sk = id;
	} 
}
