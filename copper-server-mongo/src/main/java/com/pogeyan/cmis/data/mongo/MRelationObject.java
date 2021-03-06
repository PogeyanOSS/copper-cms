/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.data.mongo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Field;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Index;
import org.mongodb.morphia.annotations.IndexOptions;
import org.mongodb.morphia.annotations.Indexes;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IRelationObject;
import com.pogeyan.cmis.api.data.ISettableBaseObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.data.common.TokenImpl;

@Entity(value = "relationData", noClassnameStored = true)
@Indexes(@Index(fields = { @Field("name") }, options = @IndexOptions(unique = true)))
public class MRelationObject implements IRelationObject, ISettableBaseObject, Serializable {
	private static final long serialVersionUID = -8927949155958535058L;
	@Id
	private String id;
	private String name;
	private BaseTypeId baseId;
	private String typeId;
	private String description;
	private String createdBy;
	private String modifiedBy;
	private Long createdAt;
	private Long modifiedAt;
	private MongoToken token;
	private MongoAclImpl acl;
	private String parentId;
	private Map<String, Object> properties;

	public MRelationObject() {

	}

	public MRelationObject(String name, BaseTypeId baseId, String typeId, String description, String createdBy, String modifiedBy, TokenImpl token,
		Map<String, Object> properties, Acl acl,
			String parentId) {
		super();
		this.id = (new ObjectId()).toString();
		this.name = name;
		this.baseId = baseId;
		this.typeId = typeId;
		this.description = description;
		this.createdBy = createdBy;
		this.modifiedBy = modifiedBy;
		this.token = convertMongoToken(token);
		this.properties = properties;
		this.acl = convertMongoAcl(acl);
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
		return this.token;
	}

	public void setChangeToken(MongoToken token) {
		this.token = token;
	}

	public boolean hasParent() {
		return false;
	}

	public MongoAclImpl getAcl() {
		return acl;
	}

	public void setAcl(MongoAclImpl acl) {
		this.acl = acl;
	}

	public String getParentId() {
		return parentId;
	}

	public void setParentId(String parentId) {
		this.parentId = parentId;
	}

	public static MongoAclImpl convertMongoAcl(Acl acl) {
		if (acl != null) {
			AccessControlListImplExt acessControl = (AccessControlListImplExt) acl;
			List<Ace> list = new ArrayList<Ace>(acl.getAces().size());
			for (Ace ace : acl.getAces()) {
				MongoAceImpl aces = new MongoAceImpl();
				aces.setDirect(true);
				aces.setPrincipal(new MongoPrincipalImpl(ace.getPrincipalId()));
				aces.setPermissions(ace.getPermissions());
				list.add(aces);
			}
			MongoAclImpl mAcl = new MongoAclImpl();
			mAcl.setAces(list);
			mAcl.setAclPropagation(acessControl.getAclPropagation());
			mAcl.setExact(true);
			return mAcl;
		}

		return null;
	}

	public static MongoToken convertMongoToken(TokenImpl token) {
		MongoToken mongoToken = new MongoToken();
		mongoToken.setChangeType(token.getChangeType());
		mongoToken.setTime(token.getTime());
		return mongoToken;

	}
}
