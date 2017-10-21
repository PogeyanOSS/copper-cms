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
package com.pogeyan.cmis.data.objects;

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Field;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Index;
import org.mongodb.morphia.annotations.IndexOptions;
import org.mongodb.morphia.annotations.Indexes;

@Entity(value = "objectData", noClassnameStored = true)
@Indexes(@Index(fields = { @Field("name") }, options = @IndexOptions(unique = true)))
public class MBaseObject {
	@Id
	private ObjectId id;
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
	private MToken token;
	private String internalPath;
	private String path;
	private List<String> policies;
	private MAclImpl acl;
	private String parentId;

	private Map<String, Object> properties;

	public String getRepositoryId() {
		return repositoryId;
	}

	public void setRepositoryId(String repositoryId) {
		this.repositoryId = repositoryId;
	}

	public MBaseObject() {

	}

	public MBaseObject(ObjectId id, String name, BaseTypeId baseId, String typeId, String fRepositoryId,
			List<String> secondaryTypeIds, String description, String createdBy, String modifiedBy, MToken token,
			String internalPath, Map<String, Object> properties, List<String> policies, MAclImpl acl, String path,
			String parentId) {
		super();
		this.id = id;
		this.name = name;
		this.baseId = baseId;
		this.typeId = typeId;
		this.repositoryId = fRepositoryId;
		this.secondaryTypeIds = secondaryTypeIds;
		this.description = description;
		this.createdBy = createdBy;
		this.modifiedBy = modifiedBy;
		this.token = token;
		this.internalPath = internalPath;
		this.path = path;
		this.properties = properties;
		this.policies = policies;
		this.acl = acl;
		this.createdAt = System.currentTimeMillis();
		this.modifiedAt = System.currentTimeMillis();
		this.parentId = parentId;
	}

	public ObjectId getId() {
		return id;
	}

	public void setId(ObjectId id) {

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

	public MToken getChangeToken() {
		return this.token;
	}

	public void setChangeToken(MToken token) {
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

	public MAclImpl getAcl() {
		return acl;
	}

	public void setAcl(MAclImpl acl) {
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

}
