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
package com.pogeyan.cmis.repo;

import java.util.HashMap;
import java.util.Map;

import org.mongodb.morphia.annotations.Embedded;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Field;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Index;
import org.mongodb.morphia.annotations.IndexOptions;
import org.mongodb.morphia.annotations.Indexes;
import com.pogeyan.cmis.api.repo.IRepository;

@Entity(value = "repository", noClassnameStored = true)
@Indexes(@Index(fields = { @Field("name") }, options = @IndexOptions(unique = true)))
public class MRepository implements IRepository {
	@Id
	private String repositoryId;
	private String repositoryName;
	private Map<String, String> db;
	private String description;
	private Map<String, String> file;
	@Embedded
	private Map<String, String> login = new HashMap<String, String>();
	boolean isDisabled;
	private String tenantId;

	public MRepository() {
	}

	public MRepository(String repositoryId, String repositoryName, Map<String, String> dBName, Map<String, String> file,
			Map<String, String> login) {
		super();
		this.repositoryId = repositoryId;
		this.repositoryName = repositoryName;
		db = dBName;
		this.file = file;
		this.login = login;
	}

	@Override
	public String getRepositoryId() {
		return this.repositoryId;
	}

	public void setRepositoryId(String repositoryId) {
		this.repositoryId = repositoryId;
	}

	@Override
	public String getRepositoryName() {
		return this.repositoryName;
	}

	public void setRepositoryName(String repositoryName) {
		this.repositoryName = repositoryName;
	}

	@Override
	public Map<String, String> getDBName() {
		return this.db;
	}

	public void setDBName(Map<String, String> DBName) {
		this.db = DBName;
	}

	@Override
	public Map<String, String> getFile() {
		return this.file;
	}

	public void setFile(Map<String, String> file) {
		this.file = file;
	}

	@Override
	public Map<String, String> getLogin() {
		return this.login;
	}

	public void setLogin(Map<String, String> login) {
		this.login = login;
	}

	@Override
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getTenantId() {
		return tenantId;
	}

	public void setTenantId(String tenantId) {
		this.tenantId = tenantId;
	}
}
