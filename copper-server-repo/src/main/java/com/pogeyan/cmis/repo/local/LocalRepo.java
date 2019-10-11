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
package com.pogeyan.cmis.repo.local;

import java.util.Map;

import com.pogeyan.cmis.api.repo.IRepository;

public class LocalRepo implements IRepository {
	private String repositoryId;
	private String repositoryName;
	private Map<String, String> DBName;
	private String description;
	private Map<String, String> fileDetails;
	private Map<String, String> loginDetails;
	private String tenantId;
	public LocalRepo() {
	}

	public LocalRepo(String repositoryId, String repositoryName, Map<String, String> dBName, String description,
			Map<String, String> fileDetails, Map<String, String> loginDetails, String tenantId) {
		super();
		this.repositoryId = repositoryId;
		this.repositoryName = repositoryName;
		DBName = dBName;
		this.description = description;
		this.fileDetails = fileDetails;
		this.loginDetails = loginDetails;
		this.tenantId = tenantId;
	}

	@Override
	public String getRepositoryId() {
		return this.repositoryId;
	}

	@Override
	public String getRepositoryName() {
		return this.repositoryName;
	}

	@Override
	public Map<String, String> getDBName() {
		return this.DBName;
	}

	@Override
	public String getDescription() {
		return this.description;
	}

	@Override
	public Map<String, String> getFile() {
		return this.fileDetails;
	}

	@Override
	public Map<String, String> getLogin() {
		return this.loginDetails;
	}

	public void setLoginDetails(Map<String, String> loginDetails) {
		this.loginDetails = loginDetails;
	}

	public void setRepositoryId(String repositoryId) {
		this.repositoryId = repositoryId;
	}

	public void setRepositoryName(String repositoryName) {
		this.repositoryName = repositoryName;
	}

	public void setDBName(Map<String, String> dBName) {
		this.DBName = dBName;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void setFileDetails(Map<String, String> fileDetails) {
		this.fileDetails = fileDetails;
	}

	public String getTenantId() {
		return tenantId;
	}

	public void setTenantId(String tenantId) {
		this.tenantId = tenantId;
	}

}
