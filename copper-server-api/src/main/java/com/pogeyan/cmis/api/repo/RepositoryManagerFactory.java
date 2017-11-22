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
package com.pogeyan.cmis.api.repo;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RepositoryManagerFactory implements IRepositoryManager {
	private static final Logger LOG = LoggerFactory.getLogger(RepositoryManagerFactory.class);
	static RepositoryManagerFactory repo = null;
	static IRepositoryStore repoStoreSetting = null;

	public static IRepositoryManager getInstance() {
		return repo;
	}

	@Override
	public IRepository getRepository(String repositoryId) {
		IRepository repository = getRepositoryStore().getRepository(repositoryId);
		return repository;
	}

	@Override
	public void init(IRepositoryStore repoStore) {
		LOG.info("RepositoryManager initiallize: {}", repoStore);
		repo = new RepositoryManagerFactory();
		repoStoreSetting = repoStore;
	}

	@Override
	public IRepositoryStore getRepositoryStore() {
		return repoStoreSetting;
	}

	public static Map<String, String> getFileDetails(String repositoryId) {
		IRepository repository = getInstance().getRepository(repositoryId);
		Map<String, String> fileDetails = repository.getFile();
		return fileDetails;
	}

	public static Map<String, String> getLoginDetails(String repositoryId) {
		IRepository repository = getInstance().getRepository(repositoryId);
		return repository.getLogin();
	}

	public static Map<String, String> getDatabaseDetails(String repositoryId) {
		IRepository repository = getInstance().getRepository(repositoryId);
		return repository.getDBName();
	}
}
