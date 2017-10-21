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

import java.util.ArrayList;
import java.util.List;

import com.pogeyan.cmis.api.repo.IRepository;
import com.pogeyan.cmis.api.repo.IRepositoryStore;

public class LocalRepoImpl implements IRepositoryStore {

	public LocalRepoImpl() {
		setRepoStore();
	}

	@Override
	public List<IRepository> getRepositories(String repositoryId) {
		List<IRepository> repoDeatils = new ArrayList<IRepository>(LocalRepoDetails.get().repoStore.values());
		return repoDeatils;
	}

	@Override
	public IRepository getRepository(String repositoryId) {
		return LocalRepoDetails.get().repoStore.get(repositoryId);
	}

	private void setRepoStore() {
		LocalRepoDetails.get().setRepoStore();
	}
}
