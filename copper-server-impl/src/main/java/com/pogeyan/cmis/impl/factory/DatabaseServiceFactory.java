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
package com.pogeyan.cmis.impl.factory;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IDBClientFactory;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;

public class DatabaseServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(DatabaseServiceFactory.class);
	private static final Map<String, IDBClientFactory> dbFactory = new HashMap<>();
	public static final String MONGO = "mongo";
	public static final String SQL = "sql";

	public static IDBClientFactory getInstance(String repositoryId) {
		LOG.debug("Repository type: {}", repositoryId);
		Map<String, String> db = RepositoryManagerFactory.getDatabaseDetails(repositoryId);
		String dbType = db.get("type");
		if (dbFactory.containsKey(dbType)) {
			return dbFactory.get(dbType);
		}

		return null;
	}

	public static void add(IDBClientFactory dbFactoryInstance) {
		if (!dbFactory.containsKey(dbFactoryInstance.getType())) {
			dbFactory.put(dbFactoryInstance.getType(), dbFactoryInstance);
		}
	}

	public static void close(String repoId) {
		if (repoId != null) {
			getInstance(repoId).close(repoId);
		} else {
			dbFactory.values().forEach(dbInstance -> {
				dbInstance.close(null);
			});
		}
	}
}
