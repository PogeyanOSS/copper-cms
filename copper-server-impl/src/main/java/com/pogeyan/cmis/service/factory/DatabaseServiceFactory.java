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
package com.pogeyan.cmis.service.factory;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.database.IDBClientFactory;
import com.pogeyan.cmis.data.mongo.services.MongoClientFactory;
import com.pogeyan.cmis.factory.RepositoryManagerFactory;

public class DatabaseServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(DatabaseServiceFactory.class);
	public static final String MONGO = "mongo";
	public static final String SQL = "sql";
	static IDBClientFactory dbService = null;

	public static IDBClientFactory getInstance(String repositoryId) {
		if (dbService == null) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Repository type{}", repositoryId);
			}
			
			Map<String, String> db = RepositoryManagerFactory.getDatabaseDetails(repositoryId);
			if (MONGO.equals(db.get("type"))) {
				dbService = MongoClientFactory.createDatabaseService();
			}
		}
		
		return dbService;
	}
}
