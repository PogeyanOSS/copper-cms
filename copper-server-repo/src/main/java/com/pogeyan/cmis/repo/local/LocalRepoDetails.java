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

import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.repo.IRepository;

public class LocalRepoDetails {
	private static final Logger LOG = LoggerFactory.getLogger(LocalRepoDetails.class);
	public Map<String, IRepository> repoStore = new HashMap<String, IRepository>();
	final static LocalRepoDetails instance = new LocalRepoDetails();

	public static LocalRepoDetails get() {
		return instance;
	}

	@SuppressWarnings("unchecked")
	public void setRepoStore() {
		if (repoStore != null) {
			JSONParser parser = new JSONParser();
			try {
				String envVariable = System.getenv("CMIS_REPO_JSON_LOCATION");
				if (envVariable == null) {
					LOG.error("CMIS_REPO_JSON_LOCATION not defined, unable to initialize CMIS");
					throw new CmisInvalidArgumentException(
							"CMIS_REPO_JSON_LOCATION not defined, unable to initialize CMIS");
				}
				Object obj = parser.parse(new FileReader(envVariable));
				JSONArray repoArray = (JSONArray) obj;
				for (Object object : repoArray) {
					JSONObject jsonObject = (JSONObject) object;
					String repositoryId = (String) jsonObject.get("repositoryId");
					String repositoryName = (String) jsonObject.get("repositoryName");
					Map<String, String> DBName = (Map<String, String>) jsonObject.get("db");
					String description = (String) jsonObject.get("description");
					Map<String, String> fileDetails = (Map<String, String>) jsonObject.get("file");
					JSONObject loginObjects = (JSONObject) jsonObject.get("login");
					JSONArray loginDetails = (JSONArray) loginObjects.get("users");
					Map<String, String> loginRepo = (Map<String, String>) jsonObject.get("login");
					if (loginDetails != null) {
						loginRepo.remove("users");
						loginRepo.put("users", loginDetails.toString());
					}
					LocalRepo repo = new LocalRepo(repositoryId, repositoryName, DBName, description, fileDetails,
							loginRepo);
					repoStore.put(repositoryId, repo);
				}
			} catch (Exception e) {
				LOG.error("reading json file exception: {}", ExceptionUtils.getStackTrace(e));
			}
		}
	}
}
