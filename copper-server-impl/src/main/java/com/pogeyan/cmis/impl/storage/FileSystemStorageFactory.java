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
package com.pogeyan.cmis.impl.storage;

import java.util.Map;

import com.pogeyan.cmis.api.storage.IRepositoryStorageSettings;
import com.pogeyan.cmis.api.storage.IStorageFactory;
import com.pogeyan.cmis.api.storage.IStorageService;

public class FileSystemStorageFactory implements IStorageFactory {
	final FileSystemStorageService fileSystemService = new FileSystemStorageService();

	@Override
	public IStorageService getStorageService(Map<String, String> parameters) {
		FileSystemStorageStoreSettings fileStoreSetting = new FileSystemStorageStoreSettings(
				parameters.get("location"));
		this.fileSystemService.setStoreSettings((IRepositoryStorageSettings) fileStoreSetting);
		return this.fileSystemService;
	}

	@Override
	public String getType() {
		return "local";
	}

}
