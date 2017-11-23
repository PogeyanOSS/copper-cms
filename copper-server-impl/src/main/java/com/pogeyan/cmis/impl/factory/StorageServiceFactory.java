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

import com.pogeyan.cmis.api.storage.IStorageFactory;
import com.pogeyan.cmis.api.storage.IStorageService;

public class StorageServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(StorageServiceFactory.class);

	public static final String STORAGE = "storage";
	static Map<String, IStorageFactory> storageFactory = new HashMap<String, IStorageFactory>();

	static public IStorageService createStorageService(Map<String, String> parameters) {
		String storageType = parameters.get(STORAGE);
		LOG.debug("DB FileSytem Setting type: {}", storageType);
		IStorageService store = null;
		if (storageFactory.get(storageType) != null) {
			return storageFactory.get(storageType).getStorageService(parameters);
		}
		
		return store;
	}

	public static void add(IStorageFactory storageServiceFactory) {
		storageFactory.put(storageServiceFactory.getType(), storageServiceFactory);
	}

}
