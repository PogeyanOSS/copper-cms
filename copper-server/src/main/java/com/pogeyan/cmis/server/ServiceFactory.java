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
package com.pogeyan.cmis.server;

import java.io.File;

public class ServiceFactory {

	private static ServiceFactory instance = new ServiceFactory();

	public static ServiceFactory getInstance() {
		return instance;
	}

	public File getTempDirectory() {
		String tempDir = System.getProperty("java.io.tmpdir");
		return new File(tempDir);
	}

	/**
	 * Returns {@code false}, do not encrypt temporary files.
	 */
	public boolean encryptTempFiles() {
		return false;
	}

	/**
	 * Returns a threshold of 4 MiB.
	 */
	public int getMemoryThreshold() {
		return 4 * 1024 * 1024;
	}

	/**
	 * Returns a max size of 4 GiB.
	 */
	public long getMaxContentSize() {
		return (long) 4 * 1024 * 1024 * 1024;
	}
}
