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
package com.pogeyan.cmis.api.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The Globals configuration utility class.
 */
public class Globals {
	private static Properties versionProps = new Properties();
	private static final Logger LOG = LoggerFactory.getLogger(Globals.class);

	static {
		try (InputStream resourceStream = Globals.class.getClassLoader().getResourceAsStream("version.properties")) {
			versionProps.load(resourceStream);
		} catch (IOException e) {
			LOG.error("Error in loading the version.properties file, error: {}", ExceptionUtils.getStackTrace(e));
		}
	}

	public static String getAppVersion() {
		if (versionProps.containsKey("version")) {
			return versionProps.getProperty("version");
		}

		return "";
	}

}
