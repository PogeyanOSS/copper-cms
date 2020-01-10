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
package com.pogeyan.cmis.browser.shared;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.commons.lang3.exception.ExceptionUtils;

import javax.servlet.http.HttpServletRequest;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;

import org.apache.chemistry.opencmis.commons.impl.UrlBuilder;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class HttpUtils {
	public static final String REPOSITORY_PLACEHOLDER = "{repositoryId}";
	public static final String ROOT_PATH_FRAGMENT = "root";
	static final Logger LOG = LoggerFactory.getLogger(HttpUtils.class);

	private static final ObjectMapper mapper = new ObjectMapper();

	private HttpUtils() {
	}

	/**
	 * Extracts a string parameter.
	 */
	public static String getStringParameter(final HttpServletRequest request, final String name) {
		assert request != null;

		if (name == null) {
			return null;
		}

		Map<String, String[]> parameters = request.getParameterMap();
		if (parameters != null && !parameters.isEmpty()) {
			if (parameters.containsKey(name)) {
				return parameters.get(name)[0];
			}
		} else if (request instanceof POSTHttpServletRequestWrapper) {
			try {
				POSTHttpServletRequestWrapper requestWrapper = (POSTHttpServletRequestWrapper) request;
				String requestBody = requestWrapper.getRequestBody();
				Map<String, Object> map = new HashMap<String, Object>();
				// convert JSON string to Map
				map = mapper.readValue(requestBody, new TypeReference<Map<String, String>>() {
				});
				if (map.containsKey(name)) {
					return map.get(name).toString();
				}
			} catch (Exception ex) {
				LOG.error("getStringParameter exception: {}, stack: {}", ex.getMessage(),
						ExceptionUtils.getStackTrace(ex));
			}
		}

		return null;
	}

	public static Map<String, String> getParameterMap(final HttpServletRequest request) {
		Map<String, String> requestParameterMap = new HashMap<String, String>();
		Map<String, String[]> parameters = request.getParameterMap();
		if (parameters != null && !parameters.isEmpty()) {
			for (Map.Entry<String, String[]> parameter : parameters.entrySet()) {
				if (parameter.getValue() == null
						|| (parameter.getValue() != null && parameter.getValue().length == 0)) {
					continue;
				}

				requestParameterMap.put(parameter.getKey(), parameter.getValue()[0]);
			}
		} else if (request instanceof POSTHttpServletRequestWrapper) {
			try {
				POSTHttpServletRequestWrapper requestWrapper = (POSTHttpServletRequestWrapper) request;
				String requestBody = requestWrapper.getRequestBody();
				// convert JSON string to Map
				requestParameterMap = mapper.readValue(requestBody, new TypeReference<Map<String, String>>() {
				});
			} catch (Exception ex) {
				LOG.error("getParameterMap exception: {}, stack: {}", ex.getMessage(),
						ExceptionUtils.getStackTrace(ex));
			}
		}

		return requestParameterMap;
	}

	/**
	 * Splits the path into its fragments.
	 */
	public static String[] splitPath(final HttpServletRequest request) {
		assert request != null;

		int prefixLength = request.getContextPath().length() + request.getServletPath().length();
		String p = request.getRequestURI().substring(prefixLength);

		if (p.length() == 0) {
			return new String[0];
		}

		String[] result = p.substring(1).split("/");
		for (int i = 0; i < result.length; i++) {
			result[i] = IOUtils.decodeURL(result[i]);

			// check for malicious characters
			for (int j = 0; j < result[i].length(); j++) {
				char c = result[i].charAt(j);
				if (c == '\n' || c == '\r' || c == '\b' || c == 0) {
					throw new CmisInvalidArgumentException("Invalid path!");
				}
			}
		}

		return result;
	}

	private static UrlBuilder compileBaseUrl(String baseUrl, String scheme, String serverName, int serverPort,
			String contextPath, String servletPath, String repositoryId) {
		if (baseUrl != null) {
			int repIdPos = baseUrl.indexOf(REPOSITORY_PLACEHOLDER);
			if (repIdPos < 0) {
				return new UrlBuilder(baseUrl);
			} else {
				return new UrlBuilder(baseUrl.substring(0, repIdPos) + repositoryId
						+ baseUrl.substring(repIdPos + REPOSITORY_PLACEHOLDER.length()));
			}
		}
		UrlBuilder url = new UrlBuilder(scheme, serverName, serverPort, null);
		url.addPath(contextPath);
		url.addPath(servletPath);
		url.addPathSegment(repositoryId);
		return url;
	}

	public static UrlBuilder compileRepositoryUrl(String baseUrl, String scheme, String serverName, int serverPort,
			String contextPath, String servletPath, String repositoryId) {
		return compileBaseUrl(baseUrl, scheme, serverName, serverPort, contextPath, servletPath, repositoryId);
	}

	public static UrlBuilder compileRootUrl(String baseUrl, String scheme, String serverName, int serverPort,
			String contextPath, String servletPath, String repositoryId) {
		return compileRepositoryUrl(baseUrl, scheme, serverName, serverPort, contextPath, servletPath, repositoryId)
				.addPathSegment(ROOT_PATH_FRAGMENT);
	}
}
