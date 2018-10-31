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
package com.pogeyan.cmis.browser;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.IncludeRelationships;
import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConstraintException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisContentAlreadyExistsException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisFilterNotValidException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNameConstraintViolationException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisServiceUnavailableException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisStorageException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisStreamNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisTooManyRequestsException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUpdateConflictException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisVersioningException;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.commons.impl.json.JSONStreamAware;

import com.pogeyan.cmis.api.BaseMessage;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.messages.CmisErrorResponse;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.browser.shared.CmisRequestParameter;
import com.pogeyan.cmis.browser.shared.ControlParser;
import com.pogeyan.cmis.browser.shared.HttpUtils;
import com.pogeyan.cmis.browser.shared.POSTHttpServletRequestWrapper;
import com.pogeyan.cmis.browser.shared.QueryStringHttpServletRequestWrapper;
import com.pogeyan.cmis.impl.services.CmisObjectService;

public class ServletHelpers {

	static void writeErrorInActor(CmisErrorResponse res, HttpServletRequest request, HttpServletResponse response) {
		int statusCode = res.getErrorCode();
		String msg = res.getError();
		response.setStatus(statusCode);

		JSONObject jsonResponse = new JSONObject();
		jsonResponse.put(BrowserConstants.MESSAGE, msg);

		try {
			writeJSON(jsonResponse, request, response);
		} catch (Exception e) {
			try {
				response.sendError(statusCode, msg);
			} catch (Exception en) {
				// there is nothing else we can do
			}
		}
	}

	static void writeJSON(JSONStreamAware json, HttpServletRequest request, HttpServletResponse response)
			throws IOException {
		response.setContentType(BrowserConstants.JSON_MIME_TYPE);
		response.setCharacterEncoding(IOUtils.UTF8);
		PrintWriter pw = response.getWriter();
		String callback = HttpUtils.getStringParameter(request, BrowserConstants.PARAM_CALLBACK);
		if (callback != null) {
			if (!callback.matches("[A-Za-z0-9._\\[\\]]*")) {
				throw new CmisInvalidArgumentException("Invalid callback name!");
			}
			pw.print(callback + "(");
		}

		json.writeJSONString(pw);

		if (callback != null) {
			pw.print(");");
		}
		pw.flush();
	}

	static String createLogMessage(Exception ex, HttpServletRequest request) {
		StringBuilder sb = new StringBuilder(256);

		sb.append(ex.getMessage());

		sb.append(" (");
		sb.append(request.getMethod());
		if (request.getRequestURL() != null) {
			sb.append(' ');
			sb.append(request.getRequestURL().toString());
		}
		if (request.getQueryString() != null) {
			sb.append('?');
			sb.append(request.getQueryString());
		}
		sb.append(')');

		return sb.toString();
	}

	static int getErrorCode(CmisBaseException ex) {
		if (ex instanceof CmisConstraintException) {
			return 409;
		} else if (ex instanceof CmisContentAlreadyExistsException) {
			return 409;
		} else if (ex instanceof CmisFilterNotValidException) {
			return 400;
		} else if (ex instanceof CmisInvalidArgumentException) {
			return 400;
		} else if (ex instanceof CmisNameConstraintViolationException) {
			return 409;
		} else if (ex instanceof CmisNotSupportedException) {
			return 405;
		} else if (ex instanceof CmisObjectNotFoundException) {
			return 404;
		} else if (ex instanceof CmisPermissionDeniedException) {
			return 403;
		} else if (ex instanceof CmisStorageException) {
			return 500;
		} else if (ex instanceof CmisStreamNotSupportedException) {
			return 403;
		} else if (ex instanceof CmisUpdateConflictException) {
			return 409;
		} else if (ex instanceof CmisVersioningException) {
			return 409;
		} else if (ex instanceof CmisTooManyRequestsException) {
			return 429;
		} else if (ex instanceof CmisServiceUnavailableException) {
			return 503;
		}

		return 500;
	}

	static void printError(Exception ex, HttpServletRequest request, HttpServletResponse response) {
		int statusCode = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
		String exceptionName = CmisRuntimeException.EXCEPTION_NAME;

		if (ex instanceof CmisRuntimeException) {
			AkkaCmisBrowserBindingServlet.LOG.error(createLogMessage(ex, request), ex);
			statusCode = getErrorCode((CmisRuntimeException) ex);
		} else if (ex instanceof CmisStorageException) {
			AkkaCmisBrowserBindingServlet.LOG.error(createLogMessage(ex, request), ex);
			statusCode = getErrorCode((CmisStorageException) ex);
			exceptionName = ((CmisStorageException) ex).getExceptionName();
		} else if (ex instanceof CmisBaseException) {
			statusCode = getErrorCode((CmisBaseException) ex);
			exceptionName = ((CmisBaseException) ex).getExceptionName();
			if (statusCode == HttpServletResponse.SC_INTERNAL_SERVER_ERROR) {
				AkkaCmisBrowserBindingServlet.LOG.error(createLogMessage(ex, request), ex);
			}
		} else if (ex instanceof IOException) {
			AkkaCmisBrowserBindingServlet.LOG.warn(createLogMessage(ex, request), ex);
		} else {
			AkkaCmisBrowserBindingServlet.LOG.error(createLogMessage(ex, request), ex);
		}

		if (response.isCommitted()) {
			AkkaCmisBrowserBindingServlet.LOG
					.warn("Failed to send error message to client. Response is already committed.", ex);
			return;
		}

		String message = ex.getMessage();
		/*
		 * if (!(ex instanceof CmisBaseException)) { message = "An error occurred!"; }
		 */

		response.resetBuffer();
		response.setStatus(statusCode);

		JSONObject jsonResponse = new JSONObject();
		jsonResponse.put(BrowserConstants.ERROR_EXCEPTION, exceptionName);
		jsonResponse.put(BrowserConstants.MESSAGE, message);

		String st = ExceptionHelper.getStacktraceAsString(ex);
		if (st != null) {
			jsonResponse.put(BrowserConstants.ERROR_STACKTRACE, st);
		}

		if (ex instanceof CmisBaseException) {
			Map<String, String> additionalData = ((CmisBaseException) ex).getAdditionalData();
			if (additionalData != null && !additionalData.isEmpty()) {
				for (Map.Entry<String, String> e : additionalData.entrySet()) {
					if (BrowserConstants.ERROR_EXCEPTION.equalsIgnoreCase(e.getKey())
							|| BrowserConstants.MESSAGE.equalsIgnoreCase(e.getKey())) {
						continue;
					}
					jsonResponse.put(e.getKey(), e.getValue());
				}
			}
		}

		try {
			ServletHelpers.writeJSON(jsonResponse, request, response);
		} catch (Exception e) {
			AkkaCmisBrowserBindingServlet.LOG.error(createLogMessage(ex, request), e);
			try {
				response.sendError(statusCode, message);
			} catch (Exception en) {
				// there is nothing else we can do
			}
		}
	}

	static BaseMessage postToBaseMessage(POSTHttpServletRequestWrapper request, String[] pathFragments,
			IUserObject userObject) {
		PostRequest postRequest = new PostRequest();
		ControlParser controlParser = new ControlParser(request);
		if (controlParser != null) {
			CmisRequestParameter requestParameter = new CmisRequestParameter();
			postRequest.setPropertyData(controlParser.getProperties());
			postRequest.setAddAcl(requestParameter.createAddAcl(controlParser, postRequest));
			postRequest.setRemoveAcl(requestParameter.createRemoveAcl(controlParser, postRequest));
			postRequest.setPolicies(requestParameter.createPolicies(controlParser, postRequest));
			postRequest.setMultipart(request.isMultiPart());
			postRequest.setContentStream(requestParameter.createContentStream(request));
			postRequest.setObjectIds(requestParameter.getObjectIds(controlParser, postRequest));
			postRequest.setChangeTokens(requestParameter.getChangeTokens(controlParser, postRequest));
			postRequest.setAddSecondaryTypes(requestParameter.addSecondaryTypes(controlParser, postRequest));
			postRequest.setRemoveSecondaryTypes(requestParameter.getChangeTokens(controlParser, postRequest));
			postRequest.setPolicyId(requestParameter.getPolicyId(controlParser, postRequest));
			postRequest.setAclPropagation(requestParameter.getAclPropagation(controlParser, postRequest));
		}

		String cmisAction = HttpUtils.getStringParameter(request, BrowserConstants.CONTROL_CMISACTION);
		postRequest.setCmisAction(cmisAction);
		if (cmisAction == null || cmisAction.length() == 0) {
			throw new CmisNotSupportedException("Unknown action");
		}

		postRequest.setParameterMap(HttpUtils.getParameterMap(request));
		postRequest.setBaseUrl((String) request.getAttribute(BrowserConstants.BASE_URL_ATTRIBUTE));
		postRequest.setScheme(request.getScheme());
		postRequest.setServerName(request.getServerName());
		postRequest.setServerPort(request.getServerPort());
		postRequest.setContextPath(request.getContextPath());
		postRequest.setServletPath(request.getServletPath());
		String objectId = HttpUtils.getStringParameter(request, BrowserConstants.CONTROL_OBJECT_ID);
		postRequest.setObjectId(objectId);
		if (pathFragments.length > 0) {
			postRequest.setRepositoryId(pathFragments[0]);
			if (objectId != null) {
				if (request.getParameter("typeId") != null) {
					String typeId = request.getParameter("typeId");
					ObjectData object = ServletHelpers.getObjectDataFor(pathFragments[0], objectId, pathFragments,
							typeId, userObject);
					String objectTypeId = getStringPropertyValue(object, PropertyIds.OBJECT_TYPE_ID);
					postRequest.setTypeId(objectTypeId);
					BaseTypeId baseTypeId = BaseTypeId
							.fromValue(getStringPropertyValue(object, PropertyIds.BASE_TYPE_ID));
					postRequest.setBaseTypeId(baseTypeId);
				} else {
					ObjectData object = ServletHelpers.getObjectDataFor(pathFragments[0], objectId, pathFragments, null,
							userObject);
					String typeId = getStringPropertyValue(object, PropertyIds.OBJECT_TYPE_ID);
					postRequest.setTypeId(typeId);
					BaseTypeId baseTypeId = BaseTypeId
							.fromValue(getStringPropertyValue(object, PropertyIds.BASE_TYPE_ID));
					postRequest.setBaseTypeId(baseTypeId);
				}
			}
		}
		String token = HttpUtils.getStringParameter(request, BrowserConstants.CONTROL_TOKEN);
		postRequest.setToken(token);
		postRequest.setRequestBody(request.getRequestBody());
		postRequest.setPathFragments(pathFragments);
		if (userObject != null) {
			postRequest.setUserName(userObject.getUserDN());
			postRequest.setUserObject(userObject);
		}
		BaseMessage bm = BaseMessage.create("", cmisAction, postRequest, ServletHelpers.getHeadersInfo(request));
		return bm;
	}

	static BaseMessage queryHttpToBaseMessage(QueryStringHttpServletRequestWrapper request, String[] pathFragments,
			IUserObject userObject) {
		QueryGetRequest queryRequest = new QueryGetRequest();
		queryRequest.setParameterMap(HttpUtils.getParameterMap(request));
		queryRequest.setBaseUrl((String) request.getAttribute(BrowserConstants.BASE_URL_ATTRIBUTE));
		queryRequest.setScheme(request.getScheme());
		queryRequest.setServerName(request.getServerName());
		queryRequest.setServerPort(request.getServerPort());
		queryRequest.setContextPath(request.getContextPath());
		queryRequest.setServletPath(request.getServletPath());

		queryRequest.setRange(request.getHeader("Range"));
		String objectId = HttpUtils.getStringParameter(request, BrowserConstants.PARAM_OBJECT_ID);
		queryRequest.setObjectId(objectId);

		if (userObject != null) {
			queryRequest.setUserName(userObject.getUserDN());
			queryRequest.setUserObject(userObject);
		}
		if (pathFragments.length > 0) {
			queryRequest.setRepositoryId(pathFragments[0]);
		}

		// selector is the actionName
		String selector = HttpUtils.getStringParameter(request, BrowserConstants.PARAM_SELECTOR);
		if (pathFragments.length > 1) {
			queryRequest.setPathFragments(pathFragments);
			// nested url
			String repositoryId = pathFragments[0];
			try {
				ObjectData object = null;
				// Checking objectId format for RestAPI.
				// Example objectId format-typeID::primaryKey::primaryKeyValue
				if (objectId != null && objectId.contains("::")) {
					String[] inputs = objectId.split("::");
					// getObjectForRestAPI method is used to get the object.
					object = CmisObjectService.Impl.getObjectForRestAPI(repositoryId, inputs[0], inputs[1], inputs[2],
							"cmis:objectId,cmis:objectTypeId,cmis:baseTypeId", false, IncludeRelationships.NONE,
							"cmis:none", false, false, null, null);
				} else {
					object = ServletHelpers.getObjectDataFor(repositoryId, objectId, pathFragments, null, userObject);
				}

				// reset object id again here
				objectId = object.getId();
				queryRequest.setObjectId(objectId);
				String typeId = getStringPropertyValue(object, PropertyIds.OBJECT_TYPE_ID);
				queryRequest.setTypeId(typeId);
				BaseTypeId baseTypeId = BaseTypeId.fromValue(getStringPropertyValue(object, PropertyIds.BASE_TYPE_ID));
				queryRequest.setBaseTypeId(baseTypeId);

				if (selector == null) {
					switch (baseTypeId) {
					case CMIS_DOCUMENT:
						selector = BrowserConstants.SELECTOR_CONTENT;
						break;
					case CMIS_FOLDER:
						selector = BrowserConstants.SELECTOR_CHILDREN;
						break;
					default:
						selector = BrowserConstants.SELECTOR_OBJECT;
						break;
					}
				}
			} catch (Exception e) {
				selector = BrowserConstants.SELECTOR_OBJECT;
			}
		}

		selector = selector == null ? "GetRepositories".toLowerCase() : selector;
		BaseMessage bm = BaseMessage.create("", selector, queryRequest, ServletHelpers.getHeadersInfo(request));
		return bm;
	}

	static ObjectData getObjectDataFor(String repositoryId, String objectId, String[] pathFragments, String typeId,
			IUserObject userObject) {
		ObjectData object = null;
		// objectId will be null if path needs to be considered
		if (objectId != null) {
			object = CmisObjectService.Impl.getObject(repositoryId, objectId,
					"cmis:objectId,cmis:objectTypeId,cmis:baseTypeId", false, IncludeRelationships.NONE, "cmis:none",
					false, false, null, userObject, BaseTypeId.CMIS_FOLDER, typeId);
		} else if (pathFragments != null) {
			object = CmisObjectService.Impl.getObjectByPath(repositoryId, getPath(pathFragments),
					"cmis:objectId,cmis:objectTypeId,cmis:baseTypeId", false, IncludeRelationships.NONE, "cmis:none",
					false, false, null, userObject, typeId);
		} else {
			return null;
		}

		return object;
	}

	/**
	 * Extracts a property from an object.
	 */
	static String getStringPropertyValue(ObjectData object, String name) {
		if (object == null) {
			return null;
		}

		Properties propData = object.getProperties();
		if (propData == null) {
			return null;
		}

		Map<String, PropertyData<?>> properties = propData.getProperties();
		if (properties == null) {
			return null;
		}

		PropertyData<?> property = properties.get(name);
		if (property == null) {
			return null;
		}

		Object value = property.getFirstValue();
		if (!(value instanceof String)) {
			return null;
		}

		return (String) value;
	}

	/**
	 * Builds the object path.
	 */
	static String getPath(String[] pathFragments) {
		if (pathFragments.length < 2) {
			throw new CmisRuntimeException("Internal error!");
		}
		if (pathFragments.length == 2) {
			return "/";
		}

		StringBuilder sb = new StringBuilder(128);
		for (int i = 2; i < pathFragments.length; i++) {
			if (pathFragments[i].length() == 0) {
				continue;
			}

			sb.append('/');
			sb.append(pathFragments[i]);
		}

		return sb.toString();
	}

	static Map<String, String> getHeadersInfo(HttpServletRequest request) {
		Map<String, String> map = new HashMap<String, String>();
		Enumeration<String> headerNames = request.getHeaderNames();
		while (headerNames.hasMoreElements()) {
			String key = (String) headerNames.nextElement();
			String value = request.getHeader(key);
			map.put(key, value);
		}

		return map;
	}
}
