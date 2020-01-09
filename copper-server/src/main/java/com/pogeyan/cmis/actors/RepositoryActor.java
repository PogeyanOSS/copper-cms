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
package com.pogeyan.cmis.actors;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import org.apache.chemistry.opencmis.commons.BasicPermissions;
import org.apache.chemistry.opencmis.commons.data.PermissionMapping;
import org.apache.chemistry.opencmis.commons.data.RepositoryInfo;
import org.apache.chemistry.opencmis.commons.definitions.PermissionDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionContainer;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionList;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.enums.CapabilityAcl;
import org.apache.chemistry.opencmis.commons.enums.CapabilityChanges;
import org.apache.chemistry.opencmis.commons.enums.CapabilityContentStreamUpdates;
import org.apache.chemistry.opencmis.commons.enums.CapabilityJoin;
import org.apache.chemistry.opencmis.commons.enums.CapabilityOrderBy;
import org.apache.chemistry.opencmis.commons.enums.CapabilityQuery;
import org.apache.chemistry.opencmis.commons.enums.CapabilityRenditions;
import org.apache.chemistry.opencmis.commons.enums.CmisVersion;
import org.apache.chemistry.opencmis.commons.enums.DateTimeFormat;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.SupportedPermissions;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AclCapabilitiesDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.CreatablePropertyTypesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.NewTypeSettableAttributesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PermissionDefinitionDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PermissionMappingDataImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RepositoryCapabilitiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RepositoryInfoImpl;
import org.apache.chemistry.opencmis.commons.impl.json.JSONArray;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParseException;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParser;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.MongoException;
import com.pogeyan.cmis.api.BaseClusterActor;
import com.pogeyan.cmis.api.BaseRequest;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.repo.IRepository;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.api.uri.exception.CmisRoleValidationException;
import com.pogeyan.cmis.api.utils.ErrorMessages;
import com.pogeyan.cmis.api.utils.Globals;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.TracingErrorMessage;
import com.pogeyan.cmis.api.utils.TracingWriter;
import com.pogeyan.cmis.browser.BrowserConstants;
import com.pogeyan.cmis.browser.shared.HttpUtils;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.impl.services.CmisObjectService;
import com.pogeyan.cmis.impl.services.CmisTypeServices;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

public class RepositoryActor extends BaseClusterActor<BaseRequest, BaseResponse> {
	private static final Logger LOG = LoggerFactory.getLogger(RepositoryActor.class);
	private static final String OPENCMIS_SERVER = "Cloud CMIS DB";
	private static final String PROTO_HEADER = "x-forwarded-proto";
	private static final String HOST_HEADER = "x-forwarded-host";
	private static final String PORT_HEADER = "x-forwarded-port";
	private static final String FOR_HEADER = "x-forwarded-for";

	public RepositoryActor() {
		this.registerMessageHandle("GetRepositories".toLowerCase(), QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getRepositories((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("repositoryInfo", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getRepositoryInfo((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("typeDefinition", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getTypeDefinition((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("typeChildren", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getTypeChildren((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("typeDescendants", QueryGetRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse.fromWithTryCatch(
						() -> this.getTypeDescendants((QueryGetRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("createType", PostRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.createType((PostRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("deleteType", PostRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.deleteType((PostRequest) t, (HashMap<String, Object>) b))));

		this.registerMessageHandle("updateType", PostRequest.class,
				(t, b) -> CompletableFuture.supplyAsync(() -> CmisBaseResponse
						.fromWithTryCatch(() -> this.updateType((PostRequest) t, (HashMap<String, Object>) b))));

	}

	public String getName() {
		return "repository";
	}

	private JSONObject getRepositoryInfo(QueryGetRequest t, HashMap<String, Object> baggage)
			throws CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::getRepositoryInfo", null);
		String permission = t.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, t.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, t.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, t.getUserName()), span));

		}
		// call DB and get the repositoryInfo
		String rootId = CmisObjectService.Impl.addRootFolder(t.getRepositoryId(), t.getUserName(), t.getTypeId(),
				tracingId, span);
		IRepository repository = RepositoryManagerFactory.getInstance().getRepositoryStore()
				.getRepository(t.getRepositoryId());
		RepositoryInfo repo = createRepositoryInfo(t.getRepositoryId(),
				repository.getRepositoryName() == null ? "" : repository.getRepositoryName(), CmisVersion.CMIS_1_1,
				rootId, repository.getDescription() == null ? "" : repository.getDescription());
		String repositoryUrl = HttpUtils.compileRepositoryUrl(t.getBaseUrl(), t.getScheme(), t.getServerName(),
				t.getServerPort(), t.getContextPath(), t.getServletPath(), repo.getId()).toString();
		String rootUrl = HttpUtils.compileRootUrl(t.getBaseUrl(), t.getScheme(), t.getServerName(), t.getServerPort(),
				t.getContextPath(), t.getServletPath(), repo.getId()).toString();
		LOG.error("Server name : {} and ContextPath : {} and BaseUrl : {} and  ServletPath : {} and Scheme : {}", t.getServerName(),
				t.getContextPath(), t.getBaseUrl(), t.getServletPath(), t.getScheme());
		JSONObject result = new JSONObject();
		LOG.error("RepositoryUrl : {} ", repositoryUrl);
		result.put(repo.getId(), JSONConverter.convert(repo, repositoryUrl, rootUrl, true));
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return result;

	}

	private JSONObject getRepositories(QueryGetRequest request, HashMap<String, Object> baggage)
			throws MongoException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::getRepositories", null);
		List<RepositoryInfo> infoDataList = new ArrayList<RepositoryInfo>() {
			private static final long serialVersionUID = 1L;
			{
				List<IRepository> respositoryList = RepositoryManagerFactory.getInstance().getRepositoryStore()
						.getRepositories(request.getRepositoryId());
				if (respositoryList != null && !respositoryList.isEmpty()) {
					for (IRepository repository : respositoryList) {
						CmisTypeServices.Impl.addBaseType(repository.getRepositoryId(), request.getUserObject(),
								tracingId, span);
						String rootId = CmisObjectService.Impl.addRootFolder(repository.getRepositoryId(),
								request.getUserName(), request.getTypeId(), tracingId, span);
						add(createRepositoryInfo(repository.getRepositoryId(), repository.getRepositoryName(),
								CmisVersion.CMIS_1_1, rootId,
								repository.getDescription() == null ? "" : repository.getDescription()));
					}
				}
			}
		};

		JSONObject result = new JSONObject();
		Map<String, String> headers = request.getHeaders();
		if(System.getenv("Reverse_Proxy").equals("true")) {
		logger.info("Reverse_Proxy Enabled : {} ");
		String scheme = headers.containsKey(PROTO_HEADER) && headers.get(PROTO_HEADER) != null
				? headers.get(PROTO_HEADER)
				: request.getScheme();
		String serverName = headers.containsKey(HOST_HEADER) && headers.get(HOST_HEADER) != null
				? headers.get(HOST_HEADER)
				: headers.containsKey(FOR_HEADER) && headers.get(FOR_HEADER) != null ? headers.get(FOR_HEADER)
						: request.getServerName();
		int port = headers.containsKey(PORT_HEADER) && headers.get(PORT_HEADER) != null
				? Integer.parseInt(headers.get(PORT_HEADER))
				: request.getServerPort();
		
		for (RepositoryInfo ri : infoDataList) {
			String repositoryUrl = HttpUtils.compileRepositoryUrl(request.getBaseUrl(), scheme, serverName, port,
					request.getContextPath(), request.getServletPath(), ri.getId()).toString();
			String rootUrl = HttpUtils.compileRootUrl(request.getBaseUrl(), scheme, serverName, port,
					request.getContextPath(), request.getServletPath(), ri.getId()).toString();
			result.put(ri.getId(), JSONConverter.convert(ri, repositoryUrl, rootUrl, true));
		}
		}else {
			for (RepositoryInfo ri : infoDataList) {
				String repositoryUrl = HttpUtils.compileRepositoryUrl(request.getBaseUrl(), request.getScheme(), request.getServerName(), request.getServerPort(),
						request.getContextPath(), request.getServletPath(), ri.getId()).toString();
				String rootUrl = HttpUtils.compileRootUrl(request.getBaseUrl(), request.getScheme(), request.getServerName(), request.getServerPort(),
						request.getContextPath(), request.getServletPath(), ri.getId()).toString();
				
				result.put(ri.getId(), JSONConverter.convert(ri, repositoryUrl, rootUrl, true));
			}
		}
		return result;

	}

	private JSONObject getTypeDefinition(QueryGetRequest request, HashMap<String, Object> baggage)
			throws CmisRuntimeException, IllegalArgumentException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::getTypeDefinition", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));

		}
		String typeId = request.getParameter(QueryGetRequest.PARAM_TYPE_ID);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		LOG.info("Method name: {}, getting type definition  using this typeId: {}, repositoryId: {}",
				"getTypeDefinition", typeId, request.getRepositoryId());
		TypeDefinition type = CmisTypeServices.Impl.getTypeDefinition(request.getRepositoryId(), typeId, null,
				request.getUserObject(), tracingId, span);
		JSONObject resultType = JSONConverter.convert(type, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return resultType;

	}

	private JSONObject getTypeChildren(QueryGetRequest request, HashMap<String, Object> baggage)
			throws IllegalArgumentException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::getTypeChildren", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
		}
		String typeId = request.getParameter(QueryGetRequest.PARAM_TYPE_ID);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		boolean includePropertyDefinitions = request.getBooleanParameter(QueryGetRequest.PARAM_PROPERTY_DEFINITIONS,
				false);
		BigInteger maxItems = request.getBigIntegerParameter(QueryGetRequest.PARAM_MAX_ITEMS);
		BigInteger skipCount = request.getBigIntegerParameter(QueryGetRequest.PARAM_SKIP_COUNT);
		LOG.info(
				"Method name: {}, getting type children  using this typeId: {}, repositoryId: {}, maxItems: {}, skipCount: {}, includePropertyDefinitions: {}",
				"getTypeChildren", typeId, request.getRepositoryId(), maxItems, skipCount, includePropertyDefinitions);
		TypeDefinitionList typeList = CmisTypeServices.Impl.getTypeChildren(request.getRepositoryId(), typeId,
				includePropertyDefinitions, maxItems, skipCount, null, request.getUserObject(), tracingId, span);
		JSONObject jsonTypeList = JSONConverter.convert(typeList, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonTypeList;

	}

	private JSONArray getTypeDescendants(QueryGetRequest request, HashMap<String, Object> baggage)
			throws IllegalArgumentException, CmisInvalidArgumentException, CmisRuntimeException,
			CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::getTypeDescendants", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "get")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
		}
		String typeId = request.getParameter(QueryGetRequest.PARAM_TYPE_ID);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();
		BigInteger depth = request.getBigIntegerParameter(QueryGetRequest.PARAM_DEPTH);
		boolean includePropertyDefinitions = request.getBooleanParameter(QueryGetRequest.PARAM_PROPERTY_DEFINITIONS,
				false);
		LOG.info(
				"Method name: {}, getting type descendants using this typeId: {}, repositoryId: {}, depth: {}, includePropertyDefinitions: {}",
				"getTypeDescendants", typeId, request.getRepositoryId(), depth, includePropertyDefinitions);
		List<TypeDefinitionContainer> typeTree = CmisTypeServices.Impl.getTypeDescendants(request.getRepositoryId(),
				typeId, depth, includePropertyDefinitions, null, request.getUserObject(), tracingId, span);

		if (typeTree == null) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter.log(String.format(ErrorMessages.TYPE_TREE_NULL), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(TracingWriter.log(String.format(ErrorMessages.TYPE_TREE_NULL), span));
		}

		JSONArray jsonTypeTree = new JSONArray();
		for (TypeDefinitionContainer container : typeTree) {
			jsonTypeTree.add(JSONConverter.convert(container, dateTimeFormat));
		}
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonTypeTree;

	}

	private JSONObject createType(PostRequest request, HashMap<String, Object> baggage) throws IllegalArgumentException,
			CmisInvalidArgumentException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::createType", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
		}
		String typeStr = request.getParameter(QueryGetRequest.CONTROL_TYPE);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();

		if (typeStr == null) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(
									String.format(ErrorMessages.TYPE_DEFINITION_MISSING, request.getUserName()), span),
							ErrorMessages.INVALID_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisInvalidArgumentException(TracingWriter
					.log(String.format(ErrorMessages.TYPE_DEFINITION_MISSING, request.getUserName()), span));
		}

		// convert type definition
		JSONParser parser = new JSONParser();
		Object typeJson = null;
		try {
			typeJson = parser.parse(typeStr);
		} catch (JSONParseException e) {
			LOG.error("JSON Parser error: {}" + ExceptionUtils.getStackTrace(e) + "TraceId: " + span != null
					? span.getTraceId()
					: null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter
							.log(String.format(ErrorMessages.JSON_ERROR, ExceptionUtils.getStackTrace(e)), span),
							ErrorMessages.BASE_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
		}
		if (!(typeJson instanceof Map)) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(
									String.format(ErrorMessages.INVALID_TYPE_DEFINITION, request.getUserName()), span),
							ErrorMessages.INVALID_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisInvalidArgumentException(TracingWriter
					.log(String.format(ErrorMessages.INVALID_TYPE_DEFINITION, request.getUserName()), span));
		}

		@SuppressWarnings("unchecked")
		TypeDefinition typeIn = JSONConverter.convertTypeDefinition((Map<String, Object>) typeJson);
		LOG.info("Method name: {}, creating the new type for this typeId: {}, repositoryId: {}", "createType",
				typeIn.getId(), request.getRepositoryId());
		TypeDefinition typeOut = CmisTypeServices.Impl.createType(request.getRepositoryId(), typeIn, null,
				request.getUserObject(), tracingId, span);
		JSONObject jsonType = JSONConverter.convert(typeOut, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonType;

	}

	private JSONObject updateType(PostRequest request, HashMap<String, Object> baggage)
			throws CmisInvalidArgumentException, IllegalArgumentException, CmisRuntimeException,
			CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::updateType", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
		}
		String typeStr = request.getParameter(QueryGetRequest.CONTROL_TYPE);
		DateTimeFormat dateTimeFormat = request.getDateTimeFormatParameter();

		if (typeStr == null) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(
									String.format(ErrorMessages.TYPE_DEFINITION_MISSING, request.getUserName()), span),
							ErrorMessages.INVALID_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisInvalidArgumentException(TracingWriter
					.log(String.format(ErrorMessages.TYPE_DEFINITION_MISSING, request.getUserName()), span));
		}

		// convert type definition
		JSONParser parser = new JSONParser();
		Object typeJson = null;
		try {
			typeJson = parser.parse(typeStr);
		} catch (JSONParseException e) {
			LOG.error("JSON parse exception: {}" + ExceptionUtils.getStackTrace(e) + "TraceId: " + span != null
					? span.getTraceId()
					: null);
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(TracingWriter
							.log(String.format(ErrorMessages.JSON_ERROR, ExceptionUtils.getStackTrace(e)), span),
							ErrorMessages.BASE_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
		}
		if (!(typeJson instanceof Map)) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(
									String.format(ErrorMessages.INVALID_TYPE_DEFINITION, request.getUserName()), span),
							ErrorMessages.INVALID_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisInvalidArgumentException(TracingWriter
					.log(String.format(ErrorMessages.INVALID_TYPE_DEFINITION, request.getUserName()), span));
		}

		@SuppressWarnings("unchecked")
		TypeDefinition typeIn = JSONConverter.convertTypeDefinition((Map<String, Object>) typeJson);
		LOG.info("Method name: {}, update type using this typeId: {}, repositoryId: {}", "updateType", typeIn.getId(),
				request.getRepositoryId());
		TypeDefinition typeOut = CmisTypeServices.Impl.updateType(request.getRepositoryId(), typeIn, null,
				request.getUserObject(), tracingId, span);
		JSONObject jsonType = JSONConverter.convert(typeOut, dateTimeFormat);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return jsonType;

	}

	private JSONObject deleteType(PostRequest request, HashMap<String, Object> baggage)
			throws IllegalArgumentException, CmisRuntimeException, CmisRoleValidationException {
		String tracingId = (String) baggage.get(BrowserConstants.TRACINGID);
		ISpan parentSpan = (ISpan) baggage.get(BrowserConstants.PARENT_SPAN);
		ISpan span = TracingApiServiceFactory.getApiService().startSpan(tracingId, parentSpan,
				"RepositoryActor::deleteType", null);
		String permission = request.getUserObject().getPermission();
		if (!Helpers.checkingUserPremission(permission, "post")) {
			TracingApiServiceFactory.getApiService().updateSpan(span,
					TracingErrorMessage.message(
							TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span),
							ErrorMessages.RUNTIME_EXCEPTION, request.getRepositoryId(), true));
			TracingApiServiceFactory.getApiService().endSpan(tracingId, span, true);
			throw new CmisRuntimeException(
					TracingWriter.log(String.format(ErrorMessages.NOT_AUTHORISED, request.getUserName()), span));
		}
		String typeId = request.getParameter(QueryGetRequest.CONTROL_TYPE_ID);
		LOG.info("Method name: {}, delete the type using this typeId: {}, repositoryId: {}", "deleteType", typeId,
				request.getRepositoryId());
		CmisTypeServices.Impl.deleteType(request.getRepositoryId(), typeId, null, request.getUserObject(), tracingId,
				span);
		TracingApiServiceFactory.getApiService().endSpan(tracingId, span, false);
		return null;

	}

	@SuppressWarnings("serial")
	private RepositoryInfo createRepositoryInfo(String repositoryId, String name, CmisVersion cmisVersion,
			String rootFolderId, String description) {
		LOG.info("createRepositoryInfo rootFolderId: {}", rootFolderId);
		MBaseObjectDAO baseMorphiaDAO = DatabaseServiceFactory.getInstance(repositoryId).getObjectService(repositoryId,
				MBaseObjectDAO.class);
		String latestToken = String.valueOf(baseMorphiaDAO.getLatestToken().getChangeToken() != null
				? baseMorphiaDAO.getLatestToken().getChangeToken().getTime()
				: null);
		// repository info
		RepositoryInfoImpl repoInfo = new RepositoryInfoImpl();
		repoInfo.setId(repositoryId == null ? "repository" : repositoryId);
		repoInfo.setName(name);
		repoInfo.setDescription(description);
		repoInfo.setRootFolder(rootFolderId.toString());
		repoInfo.setPrincipalAnonymous("");
		repoInfo.setPrincipalAnyone("");
		repoInfo.setThinClientUri("");
		repoInfo.setChangesIncomplete(Boolean.FALSE);
		repoInfo.setLatestChangeLogToken(latestToken != null ? latestToken : "token-24");
		repoInfo.setVendorName("Pogeyan Technologies Pvt. Ltd.");
		repoInfo.setProductName(OPENCMIS_SERVER);
		repoInfo.setProductVersion(Globals.getAppVersion());

		// set capabilities
		RepositoryCapabilitiesImpl caps = new RepositoryCapabilitiesImpl();
		caps.setAllVersionsSearchable(true);
		caps.setCapabilityAcl(CapabilityAcl.MANAGE);
		caps.setCapabilityChanges(CapabilityChanges.OBJECTIDSONLY);
		caps.setCapabilityContentStreamUpdates(CapabilityContentStreamUpdates.ANYTIME);
		caps.setCapabilityJoin(CapabilityJoin.NONE);
		caps.setCapabilityQuery(CapabilityQuery.NONE);
		caps.setCapabilityRendition(CapabilityRenditions.READ);
		caps.setIsPwcSearchable(true);
		caps.setIsPwcUpdatable(true);
		caps.setSupportsGetDescendants(true);
		caps.setSupportsGetFolderTree(true);
		caps.setSupportsMultifiling(true);
		caps.setSupportsUnfiling(true);
		caps.setSupportsVersionSpecificFiling(true);

		AclCapabilitiesDataImpl aclCaps = new AclCapabilitiesDataImpl();
		aclCaps.setAclPropagation(AclPropagation.OBJECTONLY);
		aclCaps.setSupportedPermissions(SupportedPermissions.BOTH);

		// permissions
		List<PermissionDefinition> permissions = new ArrayList<PermissionDefinition>();
		permissions.add(createPermission(BasicPermissions.READ, "Read"));
		permissions.add(createPermission(BasicPermissions.WRITE, "Write"));
		permissions.add(createPermission(BasicPermissions.ALL, "All"));
		if (cmisVersion == CmisVersion.CMIS_1_1) {
			NewTypeSettableAttributesImpl typeAttrs = new NewTypeSettableAttributesImpl();
			typeAttrs.setCanSetControllableAcl(true);
			typeAttrs.setCanSetControllablePolicy(true);
			typeAttrs.setCanSetCreatable(true);
			typeAttrs.setCanSetDescription(true);
			typeAttrs.setCanSetDisplayName(true);
			typeAttrs.setCanSetFileable(true);
			typeAttrs.setCanSetFulltextIndexed(true);
			typeAttrs.setCanSetId(true);
			typeAttrs.setCanSetIncludedInSupertypeQuery(false);
			typeAttrs.setCanSetLocalName(true);
			typeAttrs.setCanSetLocalNamespace(true);
			typeAttrs.setCanSetQueryable(true);
			typeAttrs.setCanSetQueryName(true);
			caps.setNewTypeSettableAttributes(typeAttrs);
		}
		aclCaps.setPermissionDefinitionData(permissions);

		// mapping
		List<PermissionMapping> list = new ArrayList<PermissionMapping>();
		list.add(createMapping(PermissionMapping.CAN_GET_DESCENDENTS_FOLDER, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_GET_CHILDREN_FOLDER, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_GET_PARENTS_FOLDER, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_GET_FOLDER_PARENT_OBJECT, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_CREATE_DOCUMENT_FOLDER, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_CREATE_FOLDER_FOLDER, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_CREATE_RELATIONSHIP_SOURCE, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_CREATE_RELATIONSHIP_TARGET, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_GET_PROPERTIES_OBJECT, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_VIEW_CONTENT_OBJECT, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_UPDATE_PROPERTIES_OBJECT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_MOVE_OBJECT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_MOVE_TARGET, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_MOVE_SOURCE, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_DELETE_OBJECT, BasicPermissions.WRITE));

		list.add(createMapping(PermissionMapping.CAN_DELETE_TREE_FOLDER, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_SET_CONTENT_DOCUMENT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_DELETE_CONTENT_DOCUMENT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_ADD_TO_FOLDER_OBJECT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_REMOVE_FROM_FOLDER_OBJECT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_CHECKOUT_DOCUMENT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_CANCEL_CHECKOUT_DOCUMENT, BasicPermissions.WRITE));

		list.add(createMapping(PermissionMapping.CAN_CHECKIN_DOCUMENT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_GET_ALL_VERSIONS_VERSION_SERIES, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_GET_OBJECT_RELATIONSHIPS_OBJECT, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_ADD_POLICY_OBJECT, BasicPermissions.WRITE));
		list.add(createMapping(PermissionMapping.CAN_REMOVE_POLICY_OBJECT, BasicPermissions.WRITE));

		list.add(createMapping(PermissionMapping.CAN_GET_APPLIED_POLICIES_OBJECT, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_GET_ACL_OBJECT, BasicPermissions.READ));
		list.add(createMapping(PermissionMapping.CAN_APPLY_ACL_OBJECT, BasicPermissions.ALL));

		Map<String, PermissionMapping> map = new LinkedHashMap<String, PermissionMapping>();
		for (PermissionMapping pm : list) {
			map.put(pm.getKey(), pm);
		}

		List<BaseTypeId> changesOnType;
		// CMIS 1.1 extensions
		if (cmisVersion == CmisVersion.CMIS_1_1) {
			repoInfo.setCmisVersionSupported(CmisVersion.CMIS_1_1.value());
			repoInfo.setCmisVersion(CmisVersion.CMIS_1_1);
			changesOnType = new ArrayList<BaseTypeId>() {
				{
					add(BaseTypeId.CMIS_DOCUMENT);
					add(BaseTypeId.CMIS_FOLDER);
					add(BaseTypeId.CMIS_ITEM);
				}
			};

			Set<PropertyType> propertyTypeSet = new HashSet<PropertyType>() {
				{
					add(PropertyType.BOOLEAN);
					add(PropertyType.DATETIME);
					add(PropertyType.DECIMAL);
					add(PropertyType.HTML);
					add(PropertyType.ID);
					add(PropertyType.INTEGER);
					add(PropertyType.STRING);
					add(PropertyType.URI);
				}
			};
			CreatablePropertyTypesImpl creatablePropertyTypes = new CreatablePropertyTypesImpl();
			creatablePropertyTypes.setCanCreate(propertyTypeSet);
			caps.setCreatablePropertyTypes(creatablePropertyTypes);
			caps.setCapabilityOrderBy(CapabilityOrderBy.COMMON);
		} else {
			repoInfo.setCmisVersionSupported(CmisVersion.CMIS_1_0.value());
			repoInfo.setCmisVersion(CmisVersion.CMIS_1_0);
			changesOnType = new ArrayList<BaseTypeId>() {
				{
					add(BaseTypeId.CMIS_DOCUMENT);
					add(BaseTypeId.CMIS_FOLDER);
				}
			};
		}

		repoInfo.setChangesOnType(changesOnType);
		aclCaps.setPermissionMappingData(map);
		repoInfo.setAclCapabilities(aclCaps);
		repoInfo.setCapabilities(caps);
		return repoInfo;
	}

	private static PermissionDefinition createPermission(String permission, String description) {
		PermissionDefinitionDataImpl pd = new PermissionDefinitionDataImpl();
		pd.setId(permission);
		pd.setDescription(description);
		return pd;
	}

	private static PermissionMapping createMapping(String key, String permission) {
		PermissionMappingDataImpl pm = new PermissionMappingDataImpl();
		pm.setKey(key);
		pm.setPermissions(Collections.singletonList(permission));
		return pm;
	}
}
