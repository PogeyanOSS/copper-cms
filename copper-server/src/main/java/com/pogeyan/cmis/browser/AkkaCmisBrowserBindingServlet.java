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
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

import javax.servlet.AsyncContext;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUnauthorizedException;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;
import org.apache.chemistry.opencmis.commons.impl.MimeHelper;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.Action;
import com.pogeyan.cmis.api.BaseMessage;
import com.pogeyan.cmis.api.GenericActor;
import com.pogeyan.cmis.api.MessageType;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.CmisErrorResponse;
import com.pogeyan.cmis.api.messages.LoginRequest;
import com.pogeyan.cmis.api.messages.LoginResponse;
import com.pogeyan.cmis.api.messages.PostFileResponse;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.utils.*;
import com.pogeyan.cmis.browser.shared.HttpUtils;
import com.pogeyan.cmis.browser.shared.POSTHttpServletRequestWrapper;
import com.pogeyan.cmis.browser.shared.QueryStringHttpServletRequestWrapper;

import akka.actor.ActorRef;
import akka.actor.ActorSelection;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.actor.UntypedActor;

@WebServlet(urlPatterns = "/*", asyncSupported = true)
public class AkkaCmisBrowserBindingServlet extends HttpServlet {
	static final Logger LOG = LoggerFactory.getLogger(AkkaCmisBrowserBindingServlet.class);

	public static final String JSON_MIME_TYPE = "application/json";
	public static final String HTML_MIME_TYPE = "text/html";
	public static final String METHOD_GET = "GET";
	public static final String METHOD_HEAD = "HEAD";
	public static final String METHOD_POST = "POST";
	public static final String METHOD_PUT = "PUT";
	public static final String METHOD_DELETE = "DELETE";
	public static final String ERROR_EXCEPTION = "exception";
	public static final String Message = "message";
	public static final String ERROR_STACKTRACE = "stacktrace";
	public static final String Ok = "Ok";

	private static final long serialVersionUID = 1L;
	private CsrfManager csrfManager;

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		// set up CSRF manager
		csrfManager = new CsrfManager(config);
	}

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			final ActorSystem system = (ActorSystem) request.getServletContext().getAttribute("ActorSystem");
			// CSRF token check
			String method = request.getMethod();
			if (!METHOD_GET.equals(method) && !METHOD_HEAD.equals(method)) {
				checkCsrfToken(request, response, false, false);
			}
			// set default headers
			response.addHeader("Cache-Control", "private, max-age=0");
			response.addHeader("Server", ServerVersion.OPENCMIS_SERVER);

			// split path
			String[] pathFragments = HttpUtils.splitPath(request);

			final AsyncContext ctx = request.startAsync(request, response);
			if (Helpers.isPerfMode()) {
				MetricsInputs.get().getCounter("counter_requests_total").inc();
			}

			if (pathFragments != null && pathFragments.length > 0 && StringUtils.isBlank(pathFragments[0])) {
				BaseMessage bm = gettingBaseMessage(method, pathFragments, null, request, response);
				if (bm != null) {
					// create actor on-the-fly
					ActorRef servletActor = system.actorOf(Props.create(ServletActor.class, ctx));
					servletActor.tell(bm, ActorRef.noSender());
				} else {
					throw new CmisNotSupportedException("Unsupported method");
				}
			} else {
				this.verifyLogin(request, pathFragments, system, (s) -> {
					try {
						IUserObject loginSession = (IUserObject) s;
						BaseMessage bm = gettingBaseMessage(method, pathFragments, loginSession, request, response);

						if (bm != null) {
							// create actor on-the-fly
							ActorRef servletActor = system.actorOf(Props.create(ServletActor.class, ctx));
							servletActor.tell(bm, ActorRef.noSender());
						} else {
							throw new CmisNotSupportedException("Unsupported method");
						}
					} catch (Exception e1) {
						MetricsInputs.markBindingServletErrorMeter();
						LOG.error("Service execution exception: {}, stack: {}", e1.getMessage(),
								ExceptionUtils.getStackTrace(e1));
						ServletHelpers.printError(e1, request, response);
					}
				}, (err) -> {
					HttpServletResponse asyncResponse = (HttpServletResponse) ctx.getResponse();
					asyncResponse.setHeader("WWW-Authenticate", "Basic realm=\"CMIS\", charset=\"UTF-8\"");
					try {
						asyncResponse.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Authorization Required");
					} catch (Exception e1) {
						MetricsInputs.markBindingServletErrorMeter();
						ServletHelpers.printError(e1, (HttpServletRequest) ctx.getRequest(), asyncResponse);
					}

					ctx.complete();
				});
			}
		} catch (Exception e) {
			MetricsInputs.markBindingServletErrorMeter();
			if (e instanceof CmisUnauthorizedException) {
				response.setHeader("WWW-Authenticate", "Basic realm=\"CMIS\", charset=\"UTF-8\"");
				response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Authorization Required");
			} else if (e instanceof CmisPermissionDeniedException) {
				response.setHeader("WWW-Authenticate", "Basic realm=\"CMIS\", charset=\"UTF-8\"");
				response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Authorization Required");
			} else {
				ServletHelpers.printError(e, request, response);
			}
		} finally {
			// in any case close the content stream if one has been provided
			// if (request instanceof POSTHttpServletRequestWrapper) {
			// InputStream stream = ((POSTHttpServletRequestWrapper)
			// request).getStream();
			// if (stream != null) {
			// try {
			// stream.close();
			// } catch (IOException e) {
			// LOG.error("Could not close POST stream: {}", e.toString(), e);
			// }
			// }
			// }

			// // we are done.
			// try {
			// response.flushBuffer();
			// } catch (IOException ioe) {
			// LOG.error("Could not flush resposne: {}", ioe.toString(), ioe);
			// }
		}
	}

	private void verifyLogin(HttpServletRequest request, String[] pathFragments, ActorSystem system,
			Action<Object> onSuccess, Action<Object> onError) {
		// forwarding callback after verifying login response object
		Action<BaseMessage> onLoginSuccess = (t) -> {
			LoginResponse lr = (LoginResponse) t.getMessageAsType(LoginResponse.class);
			if (lr.isSuccessfulLogin()) {
				// To test different users and different password on test
				// cases require to remove cache on 1 second
				/*
				 * if (Helpers.isTestMode()) { //RedissonCacheFactory.get().put("login." +
				 * userName, lr.getLoginDetails(), 5, TimeUnit.SECONDS); } else { // set in map
				 * cache for 30 mins expiry //RedissonCacheFactory.get().put("login." +
				 * userName, lr.getLoginDetails(), 30, TimeUnit.MINUTES); }
				 */
				onSuccess.apply(lr.getLoginDetails());
			} else {
				onError.apply(null);
			}
		};

		ActorRef genericActorRef = system.actorOf(Props.create(GenericActor.class, onLoginSuccess, onError));
		LoginRequest loginRequest = new LoginRequest();
		loginRequest.setHeaders(ServletHelpers.getHeadersInfo(request));
		if (pathFragments.length > 0) {
			loginRequest.setRepositoryId(pathFragments[0]);
		}
		BaseMessage loginMessage = BaseMessage.create("login", "authenticate", loginRequest);
		genericActorRef.tell(loginMessage, ActorRef.noSender());

		/*
		 * String userName = callContextMap.get(BrowserConstants.USERNAME); Object
		 * loginSession = RedissonCacheFactory.get().get("login." + userName); if
		 * (loginSession == "") { } else { onSuccess.apply(loginSession); }
		 */
	}

	private BaseMessage gettingBaseMessage(String method, String[] pathFragments, IUserObject loginSession,
			HttpServletRequest request, HttpServletResponse response) {
		BaseMessage bm = null;
		try {
			// check HTTP method
			if (METHOD_GET.equals(method)) {
				QueryStringHttpServletRequestWrapper qRequest = new QueryStringHttpServletRequestWrapper(request);
				bm = ServletHelpers.queryHttpToBaseMessage((QueryStringHttpServletRequestWrapper) qRequest,
						pathFragments, loginSession);
			} else if (METHOD_POST.equals(method)) {
				POSTHttpServletRequestWrapper pRequest = new POSTHttpServletRequestWrapper(request);
				bm = ServletHelpers.postToBaseMessage((POSTHttpServletRequestWrapper) pRequest, pathFragments,
						loginSession);
			} else {
				throw new CmisNotSupportedException("Unsupported method");
			}

		} catch (Exception e1) {
			LOG.error("Service execution exception: {}, stack: {}", e1.getMessage(), ExceptionUtils.getStackTrace(e1));
			ServletHelpers.printError(e1, request, response);
		}
		return bm;
	}

	/**
	 * Checks the CSRF if configured. Throws an
	 * {@link CmisPermissionDeniedException} if something is wrong.
	 */
	protected void checkCsrfToken(HttpServletRequest req, HttpServletResponse resp, boolean isRepositoryInfoRequest,
			boolean isContentRequest) {
		csrfManager.check(req, resp, isRepositoryInfoRequest, isContentRequest);
	}

	static class ServletActor extends UntypedActor {
		private final AsyncContext asyncContext;
		private ActorSelection gatewayActor = null;

		public ServletActor(AsyncContext asyncContext) {
			this.gatewayActor = this.getContext().actorSelection("/user/gateway");
			this.asyncContext = asyncContext;
		}

		@Override
		public void onReceive(Object message) throws Throwable {
			if (message instanceof BaseMessage) {
				BaseMessage bm = (BaseMessage) message;
				if (bm.getMessageType() == MessageType.REQUEST) {
					this.gatewayActor.tell(bm, this.getSelf());
				} else {
					HttpServletRequest request = (HttpServletRequest) this.asyncContext.getRequest();
					HttpServletResponse response = (HttpServletResponse) this.asyncContext.getResponse();
					if (bm.getMessageType() == MessageType.RESPONSE) {
						// check for post respose message
						if (bm.getMessageBodyType() == PostFileResponse.class) {
							PostFileResponse fileResponse = bm.getMessageAsType(PostFileResponse.class);
							if (fileResponse == null) {
								CmisBaseResponse errorMessage = CmisBaseResponse
										.setCmisResponse("File response found NULL", 500);
								ServletHelpers.writeErrorInActor((CmisErrorResponse) errorMessage.getCmisData(),
										request, response);

								return;
							} else if (fileResponse.getContent() != null) {
								this.handleFileContentStream(request, response, fileResponse);
							}

						} else {
							response.setStatus(HttpServletResponse.SC_OK);
							response.setContentType(JSON_MIME_TYPE);
							response.setCharacterEncoding("UTF-8");
							response.getWriter().write(bm.getMessagePlain());
						}
					} else {
						CmisErrorResponse res = (CmisErrorResponse) bm.getMessageAsType(CmisErrorResponse.class);
						ServletHelpers.writeErrorInActor(res, request, response);
					}
					this.asyncContext.complete();
					// stop actor
					this.getContext().stop(this.getSelf());
				}
			} else {
				HttpServletResponse response = (HttpServletResponse) this.asyncContext.getResponse();
				// write error
				response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
				// TODO: Write proper error message with error code
				response.getWriter().println("{}");
				this.asyncContext.complete();
				// stop actor
				this.getContext().stop(this.getSelf());
			}
		}

		private void handleFileContentStream(HttpServletRequest request, HttpServletResponse response,
				PostFileResponse fileResponse) throws UnsupportedEncodingException {
			ContentStream content = fileResponse.getContent();
			// BigInteger offset = fileResponse.getOffset();
			// BigInteger length = content.getBigLength();
			boolean download = fileResponse.isDownload();
			// String contentType = content.getMimeType();
			// if (contentType == null) {

			// }
			// set headers
			// if ((offset == null || offset != null && offset.signum() == 0) &&
			// length == null) {
			response.setStatus(HttpServletResponse.SC_OK);
			/*
			 * } else { response.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT); if
			 * (content.getBigLength() != null && content.getBigLength().signum() == 1) {
			 * BigInteger firstBytePos = (offset == null ? BigInteger.ZERO : offset);
			 * BigInteger lastBytePos =
			 * firstBytePos.add(content.getBigLength().subtract(BigInteger.ONE)) ;
			 * 
			 * response.setHeader("Content-Range", "bytes " + firstBytePos.toString() + "-"
			 * + lastBytePos.toString() + "/*"); } }
			 */
			// String contentType = QueryGetRequest.MEDIATYPE_OCTETSTREAM;
			response.setContentType(fileResponse.getContent().getMimeType());
			/*
			 * long length = content.getLength(); if (length <= Integer.MAX_VALUE) {
			 * response.setContentLength((int) length); } else {
			 * response.addHeader("Content-Length", Long.toString(length)); }
			 */

			String contentFilename = content.getFileName();
			if (contentFilename == null) {
				contentFilename = "content";
			}

			byte[] fileNameBytes = contentFilename.getBytes("utf-8");
			String dispositionFileName = "";
			for (byte b : fileNameBytes)
				dispositionFileName += (char) (b & 0xff);

			if (download) {
				response.setHeader(MimeHelper.CONTENT_DISPOSITION,
						MimeHelper.encodeContentDisposition(MimeHelper.DISPOSITION_ATTACHMENT, dispositionFileName));
			} else {
				response.setHeader(MimeHelper.CONTENT_DISPOSITION,
						MimeHelper.encodeContentDisposition(MimeHelper.DISPOSITION_INLINE, dispositionFileName));
			}

			// send content
			InputStream in = content.getStream();
			try {
				if (!(response.containsHeader("Content-Range"))) {
					response.setStatus(HttpServletResponse.SC_OK);
				} else {
					String range = request.getHeader("Range");
					String[] parts = range.split("=");
					if (parts.length > 1 && parts[1] != null) {
						String[] ranges = parts[1].split("-");
						String rangeStart = ranges[0];
						String rangeEnd = ranges[1];
						response.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
						response.setHeader("Accept-Ranges", "bytes");
						response.setIntHeader("Content-Length",
								Integer.parseInt(rangeEnd) + 1 - Integer.parseInt(rangeStart));
						response.setHeader("Content-Range",
								"bytes " + rangeStart + "-" + rangeEnd + "/" + content.getLength());
					} else {
						LOG.error("Content range parts should not be null");
						throw new CmisInvalidArgumentException("Content range parts should not be null.");
					}
				}
				OutputStream out = response.getOutputStream();
				IOUtils.copy(in, out, QueryGetRequest.BUFFER_SIZE);
				IOUtils.closeQuietly(out);
			} catch (IOException e) {
				CmisBaseResponse errorMessage = CmisBaseResponse.setCmisResponse(e.getMessage(), 500);
				ServletHelpers.writeErrorInActor((CmisErrorResponse) errorMessage.getCmisData(), request, response);
			} finally {
				IOUtils.closeQuietly(in);
				IOUtils.closeQuietly(content);
			}
		}
	}
}
