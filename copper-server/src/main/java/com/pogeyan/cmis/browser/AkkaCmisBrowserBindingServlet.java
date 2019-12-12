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
import java.io.InvalidObjectException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.Enumeration;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;
import javax.servlet.AsyncContext;
import javax.servlet.AsyncEvent;
import javax.servlet.AsyncListener;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.exceptions.CmisNotSupportedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisPermissionDeniedException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUnauthorizedException;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;
import org.apache.chemistry.opencmis.commons.impl.MimeHelper;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.BaseMessage;
import com.pogeyan.cmis.api.MessageType;
import com.pogeyan.cmis.api.auth.IAuthService;
import com.pogeyan.cmis.api.auth.IUserObject;
import com.pogeyan.cmis.api.auth.LoginRequestObject;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.messages.CmisErrorResponse;
import com.pogeyan.cmis.api.messages.LoginRequest;
import com.pogeyan.cmis.api.messages.PostFileResponse;
import com.pogeyan.cmis.api.messages.QueryGetRequest;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
import com.pogeyan.cmis.browser.shared.HttpUtils;
import com.pogeyan.cmis.browser.shared.POSTHttpServletRequestWrapper;
import com.pogeyan.cmis.browser.shared.QueryStringHttpServletRequestWrapper;
import com.pogeyan.cmis.impl.factory.LoginAuthServiceFactory;

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

			Enumeration headerNames = request.getHeaderNames();
			while (headerNames.hasMoreElements()) {
				String headerName = (String) headerNames.nextElement();
				System.out.println("" + headerName + "::" + request.getHeader(headerName));
			}

			// split path
			String[] pathFragments = HttpUtils.splitPath(request);

			if (Helpers.isPerfMode()) {
				MetricsInputs.get().getCounter("counter_requests_total").inc();
			}

			if (pathFragments != null && pathFragments.length > 0 && StringUtils.isBlank(pathFragments[0])) {
				BaseMessage bm = gettingBaseMessage(method, pathFragments, null, request, response);
				if (bm != null) {
					// create actor on-the-fly
					final AsyncContext ctx = request.startAsync(request, response);
					ActorRef servletActor = system.actorOf(Props.create(ServletActor.class, ctx));
					servletActor.tell(bm, ActorRef.noSender());
				} else {
					throw new CmisNotSupportedException("Unsupported method");
				}
			} else {
				try {
					IUserObject loginSession = this.verifyLogin(request, pathFragments);
					if(loginSession == null) {
						throw new CmisUnauthorizedException();
					}
					BaseMessage bm = gettingBaseMessage(method, pathFragments, loginSession, request, response);
					if (bm != null) {
						// create actor on-the-fly
						final AsyncContext ctx = request.startAsync(request, response);
						ActorRef servletActor = system.actorOf(Props.create(ServletActor.class, ctx));
						servletActor.tell(bm, ActorRef.noSender());
					} else {
						throw new CmisNotSupportedException("Unsupported method");
					}
				} catch (Exception e1) {
					MetricsInputs.markBindingServletErrorMeter();
					LOG.error("Service execution exception: {}, stack: {}", e1.getMessage(),
							ExceptionUtils.getStackTrace(e1));
					response.setHeader("WWW-Authenticate", "Basic realm=\"CMIS\", charset=\"UTF-8\"");
					try {
						response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Authorization Required");
					} catch (Exception e2) {
						MetricsInputs.markBindingServletErrorMeter();
						ServletHelpers.printError(e1, (HttpServletRequest) request, response);
					}
				}
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

	private IUserObject verifyLogin(HttpServletRequest request, String[] pathFragments)
			throws InvalidTargetObjectTypeException, InvalidObjectException {
		LoginRequest loginRequest = new LoginRequest();
		loginRequest.setHeaders(ServletHelpers.getHeadersInfo(request));
		if (pathFragments.length > 0) {
			loginRequest.setRepositoryId(pathFragments[0]);
		}
		Map<String, String> loginSettings = RepositoryManagerFactory.getLoginDetails(loginRequest.getRepositoryId());
		if (LOG.isDebugEnabled()) {
			LOG.debug("Login settings for repositoryId: {}", loginSettings.toString());
		}
		IAuthService authService = LoginAuthServiceFactory.createAuthService(loginSettings);
		if (authService != null) {
			LoginRequestObject loginObject = new LoginRequestObject(loginRequest.getHeaders().get("authorization"),
					loginRequest.getRepositoryId());
			IUserObject result = authService.authenticate(loginObject);
			return result;
		} else {
			LOG.error("Login authenticate service not found for: {}", loginSettings.toString());
			throw new InvalidTargetObjectTypeException(
					"Login authenticate service not found for: " + loginSettings.toString());
		}
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
			long timeout = 120000;
			this.asyncContext.setTimeout(timeout);
			this.asyncContext.addListener(new AsyncListener() {
				@Override
				public void onComplete(AsyncEvent event) throws IOException {
					stopActor();
				}

				@Override
				public void onTimeout(AsyncEvent event) throws IOException {
					try {
						asyncContext.complete();
						stopActor();
					} catch (Exception e) {

					}
				}

				@Override
				public void onError(AsyncEvent event) throws IOException {
					try {
						asyncContext.complete();
						stopActor();
					} catch (Exception e) {
					}
				}

				@Override
				public void onStartAsync(AsyncEvent event) throws IOException {
				}
			});

		}

		@Override
		public void onReceive(Object message) throws Throwable {
			try {
				onProcess(message);
			} catch (Exception e) {
				LOG.error("Servlet Actor onReceive execution exception: {}, stack: {}", e.getMessage(),
						ExceptionUtils.getStackTrace(e));
			}
		}

		protected void stopActor() {
			this.getContext().stop(this.getSelf());
		}

		private void onProcess(Object message) {
			try {
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
					}
				} else {
					HttpServletResponse response = (HttpServletResponse) this.asyncContext.getResponse();
					response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
					response.getWriter().println("{}");
					try {
						this.asyncContext.complete();
					} catch (IllegalStateException ex) {
						LOG.trace("Already resumed!", ex);
					}
				}
			} catch (Exception e) {
				LOG.error("Servlet Actor onProcess method exception: {}, stack: {}", e.getMessage(),
						ExceptionUtils.getStackTrace(e));
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
				String range = request.getHeader("Range");
				if (range != null) {
					String[] ranges = range.split("=");
					String[] parts = ranges[1].split("-");
					if (parts.length > 1 && parts[1] != null) {
						String rangeStart = parts[0];
						String rangeEnd = parts[1];
						if (Integer.parseInt(rangeStart) < content.getLength()
								&& Integer.parseInt(rangeEnd) < content.getLength()) {
							response.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
							response.setHeader("Accept-Ranges", "bytes");
							response.setIntHeader("Content-Length",
									Integer.parseInt(rangeEnd) + 1 - Integer.parseInt(rangeStart));
							response.setHeader("Content-Range",
									"bytes " + rangeStart + "-" + rangeEnd + "/" + content.getLength());
							long start = Long.parseLong(rangeStart);
							in.skip(start);
						}
						// else {
						// response.setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
						// }
					} else {
						response.setStatus(HttpServletResponse.SC_OK);
					}
				} else {
					response.setStatus(HttpServletResponse.SC_OK);
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
