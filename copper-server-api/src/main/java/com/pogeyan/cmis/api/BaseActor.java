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
package com.pogeyan.cmis.api;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiFunction;

import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParseException;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParser;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.codahale.metrics.Timer;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
import com.pogeyan.cmis.api.utils.TracingMessage;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

import akka.actor.ActorRef;
import akka.actor.UntypedActor;

/**
 * 
 * Defines the core functionality of the actor receive/send pattern. The
 * messages are handled asynchronously using Java 8 CompletableFuture<T>.
 *
 * @param <T>
 *            the generic type
 * @param <R>
 *            the generic type
 */
abstract class BaseActor<T, R extends BaseResponse> extends UntypedActor {

	/** The Constant logger. */
	protected static final Logger logger = LoggerFactory.getLogger(BaseActor.class);
	private Map<String, ActorHandleContext> messageHandles = new HashMap<String, ActorHandleContext>();
	private Map<String, Timer.Context> perfTimerContext = new HashMap<String, Timer.Context>();
	private Map<String, ISpan> traceContext = new HashMap<String, ISpan>();
	private static final String TRACINGID = "TracingId";
	private static final String REQUEST_HEADERS = "RequestHeaders";
	private static final String PARENT_SPAN = "ParentSpan";
	public static final String BASE_MESSAGE = "ActionName: %s, TypeName: %s, RequestData: %s, TraceId: %s";
	public static final String REPOID = "repositoryId";
	public static final String USEROBJECT = "userObject";
	public static final String TRACING = "tracing";

	public abstract String getName();

	/**
	 * Process the message of Actor by calling #
	 * {@link #doProcess(ActorRef, BaseMessage)}
	 * 
	 * @see akka.actor.UntypedActor#onReceive(java.lang.Object)
	 */
	@Override
	public void onReceive(Object message) {
		ActorRef sender = this.getSender();
		if (message instanceof BaseMessage) {
			BaseMessage b = (BaseMessage) message;
			this.doProcess(sender, b);
		}
	}

	protected String[] getMethodSelectors() {
		return this.messageHandles.keySet().stream().map(t -> t).toArray(s -> new String[s]);
	}

	/**
	 * Processing the message from sender.
	 * 
	 * @param sender
	 *            The sender Actorref
	 * @param b
	 *            the base message
	 */
	private void doProcess(final ActorRef sender, BaseMessage b) {

		if (logger.isTraceEnabled()) {
			logger.trace("Processing the message from sender: {}, messageId: {}", sender, b.getMessageId());
		}

		if (this.messageHandles.containsKey(b.getActionName())) {
			if (logger.isDebugEnabled()) {
				logger.debug("Message handle identified for sender:{} with messageId: {}, actionName: {}", sender,
						b.getMessageId(), b.getActionName());
			}

			final ActorHandleContext ctx = this.messageHandles.get(b.getActionName());
			T tIn = b.getMessageAsType(ctx.decoderType);
			if (tIn != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("Message decoded for sender:{} with messageId: {}", sender, b.getMessageId());
				}
				if (Helpers.isPerfMode()) {
					HashMap<String, String> headers = b.getBaggage(REQUEST_HEADERS);
					ISpan parentSpan = TracingApiServiceFactory.getApiService().startSpan(b.getMessageId(), null,
							"BaseActor::" + b.getTypeName() + "::" + b.getActionName(), headers);
					b.addBaggage(TRACINGID, b.getMessageId());
					b.addBaggage(PARENT_SPAN, parentSpan);
					this.traceContext.put(b.getMessageId(), parentSpan);
					if (headers.get(TRACING) != null && Boolean.valueOf(headers.get(TRACING))) {
						try {
							JSONObject jobj = (JSONObject) new JSONParser().parse(b.getMessagePlain());
							jobj.remove(USEROBJECT);
							TracingApiServiceFactory.getApiService().updateSpan(parentSpan,
									TracingMessage.message(
											((String.format(BASE_MESSAGE, b.getActionName(), b.getTypeName(), jobj,
													parentSpan.getTraceId()))),
											this.getClass().getSimpleName(), (String) jobj.get(REPOID), false));
						} catch (JSONParseException e) {
							logger.error("Exception in parsing message : {}, {}, {}", b.getTypeName(),
									b.getActionName(), e.getMessage());
						}

					}
				}

				CompletableFuture<R> f_response = ctx.fn.apply(tIn, b.getBaggage());
				if (Helpers.isPerfMode()) {
					Timer.Context timerContext = MetricsInputs.get()
							.getTimer("TimerClass_" + b.getTypeName() + "_" + b.getActionName()).time();
					perfTimerContext.put(b.getMessageId(), timerContext);
				}

				f_response.thenAcceptAsync((resp) -> {
					BaseMessage respMessage = b.clone(MessageType.RESPONSE);
					if (resp == null || resp != null && !StringUtils.isBlank(resp.getError())) {
						if (logger.isDebugEnabled()) {
							logger.debug("no response found for message: {}, replying with MessageType.ERROR",
									b.getMessageId());
						}
						respMessage.setMessageInternal(((CmisBaseResponse) resp).getCmisData());
						respMessage.setMessageType(MessageType.ERROR);

					} else {
						if (resp instanceof CmisBaseResponse) {
							if (logger.isDebugEnabled()) {
								logger.debug("Message has CmisBaseResponse for messageId: {}", b.getMessageId());
							}
							respMessage.setMessageInternal(((CmisBaseResponse) resp).getCmisData());

						} else {
							respMessage.setMessageInternal(resp);

						}
						if (logger.isDebugEnabled()) {
							logger.debug("Response for messageId: {}, message: {} ", respMessage.getMessageId(),
									respMessage.getMessagePlain());
						}
					}
					// inform the original sender of the reply
					sender.tell(respMessage, this.getSelf());
					if (Helpers.isPerfMode()) {
						Timer.Context timerContextStop = this.perfTimerContext.get(b.getMessageId());
						if (timerContextStop != null) {
							timerContextStop.stop();
							this.perfTimerContext.remove(b.getMessageId());
						}
						TracingApiServiceFactory.getApiService().endSpan(b.getMessageId(),
								this.traceContext.get(b.getMessageId()), false);
						this.traceContext.remove(b.getMessageId());
					}

				}).exceptionally((err) -> {
					logger.error("Exception in processing function : {}, {}, {}", b.getTypeName(), b.getActionName(),
							err.getMessage());
					BaseMessage respMessage = b.clone(MessageType.ERROR);
					respMessage.setMessageId(b.getMessageId());
					sender.tell(respMessage, this.getSelf());
					return null;
				});
			} else {
				reportError(sender, b);
			}
		} else {
			reportError(sender, b);
		}
	}

	private void reportError(final ActorRef sender, BaseMessage b) {
		if (logger.isTraceEnabled()) {
			logger.trace("reportError for the message from sender: {}, messageId: {}", sender, b.getMessageId());
		}

		BaseMessage errMsg = BaseMessage.empty();
		errMsg.setMessageType(MessageType.ERROR);
		errMsg.setMessageId(b.getMessageId());
		sender.tell(errMsg, null);
	}

	/**
	 * Register a message handle for incoming messages.
	 *
	 * @param messageHandle
	 *            - Action name to which the message requests to
	 * @param decoderClassType
	 *            the decoder class type
	 * @param fn
	 *            - Functor handle
	 */
	protected void registerMessageHandle(String messageHandle, Class<?> decoderClassType,
			BiFunction<T, HashMap<String, Object>, CompletableFuture<R>> fn) {
		if (logger.isDebugEnabled()) {
			logger.debug(
					"Registering a message handle for incoming messages from messageHandle:{}, decoderClassType:{}",
					messageHandle, decoderClassType);
		}

		if (!this.messageHandles.containsKey(messageHandle)) {
			ActorHandleContext ctx = new ActorHandleContext();
			ctx.decoderType = decoderClassType;
			ctx.fn = fn;
			this.messageHandles.put(messageHandle, ctx);
		}
	}

	/**
	 * 
	 * Declaring decoderType and BiFunction.
	 *
	 */
	class ActorHandleContext {
		Class<?> decoderType;
		BiFunction<T, HashMap<String, Object>, CompletableFuture<R>> fn;
	}
}
