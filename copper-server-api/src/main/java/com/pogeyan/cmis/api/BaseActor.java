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

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.codahale.metrics.Timer;
import com.pogeyan.cmis.api.data.ISpan;
import com.pogeyan.cmis.api.messages.CmisBaseResponse;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
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
	private ISpan parentSpan;

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

				parentSpan = TracingApiServiceFactory.getApiService().startSpan(null,
						"BaseActor_" + b.getTypeName() + "_" + b.getActionName(), b.getTracingHeaders());
				b.addBaggage("ParentSpan", parentSpan);

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
						TracingApiServiceFactory.getApiService().endSpan(parentSpan);

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
