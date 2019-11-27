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

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.codahale.metrics.Meter;
import com.codahale.metrics.Metric;
import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.Timer;
import com.pogeyan.cmis.api.BaseMessage;
import com.pogeyan.cmis.api.BaseResponse;
import com.pogeyan.cmis.api.MessageType;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
import com.pogeyan.cmis.browser.ActorServiceFactory;

import akka.actor.ActorRef;
import akka.actor.Terminated;
import akka.actor.UntypedActor;

public class GatewayActor extends UntypedActor {
	/** The Constant LOG. */
	private static final Logger LOG = LoggerFactory.getLogger(GatewayActor.class);

	/** The message refs. */
	private final Map<String, ActorRef> messageRefs = new HashMap<String, ActorRef>();
	private final Map<String, ActorRef> actorRefs = new HashMap<String, ActorRef>();
	private final Map<String, Timer.Context> perfTimerContext = new HashMap<String, Timer.Context>();
	private Meter requestMeter = null;
	private Meter responseMeter = null;
	private Meter errorMeter = null;
	private Meter baseResponseErrorMeter = null;

	public GatewayActor() {

		if (Helpers.isPerfMode()) {
			this.requestMeter = MetricsInputs.get().getMarker("RequestMeter");
			this.responseMeter = MetricsInputs.get().getMarker("ResponseMeter");
			this.errorMeter = MetricsInputs.get().getMarker("ErrorMeter");
			this.baseResponseErrorMeter = MetricsInputs.get().getMarker("BaseResponseErrorMeter");
			MetricsInputs.get().getMetrics().register(MetricRegistry.name(GatewayActor.class, "GateWay Actor-Gauges"),
					new Gauge<Integer>() {
						@Override
						public Integer getValue() {
							return messageRefs.size();
						}
					});
		}
	}

	@Override
	public void onReceive(Object message) throws Exception {
		if (message instanceof BaseMessage) {
			BaseMessage bm = (BaseMessage) message;
			if (bm.getTypeName().equals("entry")) {
				// removed the oldLogic
			} else if (bm.getMessageType() == MessageType.REQUEST) {
				if (!this.messageRefs.containsKey(bm.getMessageId())) {
					this.messageRefs.put(bm.getMessageId(), this.getSender());
					// get actor either with type name or action name
					ActorRef actorReference = ActorServiceFactory.getInstance().initActorRef(bm.getTypeName(),
							bm.getActionName());
					actorRefs.put(bm.getMessageId(), actorReference);
					if (actorReference != null) {
						// get type name from actor reference
						String typeName = this.getTypeName(actorReference);
						bm.setTypeName(typeName);
						actorReference.tell(bm, this.getSelf());
						if (Helpers.isPerfMode()) {
							this.requestMeter.mark();
							Timer.Context timerContext = MetricsInputs.get().getTimer("TimerID_" + bm.getMessageId())
									.time();
							perfTimerContext.put(bm.getMessageId(), timerContext);
						}
					} else {
						LOG.error("Actor not found for {}", bm.getTypeName());
						BaseResponse errorResponse = BaseResponse.error("Actor not available for: " + bm.getMessageId()
								+ ", using this actor: " + bm.getTypeName(), 500);
						BaseMessage respMessage = BaseMessage.create("ERROR", "ERROR", errorResponse);
						respMessage.setMessageType(MessageType.ERROR);
						this.getSender().tell(respMessage, null);
						if (Helpers.isPerfMode()) {
							this.errorMeter.mark();
						}
					}
				} else {
					BaseResponse errorResponse = BaseResponse
							.error("Message id already present --> " + bm.getMessageId(), 404);
					LOG.error("Message id already present: {}", bm.getMessageId());
					BaseMessage respMessage = BaseMessage.create("ERROR", "ERROR", errorResponse);
					respMessage.setMessageType(MessageType.ERROR);
					this.getSender().tell(respMessage, null);
					if (Helpers.isPerfMode()) {
						this.errorMeter.mark();
						this.baseResponseErrorMeter.mark();
					}
				}
			} else {
				if (this.messageRefs.containsKey((bm.getMessageId()))) {
					if (LOG.isDebugEnabled()) {
						LOG.debug("Reply back to servlet actor for message: {}", bm);
					}
					ActorRef responseActorRef = this.messageRefs.get(bm.getMessageId());
					ActorServiceFactory.getInstance().stopActor(actorRefs.get(bm.getMessageId()));
					this.messageRefs.remove(bm.getMessageId());
					responseActorRef.tell(bm, ActorRef.noSender());
					if (Helpers.isPerfMode()) {
						this.responseMeter.mark();
						Timer.Context timerContext = this.perfTimerContext.get(bm.getMessageId());
						if (timerContext != null) {
							timerContext.stop();
							this.perfTimerContext.remove(bm.getMessageId());
						}
					}
				} else {
					LOG.error("Unknown message in messageRefs : {}", bm.getMessageId());
					unhandled(bm);
				}
			}
		} else if (message instanceof Terminated) {
			Terminated terminated = (Terminated) message;
			String[] s = terminated.getActor().path().name().split(Pattern.quote("."));
			String terminatedActor = s[s.length - 1];
			LOG.info("Actor terminated: {}", terminatedActor);
		} else {
			LOG.error("Unknown message received: {}", message != null ? message.toString() : "");
			unhandled(message);
		}

	}

	private String getTypeName(ActorRef actorRef) {
		String[] s = actorRef.path().name().split(Pattern.quote("."));
		String typeName = s[s.length - 1];
		return typeName;
	}

	public interface Gauge<T> extends Metric {
		T getValue();
	}
}
