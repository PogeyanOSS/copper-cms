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

import akka.actor.ActorSelection;
import akka.actor.UntypedActor;

public class GenericActor extends UntypedActor {
	private Action<BaseMessage> onSuccess = null;
	private Action<BaseMessage> onError = null;
	private ActorSelection gatewayActor = null;

	public GenericActor(Action<BaseMessage> onSuccess, Action<BaseMessage> onError) {
		this.onSuccess = onSuccess;
		this.onError = onError;
		this.gatewayActor = this.getContext().actorSelection("/user/gateway");
	}

	@Override
	public void onReceive(Object message) throws Throwable {
		if (message instanceof BaseMessage) {
			BaseMessage bm = (BaseMessage) message;
			if (bm.getMessageType() == MessageType.REQUEST) {
				this.gatewayActor.tell(bm, this.getSelf());
			} else {
				// response
				if (this.onSuccess != null) {
					this.onSuccess.apply(bm);
				}
				
				// stop actor
				this.getContext().stop(this.getSelf());
			}
		} else {
			if (this.onError != null) {
				BaseMessage errMsg = BaseMessage.empty();
				errMsg.setMessageType(MessageType.ERROR);
				this.onError.apply(errMsg);
			}
			
			// stop actor
			this.getContext().stop(this.getSelf());
		}
	}
}