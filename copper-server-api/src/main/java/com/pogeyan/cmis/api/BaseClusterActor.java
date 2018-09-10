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

import com.pogeyan.cmis.api.messages.MemberUpRequest;

import akka.cluster.ClusterEvent.MemberUp;
import akka.cluster.Member;

public abstract class BaseClusterActor<T, R extends BaseResponse> extends BaseActor<T, R> {

	// subscribe to cluster changes, MemberUp
	@Override
	public void preStart() {
		/*
		 * if (!Helpers.isInDevMode()) { Cluster cluster =
		 * Cluster.get(getContext().system()); cluster.subscribe(getSelf(),
		 * MemberUp.class); } else {
		 */
		// temp registeration in dev mode
		this.register(null);
		// }
	}

	// re-subscribe when restart
	@Override
	public void postStop() {
		/*
		 * if (!Helpers.isInDevMode()) { Cluster cluster =
		 * Cluster.get(getContext().system()); cluster.unsubscribe(getSelf()); }
		 */
	}

	@Override
	public void onReceive(Object message) {
		if (message instanceof MemberUp) {
			MemberUp mUp = (MemberUp) message;
			register(mUp.member());
		} else {
			super.onReceive(message);
		}
	}

	void register(Member member) {
		MemberUpRequest mUp = new MemberUpRequest();
		mUp.setSelectors(this.getMethodSelectors());
		BaseMessage bm = BaseMessage.create("entry", "entry", mUp);
		/*
		 * if (!Helpers.isInDevMode() && member.hasRole("gateway")) {
		 * getContext().actorSelection(member.address() +
		 * "/user/gateway").tell(bm, getSelf()); } else {
		 */
		getContext().actorSelection("/user/gateway").tell(bm, getSelf());
		/// }
	}
}
