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

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import akka.actor.Actor;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;

/**
 * It gets the ActorRef by implementing {@link IActorProducer}
 */
public class InMemoryActorProducer implements IActorProducer {

	final ConcurrentMap<String, ActorRef> actors = new ConcurrentHashMap<String, ActorRef>();

	/**
	 * Instantiates a new InMemoryActorProducer.
	 */
	public InMemoryActorProducer() {
	}

	/**
	 * It gets ActorRef by passing {@link Actor}
	 * 
	 * @see com.kobil.ldap.ssms.IActorProducer#getActorFrom(akka.actor.ActorSystem,
	 *      java.lang.String)
	 */
	@Override
	public ActorRef getActorFrom(ActorSystem actorSystem, String actorName) {
		Class<?> cls = null;
		try {
			cls = Class.forName(actorName);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
		if (cls != null) {
			if (!actors.containsKey(actorName)) {
				ActorRef actor = actorSystem.actorOf(Props.create(cls), actorName);
				this.actors.put(actorName, actor);
			}

			return this.actors.get(actorName);
		}

		return null;
	}

	@Override
	public void clear() {
		// use this to clear the cache and re-generate actors
		// useful when restarting services
		this.actors.clear();
	}

	final static IActorProducer instance = new InMemoryActorProducer();
	public static IActorProducer getInstance() {
		return instance;
	}
}
