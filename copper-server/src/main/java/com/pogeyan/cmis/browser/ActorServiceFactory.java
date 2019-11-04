/**
 * Copyright 2017 Pogeyan Technologies
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.pogeyan.cmis.browser;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ObjectArrays;
import com.pogeyan.cmis.api.IActorService;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;

public class ActorServiceFactory {

	/** The constant logger */
	private static final Logger LOG = LoggerFactory.getLogger(ActorServiceFactory.class);
	static ActorSystem system;
	private static Map<Class<?>, String> actorClassMap = new HashMap<Class<?>, String>();
	private static Map<Class<?>, String[]> actorSelectorsMap = new HashMap<Class<?>, String[]>();
	private static String[] IActorsServices = new String[] { "com.pogeyan.cmis.actors.IAclActor",
			"com.pogeyan.cmis.actors.IDiscoveryActor", "com.pogeyan.cmis.actors.INavigationActor",
			"com.pogeyan.cmis.actors.IObjectActor", "com.pogeyan.cmis.actors.IPolicyActor",
			"com.pogeyan.cmis.actors.IRelationshipActor", "com.pogeyan.cmis.actors.IRepositoryActor",
			"com.pogeyan.cmis.actors.ITypeCacheActor", "com.pogeyan.cmis.actors.IVersioningActor",
			"com.pogeyan.cmis.auth.ILoginActor" };

	public void setExternalActors(String[] externalActors) {
		IActorsServices = ObjectArrays.concat(IActorsServices, externalActors, String.class);
	}

	public ActorServiceFactory() {
		ActorServiceFactory.setSystem(ActorSystem.create("GatewaySystem"));
		this.storeActorMetaData();
	}

	public static ActorSystem getSystem() {
		return system;
	}

	public static void setSystem(ActorSystem system) {
		ActorServiceFactory.system = system;
	}

	private void storeActorMetaData() {
		for (String actor : IActorsServices) {
			try {
				Class<?> ActorClassFactory = Class.forName(actor);
				IActorService ActorFactory = (IActorService) ActorClassFactory.newInstance();
				actorClassMap.put(ActorFactory.getActorClass(), ActorFactory.getServiceURL());
				actorSelectorsMap.put(ActorFactory.getActorClass(), ActorFactory.getMethodSelectors());
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException e) {
				LOG.error("Could not create a IActorsServices factory instance: {}", e);
			}
		}
	}

	public static ActorRef initActorRef(String typeName, String actionName) throws ClassNotFoundException {
		Map<Object, Class<?>> classMap = actorClassMap.entrySet().stream().filter(t -> t.getValue().equals(typeName))
				.collect(Collectors.toMap(a -> a.getValue(), a -> a.getKey()));
		Map<Object, Class<?>> selectorsMap = actorSelectorsMap.entrySet().stream()
				.filter(t -> Arrays.asList(t.getValue()).contains(actionName))
				.collect(Collectors.toMap(a -> actionName, a -> a.getKey()));
		if (classMap.size() > 0) {
			return system.actorOf(Props.create(classMap.get(typeName)), typeName + "_" + UUID.randomUUID());
		} else if (selectorsMap.size() > 0) {
			return system.actorOf(Props.create(selectorsMap.get(actionName)), actionName + "_" + UUID.randomUUID());
		} else {
			return null;
		}

	}

}