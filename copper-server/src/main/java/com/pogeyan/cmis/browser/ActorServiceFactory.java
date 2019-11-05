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

import com.pogeyan.cmis.api.IActorService;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;

public class ActorServiceFactory {

	/** The constant logger */
	private static final Logger LOG = LoggerFactory.getLogger(ActorServiceFactory.class);
	private static ActorServiceFactory sf = null;
	private ActorSystem system;
	private Map<Class<?>, String> actorClassMap = new HashMap<Class<?>, String>();
	private Map<ActorRef, String> serviceActorActionRefs = new HashMap<ActorRef, String>();
	private Map<ActorRef, Boolean> singletonActorRefs = new HashMap<ActorRef, Boolean>();
	private Map<Class<?>, String[]> actorSelectorsMap = new HashMap<Class<?>, String[]>();
	private Map<ActorRef, String[]> serviceActorSelectorRefs = new HashMap<ActorRef, String[]>();
	private static String[] IActorsServices = new String[] { "com.pogeyan.cmis.actors.IAclActor",
			"com.pogeyan.cmis.actors.IDiscoveryActor", "com.pogeyan.cmis.actors.INavigationActor",
			"com.pogeyan.cmis.actors.IObjectActor", "com.pogeyan.cmis.actors.IPolicyActor",
			"com.pogeyan.cmis.actors.IRelationshipActor", "com.pogeyan.cmis.actors.IRepositoryActor",
			"com.pogeyan.cmis.actors.ITypeCacheActor", "com.pogeyan.cmis.actors.IVersioningActor",
			"com.pogeyan.cmis.auth.ILoginActor" };

	public void setExternalActors(String[] externalActors) {
		LOG.info("Storing External Actor MetaData");
		this.storeActorMetaData(externalActors);
	}

	public ActorServiceFactory() {
		LOG.info("Storing Internal Actor MetaData");
		this.setSystem(ActorSystem.create("GatewaySystem"));
		this.storeActorMetaData(IActorsServices);
	}

	public static ActorServiceFactory getInstance() {
		if (sf == null) {
			sf = new ActorServiceFactory();
		}
		return sf;
	}

	public void setSystem(ActorSystem system) {
		this.system = system;
	}

	public ActorSystem getSystem() {
		return system;
	}

	private void storeActorMetaData(String[] actors) {
		for (String actor : actors) {
			try {
				Class<?> ActorClassFactory = Class.forName(actor);
				IActorService ActorFactory = (IActorService) ActorClassFactory.newInstance();
				if (ActorFactory.isSingletonService()) {
					LOG.info("initializing SingletonServiceActor: {}", ActorFactory.getServiceURL());
					ActorRef serviceActor = system.actorOf(Props.create(ActorFactory.getActorClass()),
							ActorFactory.getServiceURL());
					serviceActorSelectorRefs.put(serviceActor, ActorFactory.getMethodSelectors());
					serviceActorActionRefs.put(serviceActor, ActorFactory.getServiceURL());
					singletonActorRefs.put(serviceActor, ActorFactory.isSingletonService());
				} else {
					actorClassMap.put(ActorFactory.getActorClass(), ActorFactory.getServiceURL());
					actorSelectorsMap.put(ActorFactory.getActorClass(), ActorFactory.getMethodSelectors());

				}
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException e) {
				LOG.error("Could not create a IActorsServices factory instance: {}", e);
			}
		}
	}

	public ActorRef initActorRef(String typeName, String actionName) throws ClassNotFoundException {
		Map<Object, Class<?>> classMap = sf.actorClassMap.entrySet().stream().filter(t -> t.getValue().equals(typeName))
				.collect(Collectors.toMap(a -> a.getValue(), a -> a.getKey()));
		Map<Object, Class<?>> selectorsMap = sf.actorSelectorsMap.entrySet().stream()
				.filter(t -> Arrays.asList(t.getValue()).contains(actionName))
				.collect(Collectors.toMap(a -> actionName, a -> a.getKey()));
		if (classMap.size() > 0) {
			return this.system.actorOf(Props.create(classMap.get(typeName)), typeName + "_" + UUID.randomUUID());
		} else if (selectorsMap.size() > 0) {
			return this.system.actorOf(Props.create(selectorsMap.get(actionName)),
					actionName + "_" + UUID.randomUUID());
		} else if (sf.serviceActorSelectorRefs.size() > 0) {
			Map<Object, Object> serviceActorSelectorMap = sf.serviceActorSelectorRefs.entrySet().stream()
					.filter(a -> Arrays.asList(a.getValue()).contains(actionName))
					.collect(Collectors.toMap(a -> actionName, a -> a.getKey()));
			Map<Object, Object> serviceActorActionMap = sf.serviceActorActionRefs.entrySet().stream()
					.filter(a -> a.getValue().equals(typeName))
					.collect(Collectors.toMap(a -> typeName, a -> a.getKey()));
			if (serviceActorSelectorMap.size() > 0) {
				return (ActorRef) serviceActorSelectorMap.get(actionName);
			} else if (serviceActorActionMap.size() > 0) {
				return (ActorRef) serviceActorActionMap.get(typeName);
			} else {
				return null;
			}
		} else {
			return null;
		}
	}

	public boolean isSingletonActor(ActorRef actor) {
		Map<ActorRef, Boolean> isSingletonActorRef = singletonActorRefs.entrySet().stream()
				.filter(a -> a.getKey().equals(actor)).collect(Collectors.toMap(a -> a.getKey(), a -> a.getValue()));
		if (isSingletonActorRef.size() > 0) {
			return isSingletonActorRef.get(actor);
		}
		return false;
	}
}