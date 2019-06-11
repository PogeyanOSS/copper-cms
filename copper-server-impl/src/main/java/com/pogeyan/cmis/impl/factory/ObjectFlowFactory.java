package com.pogeyan.cmis.impl.factory;

import java.util.HashMap;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IObjectFlowFactory;
import com.pogeyan.cmis.api.data.IObjectFlowService;

import akka.actor.ActorSystem;

public class ObjectFlowFactory {
	private static final Logger LOG = LoggerFactory.getLogger(ObjectFlowFactory.class);
	static Map<String, IObjectFlowFactory> objectFlowFactory = new HashMap<String, IObjectFlowFactory>();
	static ActorSystem system;

	public static IObjectFlowService createObjectFlowService(String type, String repositoryId) {
		if (objectFlowFactory != null) {
			try {
				return objectFlowFactory.get(type).getObjectFlowService(repositoryId);
			} catch (InvalidTargetObjectTypeException e) {
				LOG.error("InvalidTargetObject {}", e.getMessage());
				throw new CmisInvalidArgumentException(e.getMessage());
			}
		}
		return null;
	}

	public static void setObjectFlow(IObjectFlowFactory objetFlowFactory) {
		objectFlowFactory.put(objetFlowFactory.getStoreSetting().getType(), objetFlowFactory);
		LOG.info("Setting ObjectFlowService: {}", objetFlowFactory);
	}

	public static Map<String, IObjectFlowFactory> getObjectFlowFactoryMap() {
		if (objectFlowFactory != null) {
			return objectFlowFactory;
		}
		return null;
	}

	public static ActorSystem getSystem() {
		return system;
	}

	public static void setSystem(ActorSystem system) {
		ObjectFlowFactory.system = system;
	}

}
