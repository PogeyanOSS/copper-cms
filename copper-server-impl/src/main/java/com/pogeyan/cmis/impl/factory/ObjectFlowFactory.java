package com.pogeyan.cmis.impl.factory;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IObjectFlowFactory;
import com.pogeyan.cmis.api.data.IObjectFlowService;

public class ObjectFlowFactory {
	private static final Logger LOG = LoggerFactory.getLogger(ObjectFlowFactory.class);
	public static IObjectFlowFactory objectFlowFactory = null;

	public static IObjectFlowService createObjectFlowService(String parameters) {
		if (objectFlowFactory != null) {
			try {
				return objectFlowFactory.getObjectFlowService(parameters);
			} catch (InvalidTargetObjectTypeException e) {
				LOG.error("InvalidTargetObject {}", e.getMessage());
				throw new CmisInvalidArgumentException(e.getMessage());
			}
		}
		return null;
	}

	public static void setObjectFlow(IObjectFlowFactory objetFlowService) {
		LOG.info("Setting ObjectFlowService: {}", objetFlowService);
		objectFlowFactory = objetFlowService;
	}

}
