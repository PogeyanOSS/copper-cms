package com.pogeyan.cmis.impl.factory;

import java.util.HashMap;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IObjectFlowFactory;
import com.pogeyan.cmis.api.data.IObjectFlowService;

public class ObjectFlowFactory {
	private static final Logger LOG = LoggerFactory.getLogger(ObjectFlowFactory.class);
	public static final String STORAGE = "storage";
	static Map<String, IObjectFlowFactory> objectFlowFactory = new HashMap<String, IObjectFlowFactory>();

	public static IObjectFlowService createObjectFlowService(Map<String, String> parameters) {
		String storageType = parameters.get(STORAGE);
		LOG.debug("DB FileSytem Setting type: {}", storageType);
		IObjectFlowService store = null;
		if (objectFlowFactory.get(storageType) != null) {
			try {
				return objectFlowFactory.get(storageType).getObjectFlowService(parameters);
			} catch (InvalidTargetObjectTypeException e) {
				LOG.error("InvalidTargetObject {}", e.getMessage());
				throw new CmisInvalidArgumentException(e.getMessage());
			}
		}

		return store;
	}

	public static void setObjectFlow(IObjectFlowFactory objetFlowService) {
		LOG.info("Setting ObjectFlowService: {}", objetFlowService);
		objectFlowFactory.put(objetFlowService.getStoreSetting().getType(), objetFlowService);
	}

}
