package com.pogeyan.cmis.impl.factory;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IObjectEncryptFactory;
import com.pogeyan.cmis.api.data.IObjectEncryptService;

public class EncryptionFactory {
	private static final Logger LOG = LoggerFactory.getLogger(EncryptionFactory.class);
	public static IObjectEncryptFactory encryptFactory = null;

	public static IObjectEncryptService createEncryptionService(String repositoryId) {
		if (encryptFactory != null) {
			try {
				return encryptFactory.getEncryptionService(repositoryId);
			} catch (InvalidTargetObjectTypeException e) {
				LOG.error("InvalidTargetObject {}", e.getMessage());
				throw new CmisInvalidArgumentException(e.getMessage());
			}
		}
		return null;
	}

	public static void setEncryptFactory(IObjectEncryptFactory objectEncryptFactory) {
		LOG.info("Setting EncryptionService: {}", objectEncryptFactory);
		encryptFactory = objectEncryptFactory;
	}
}
