package com.pogeyan.cmis.impl.factory;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.ITypePermissionFactory;
import com.pogeyan.cmis.api.data.ITypePermissionService;

public class TypeServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(TypeServiceFactory.class);
	public static ITypePermissionFactory typeFlowFactory = null;

	public static ITypePermissionService createTypePermissionFlowService(String repositoryId) {
		if (typeFlowFactory != null) {
			try {
				return typeFlowFactory.getTypePermissionFlowService(repositoryId);
			} catch (Exception e) {
				LOG.error("className: {}, error: {}", "TypeServiceFactory", e.getMessage());
				throw new CmisInvalidArgumentException(e.getMessage());
			}
		}
		return null;
	}

	public static void setTypePermissionFactory(ITypePermissionFactory typePermissionFlowFactory) {
		LOG.info("className: {}, setting typeFlowService: {}", "TypeServiceFactory", typePermissionFlowFactory);
		typeFlowFactory = typePermissionFlowFactory;
	}
}
