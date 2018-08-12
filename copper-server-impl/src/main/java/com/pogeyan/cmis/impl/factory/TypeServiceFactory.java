package com.pogeyan.cmis.impl.factory;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.auth.IUserGroupObject;
import com.pogeyan.cmis.api.data.ITypePermissionService;
import com.pogeyan.cmis.api.data.ITypePermissionfactory;

public class TypeServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(TypeServiceFactory.class);
	public static ITypePermissionfactory typeFlowFactory = null;

	public static ITypePermissionService createTypePermissionFlowService(String repositoryId, IUserGroupObject[] role) {
		if (typeFlowFactory != null) {
			try {
				return typeFlowFactory.getTypePermissionFlowService(repositoryId, role);
			} catch (Exception e) {
				LOG.error("InvalidTargetObject {}", e.getMessage());
				throw new CmisInvalidArgumentException(e.getMessage());
			}
		}
		return null;
	}

	public static void setTypePermissionFactory(ITypePermissionfactory objetFlowFactory) {
		LOG.info("Setting TypeFlowService: {}", objetFlowFactory);
		typeFlowFactory = objetFlowFactory;
	}
}
