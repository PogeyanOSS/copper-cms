package com.pogeyan.cmis.impl.factory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.pogeyan.cmis.api.data.IAclService;

public class AclServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(AclServiceFactory.class);
	static IAclService aclserviceclass = null;
	
	static public IAclService getTypeAclService() {
		return aclserviceclass;
	}
	public static void addTypeAclService(IAclService AclServiceFactory) {
		LOG.info("AclServiceFactory for type: {}", AclServiceFactory);
		aclserviceclass = AclServiceFactory;
	};
}
