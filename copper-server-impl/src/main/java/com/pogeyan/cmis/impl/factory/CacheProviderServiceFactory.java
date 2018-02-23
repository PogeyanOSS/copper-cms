package com.pogeyan.cmis.impl.factory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.ICacheProvider;

public class CacheProviderServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(CacheProviderServiceFactory.class);
	static ICacheProvider typeCacheProvider = null;

	static public ICacheProvider getTypeCacheServiceProvider() {
		return typeCacheProvider;
	}

	public static void addTypeCacheService(ICacheProvider cacheProviderServiceFactory) {
		LOG.info("cacheProviderServiceFactory for type: {}", cacheProviderServiceFactory);
		typeCacheProvider = cacheProviderServiceFactory;
	};
}
