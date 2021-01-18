package com.pogeyan.cmis.impl.factory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.ICacheProvider;

public class CacheProviderServiceFactory {
	private static final Logger LOG = LoggerFactory.getLogger(CacheProviderServiceFactory.class);
	static ICacheProvider typeCacheProvider = null;
	static ICacheProvider userCacheMapProvider = null;
	static ICacheProvider roleCacheMapProvider = null;
	static ICacheProvider relationshipCacheProvider = null;
	static ICacheProvider commonCacheProvider = null;

	static public ICacheProvider getTypeCacheServiceProvider() {
		return typeCacheProvider;
	}

	static public ICacheProvider getUserCacheServiceProvider() {
		return userCacheMapProvider;
	}

	static public ICacheProvider getRoleCacheServiceProvider() {
		return roleCacheMapProvider;
	}
	
	static public ICacheProvider getRelationshipCacheServiceProvider() {
		return relationshipCacheProvider;
	}
	
	static public ICacheProvider getCommonCacheServiceProvider() {
		return commonCacheProvider;
	}

	public static void addTypeCacheService(ICacheProvider cacheProviderServiceFactory) {
		LOG.info("cacheProviderServiceFactory for type: {}", cacheProviderServiceFactory);
		typeCacheProvider = cacheProviderServiceFactory;
	};

	public static void addUserCacheService(ICacheProvider cacheProviderServiceFactory) {
		LOG.info("cacheProviderServiceFactory for user: {}", cacheProviderServiceFactory);
		userCacheMapProvider = cacheProviderServiceFactory;
	};

	public static void addRoleCacheService(ICacheProvider cacheProviderServiceFactory) {
		LOG.info("cacheProviderServiceFactory for role: {}", cacheProviderServiceFactory);
		roleCacheMapProvider = cacheProviderServiceFactory;
	};
	
	public static void addRelationshipCacheService(ICacheProvider cacheProviderServiceFactory) {
		LOG.info("cacheProviderServiceFactory for relationship: {}", cacheProviderServiceFactory);
		relationshipCacheProvider = cacheProviderServiceFactory;
	}

	public static void addCommonCacheService(ICacheProvider cacheProviderServiceFactory) {
		LOG.info("cacheProviderServiceFactory for common services: {}", cacheProviderServiceFactory);
		commonCacheProvider = cacheProviderServiceFactory;
	};
	
	public static void closeAll() {
		typeCacheProvider.close();
		userCacheMapProvider.close();
		roleCacheMapProvider.close();
		relationshipCacheProvider.close();
		commonCacheProvider.close();
	}
}
