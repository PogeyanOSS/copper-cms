package com.pogeyan.cmis.impl.cacheProvider;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.pogeyan.cmis.api.data.ICacheProvider;

public class GoogleGuiceCacheProviderImpl implements ICacheProvider {

	private Map<String, Cache<String, Object>> repo = new HashMap<String, Cache<String, Object>>();
	private long intervalTime;

	@SuppressWarnings("unchecked")
	@Override
	public <T> T get(String repositoryId, List<?> key) {
		Cache<String, Object> typeCacheMap = repo.get(repositoryId);
		if (typeCacheMap != null) {
			if (key == null) {
				if (typeCacheMap.size() > 0) {
					return (T) typeCacheMap.asMap().entrySet().stream().map(t -> t.getValue())
							.collect(Collectors.toList());
				}
				return null;
			} else if (key.size() == 1) {
				return (T) Arrays.asList(typeCacheMap.getIfPresent(key.get(0)));
			} else {
				return (T) key.stream().map(t -> typeCacheMap.getIfPresent(t.toString())).collect(Collectors.toList());
			}
		}
		return null;
	}

	@Override
	public <T> void put(String repositoryId, String key, T object) {
		Cache<String, Object> repoCacheMap = repo.get(repositoryId);
		if (repoCacheMap != null) {
			repoCacheMap.put(key, object);
		} else {
			Cache<String, Object> typeCacheMap = CacheBuilder.newBuilder()
					.expireAfterWrite(intervalTime, TimeUnit.SECONDS).build();
			typeCacheMap.put(key, object);
			repo.put(repositoryId, typeCacheMap);
		}

	}

	@Override
	public boolean contains(String repositoryId, String key) {
		Cache<String, Object> typeCacheMap = repo.get(repositoryId);
		boolean typePresent = typeCacheMap.getIfPresent(key) != null ? true : false;
		return typePresent;
	}

	@Override
	public void init(long time) {
		this.intervalTime = time;
	}

	@Override
	public void remove(String repositoryId, String key) {
		Cache<String, Object> typeCacheMap = repo.get(repositoryId);
		if (typeCacheMap != null) {
			typeCacheMap.invalidate(key);
		}
	}

	@Override
	public void removeAll(String repositoryId) {
		if (repo.containsKey(repositoryId)) {
			repo.remove(repositoryId);
		}
	}
}
