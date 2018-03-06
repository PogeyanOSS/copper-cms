package com.pogeyan.cmis.api.data;

import java.util.List;

public interface ICacheProvider {

	public <T> T get(String repositoryId, List<?> key);

	public <T> void put(String repositoryId, String key, T object);

	public void remove(String repositoryId, String key);

	public boolean contains(String repositoryId, String key);

	public void init(long time);

}
