package com.pogeyan.cmis.data.mongo;

public interface PassiveExpiringMapListener<K, V> {
	public void notifyOnRemoval(Object key, V value);
}
