package com.pogeyan.cmis.data.query;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class IQueryResponse implements Map<String, Object> {
	
	private  Map<String, Object> map;
	
	public IQueryResponse() {
		this.map = new HashMap<String, Object>();
	}
	
	public IQueryResponse(String key, Object value) {
		this.map = new LinkedHashMap<String, Object>();
		this.map.put(key, value);
    }
	
	public IQueryResponse(Map<String, Object> map) {
        this.map = new HashMap<String, Object>(map);
    }

	@Override
	public int size() {
		return this.map.size();
	}

	@Override
	public boolean isEmpty() {
		return this.map.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return this.map.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return this.map.containsValue(value);
	}

	@Override
	public Object get(Object key) {
		return this.map.get(key);
	}

	@Override
	public Object put(String key, Object value) {
		return this.map.put(key, value);
	}

	@Override
	public Object remove(Object key) {
		return this.map.remove(key);
	}

	@Override
	public void putAll(Map<? extends String, ? extends Object> m) {
		this.map.putAll(map);
	}

	@Override
	public void clear() {
		this.map.clear();
	}

	@Override
	public Set<String> keySet() {
		return this.map.keySet();
	}

	@Override
	public Collection<Object> values() {
		return this.map.values();
	}

	@Override
	public Set<Entry<String, Object>> entrySet() {
		return this.map.entrySet();
	}

}
