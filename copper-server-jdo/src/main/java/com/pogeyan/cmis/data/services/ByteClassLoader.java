package com.pogeyan.cmis.data.services;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

import groovy.lang.GroovyClassLoader;

public class ByteClassLoader extends URLClassLoader {
	private static final URL[] EMPTY_URL_ARRAY = new URL[0];
	private Map<String, byte[]> classBytes = new HashMap<>();
	private final GroovyClassLoader gcl;

	public ByteClassLoader(Map<String, byte[]> classBytes, GroovyClassLoader gcl) {
		super(EMPTY_URL_ARRAY);
		this.classBytes = classBytes;
		this.gcl = gcl;
	}

	@Override
	protected Class<?> findClass(final String name) throws ClassNotFoundException {
		if (this.classBytes.containsKey(name)) {
			byte[] classBy = this.classBytes.get(name);
			return defineClass(name, classBy, 0, classBy.length);
		}

		return this.gcl.loadClass(name);
	}

}