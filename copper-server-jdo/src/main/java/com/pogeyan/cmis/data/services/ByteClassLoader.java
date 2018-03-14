package com.pogeyan.cmis.data.services;

import java.net.URL;
import java.net.URLClassLoader;

import groovy.lang.GroovyClassLoader;

public class ByteClassLoader extends URLClassLoader {
	private static final URL[] EMPTY_URL_ARRAY = new URL[0];
	private final String name;
	private final byte[] classBytes;
	private final GroovyClassLoader gcl;

	public ByteClassLoader(String name, byte[] extraClassDefs, GroovyClassLoader gcl) {
		super(EMPTY_URL_ARRAY);
		this.name = name;
		this.classBytes = extraClassDefs;
		this.gcl = gcl;
	}

	@Override
	protected Class<?> findClass(final String name) throws ClassNotFoundException {
		if (this.name.equals(name) && this.classBytes != null) {
			return defineClass(name, this.classBytes, 0, this.classBytes.length);
		}
		
		return this.gcl.loadClass(name);
	}
}
