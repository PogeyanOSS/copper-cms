package com.pogeyan.cmis.data.services;

import java.net.URL;
import java.net.URLClassLoader;

public class ByteClassLoader extends URLClassLoader {
	private static final URL[] EMPTY_URL_ARRAY = new URL[0];
	private final byte[] classBytes;

	public ByteClassLoader(byte[] extraClassDefs) {
		super(EMPTY_URL_ARRAY);
		this.classBytes = extraClassDefs;
	}

	@Override
	protected Class<?> findClass(final String name) throws ClassNotFoundException {
		if (this.classBytes != null) {
			return defineClass(name, this.classBytes, 0, this.classBytes.length);
		}
		return null;
	}
}
