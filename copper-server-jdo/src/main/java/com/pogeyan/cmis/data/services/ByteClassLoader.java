package com.pogeyan.cmis.data.services;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

public class ByteClassLoader extends URLClassLoader {
	private static final URL[] EMPTY_URL_ARRAY = new URL[0];
	private final Map<String, byte[]> extraClassDefs;



	public ByteClassLoader(Map<String, byte[]> extraClassDefs) {
		super(EMPTY_URL_ARRAY);
		this.extraClassDefs = new HashMap<String, byte[]>(extraClassDefs);
	}

	@Override
	protected Class<?> findClass(final String name) throws ClassNotFoundException {
		byte[] classBytes = this.extraClassDefs.remove(name);
		if (classBytes != null) {
			return defineClass(name, classBytes, 0, classBytes.length);
		}
		return super.findClass(name);
	}

}