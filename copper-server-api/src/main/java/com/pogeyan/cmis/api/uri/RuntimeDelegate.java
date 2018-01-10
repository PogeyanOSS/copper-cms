/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 ******************************************************************************/
package com.pogeyan.cmis.api.uri;

/**
 * Provides access to core implementation classes for interfaces. This class is
 * used by internal abstract API implementations and it is not intended to be
 * used by others.
 * 
 * @org.apache.olingo.odata2.DoNotImplement
 * 
 */
public abstract class RuntimeDelegate {

	private static final String IMPLEMENTATION = "com.pogeyan.cmis.impl.uri.RuntimeDelegateImpl";

	/**
	 * Create a runtime delegate instance from the core library. The core
	 * library (org.apache.olingo.odata2.core.jar) needs to be included into the
	 * classpath of the using application.
	 * 
	 * @return an implementation object
	 */
	private static RuntimeDelegateInstance getInstance() {
		RuntimeDelegateInstance delegate;

		try {
			final Class<?> clazz = Class.forName(RuntimeDelegate.IMPLEMENTATION);

			/*
			 * We explicitly do not use the singleton pattern to keep the server
			 * state free and avoid class loading issues also during hot
			 * deployment.
			 */
			final Object object = clazz.newInstance();
			delegate = (RuntimeDelegateInstance) object;

		} catch (final Exception e) {
			throw new RuntimeDelegateException(e);
		}
		return delegate;
	}

	protected abstract UriParser getUriParser();

	/**
	 * Returns an parser which can parse OData uris based on metadata.
	 * 
	 * @param edm
	 *            metadata of the implemented service
	 * @return an implementation object
	 */
	public static UriParser getUriParserInstance() {
		return RuntimeDelegate.getInstance().getUriParser();
	}

	/**
	 * An implementation is available in the core library.
	 * 
	 * @org.apache.olingo.odata2.DoNotImplement
	 */
	public static abstract class RuntimeDelegateInstance {
	    protected abstract UriParser getUriParser();
	}

	private static class RuntimeDelegateException extends RuntimeException {

		private static final long serialVersionUID = 1L;

		public RuntimeDelegateException(final Exception e) {
			super(e);
		}
	}
}
