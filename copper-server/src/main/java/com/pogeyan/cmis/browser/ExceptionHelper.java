/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.browser;

import java.io.PrintWriter;
import java.io.StringWriter;

public final class ExceptionHelper {

	/**
	 * System property to enable stack traces in CMIS exceptions.
	 */
	public static final String ENABLE_STACK_TRACE_PROPERTY = "org.apache.chemistry.opencmis.stacktrace.enable";

	/**
	 * System property to disable stack traces in CMIS exceptions. It's only
	 * here for legacy reasons and should not be used.
	 * 
	 * If this system property is set it takes precedence over
	 * {@link ExceptionHelper#ENABLE_STACK_TRACE_PROPERTY} for backwards
	 * compatibility.
	 */
	@Deprecated
	public static final String DISABLE_STACK_TRACE_PROPERTY = "org.apache.chemistry.opencmis.stacktrace.disable";

	private static final boolean SEND_STACK_TRACE;

	static {
		SEND_STACK_TRACE = Boolean.parseBoolean(System.getProperty(ENABLE_STACK_TRACE_PROPERTY, "false"))
				&& System.getProperty(DISABLE_STACK_TRACE_PROPERTY) == null;
	}

	private ExceptionHelper() {
	}

	/**
	 * Returns the stack trace as string.
	 */
	public static String getStacktraceAsString(Throwable t) {
		if (!SEND_STACK_TRACE || t == null) {
			return null;
		}

		StringWriter sw = new StringWriter(512);
		PrintWriter pw = new PrintWriter(sw);

		t.printStackTrace(pw);

		return sw.toString();
	}
}
