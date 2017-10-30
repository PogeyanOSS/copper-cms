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
package com.pogeyan.cmis.api.data.common;

import org.apache.chemistry.opencmis.commons.enums.ChangeType;

public enum TokenChangeType {
	CREATED(0), UPDATED(1), DELETED(2), SECURITY(3);
	private final int value;

	TokenChangeType(int v) {
		value = v;
	}

	public int value() {
		return value;
	}

	public static ChangeType fromValue(TokenChangeType v) {
		for (TokenChangeType c : TokenChangeType.values()) {
			if (c == v) {
				return ChangeType.fromValue(c.name().toLowerCase());
			}
		}
		throw new IllegalArgumentException("Illegal value");
	}

	public static int toValue(String v) {
		for (TokenChangeType c : TokenChangeType.values()) {
			if (c.name() == v) {
				return c.value;
			}
		}
		throw new IllegalArgumentException("Illegal value");
	}

	public static TokenChangeType getTokenValue(int v) {
		for (TokenChangeType c : TokenChangeType.values()) {
			if (c.value == v) {
				return c;
			}
		}
		throw new IllegalArgumentException("Illegal value");
	}
}
