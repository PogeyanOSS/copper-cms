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
package com.pogeyan.cmis.api;

/**
 * The MessageType defining the REQUEST/RESPONSE of the {@link BaseMessage}
 * message.
 */
public enum MessageType {
	REQUEST("0"), RESPONSE("1"), ERROR("2");

	MessageType(final String value) {
	}

	/**
	 * Gets a MessageType from id or <tt>null</tt> if the requested type doesn't
	 * exist.
	 * 
	 * @param id
	 *            String
	 * @return MessageType
	 */
	public static MessageType fromId(final String id) {
		if (id != null) {
			for (MessageType type : MessageType.values()) {
				if (id.equalsIgnoreCase(type.name())) {
					return type;
				}
			}
		}
		return null;
	}
}
