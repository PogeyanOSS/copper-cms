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

import java.io.IOException;

import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonProcessingException;
import org.codehaus.jackson.map.DeserializationContext;
import org.codehaus.jackson.map.JsonDeserializer;

/**
 * The MessageTypeDeserializer for Jackson serializer.
 */
public class MessageTypeDeserializer extends JsonDeserializer<MessageType> {

	/**
	 * (non-Javadoc)
	 * 
	 * @see org.codehaus.jackson.map.JsonDeserializer#deserialize(org.codehaus.jackson
	 *      .JsonParser, org.codehaus.jackson.map.DeserializationContext)
	 */
	@Override
	public final MessageType deserialize(final JsonParser jp, final DeserializationContext ctxt)
			throws IOException, JsonProcessingException {
		MessageType type = MessageType.fromId(jp.getText());
		if (type != null) {
			return type;
		}
		return MessageType.REQUEST;
	}

}
