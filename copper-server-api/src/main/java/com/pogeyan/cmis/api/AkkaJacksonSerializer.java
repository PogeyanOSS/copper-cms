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

import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.JsonParseException;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import akka.serialization.JSerializer;

/**
 * The Class AkkaJacksonSerializer.
 */
public class AkkaJacksonSerializer extends JSerializer {
	private final ObjectMapper mapper = new ObjectMapper();
	private static final Logger logger = LoggerFactory.getLogger(AkkaJacksonSerializer.class);

	/*
	 * (non-Javadoc)
	 * 
	 * @see akka.serialization.Serializer#includeManifest()
	 */
	// This is whether "fromBinary" requires a "clazz" or not
	@Override
	public boolean includeManifest() {
		return false;
	}

	// Pick a unique identifier for your Serializer,
	// you've got a couple of billions to choose from,
	/*
	 * (non-Javadoc)
	 * 
	 * @see akka.serialization.Serializer#identifier()
	 */
	// 0 - 16 is reserved by Akka itself
	@Override
	public final int identifier() {
		return 1234567;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see akka.serialization.Serializer#toBinary(java.lang.Object)
	 */
	// "toBinary" serializes the given object to an Array of Bytes
	@Override
	public byte[] toBinary(Object obj) {
		// Put the code that serializes the object here
		byte[] jsonValue = new byte[] {};
		try {
			jsonValue = mapper.writeValueAsBytes(obj);
			if (logger.isTraceEnabled())
				logger.trace("writing json Value : " + jsonValue);
			return jsonValue;
		} catch (JsonGenerationException e) {
			logger.error("Error while writing value : " + e.getMessage());
			e.printStackTrace();
		} catch (JsonMappingException e) {
			logger.error("Error while writing value : " + e.getMessage());
			e.printStackTrace();
		} catch (IOException e) {
			logger.error("Error while writing value : " + e.getMessage());
			e.printStackTrace();
		}

		return jsonValue;
	}

	// "fromBinary" deserializes the given array,
	/*
	 * (non-Javadoc)
	 * 
	 * @see akka.serialization.JSerializer#fromBinaryJava(byte[],
	 * java.lang.Class)
	 */
	// using the type hint (if any, see "includeManifest" above)
	@Override
	public Object fromBinaryJava(byte[] bytes, Class<?> clazz) {
		// Put your code that deserializes here
		BaseMessage b = null;
		try {
			b = mapper.readValue(bytes, BaseMessage.class);
			if (logger.isTraceEnabled())
				logger.trace("writing Base Message: " + b);
			return b;
		} catch (JsonParseException e) {
			logger.error("Error while writing value : " + e.getMessage());
			e.printStackTrace();
		} catch (JsonMappingException e) {
			logger.error("Error while writing value : " + e.getMessage());
			e.printStackTrace();
		} catch (IOException e) {
			logger.error("Error while writing value : " + e.getMessage());
			e.printStackTrace();
		}

		return null;
	}
}
