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
import java.util.HashMap;

import org.codehaus.jackson.JsonParseException;
import org.codehaus.jackson.annotate.JsonCreator;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.annotate.JsonDeserialize;

import com.pogeyan.cmis.api.utils.Helpers;

/**
 * BaseMessage container for messages passed from/to between actors.
 */
public class BaseMessage {
	private String messageId;
	private HashMap<String, Object> baggage = new HashMap<String, Object>();

	/** The message body */
	private Object messageBody;

	private String typeName;
	private String actionName;
	@JsonDeserialize(using = MessageTypeDeserializer.class)
	private MessageType messageType = MessageType.REQUEST;

	/**
	 * Instantiates a new base message.
	 */
	public BaseMessage() {
		this.setMessageId(Long.toString(Helpers.rand()));
	}

	/**
	 * Gets the message id.
	 *
	 * @return the message id
	 */
	public final String getMessageId() {
		return messageId;
	}

	/**
	 * Sets the message id.
	 *
	 * @param messageId
	 *            the new message id
	 */
	public final void setMessageId(final String messageId) {
		this.messageId = messageId;
	}

	/**
	 * Gets the message type.
	 *
	 * @return the message type
	 */
	public final MessageType getMessageType() {
		return this.messageType;
	}

	/**
	 * Sets the message type.
	 *
	 * @param messageType
	 *            the new message type
	 */
	public final void setMessageType(final MessageType messageType) {
		this.messageType = messageType;
	}

	/**
	 * Adds the baggage.
	 *
	 * @param key
	 *            the key
	 * @param v
	 *            the v
	 */
	public final void addBaggage(final String key, final Object v) {
		if (!this.baggage.containsKey(key)) {
			this.baggage.put(key, v);
		}
	}

	/**
	 * Gets the baggage.
	 *
	 * @return the baggage
	 */
	public final HashMap<String, Object> getBaggage() {
		return this.baggage;
	}

	/**
	 * Clear baggage.
	 */
	public final void clearBaggage() {
		this.baggage.clear();
	}

	/**
	 * Gets the baggage.
	 *
	 * @param <T>
	 *            the generic type
	 * @param key
	 *            the key
	 * @return the baggage
	 */
	@SuppressWarnings("unchecked")
	public final <T> T getBaggage(final String key) {
		if (this.baggage.containsKey(key)) {
			return (T) this.baggage.get(key);
		}

		return null;
	}

	static ObjectMapper jsonMapper = new ObjectMapper();

	/**
	 * Returns the message body as plain string.
	 * 
	 * @return
	 */
	public final String getMessagePlain() {
		if (this.messageBody != null && this.messageBody.getClass() == String.class) {
			return (String) this.messageBody;
		} else if (this.messageBody != null) {
			try {
				return jsonMapper.writeValueAsString(this.messageBody);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		return "{}";
	}
	
	public final Class<?> getMessageBodyType() {
		return this.messageBody != null ? this.messageBody.getClass() : null;
	}

	/**
	 * Gets the message as type.
	 *
	 * @param <T>
	 *            the generic type
	 * @param cls
	 *            the cls
	 * @return the message as type
	 */
	@SuppressWarnings("unchecked")
	public final <T> T getMessageAsType(final Class<?> cls) {
		if (this.messageBody != null && this.messageBody.getClass() == String.class) {
			Object t0;
			try {
				if (this.messageBody != null && (this.messageBody.equals("") || this.messageBody.equals("{}"))) {
					return null;
				} else {
					t0 = jsonMapper.readValue((String) this.messageBody, cls);
				}
				return (T) t0;
			} catch (JsonParseException e) {
				e.printStackTrace();
			} catch (JsonMappingException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
			return null;
		} else {
			if (this.messageBody != null) {
				return (T) this.messageBody;
			} else {
				return null;
			}
		}
	}

	/**
	 * Empty @{link BaseMessage} instance.
	 *
	 * @return the base message
	 */
	public static BaseMessage empty() {
		BaseMessage bm = new BaseMessage();
		bm.messageType = MessageType.REQUEST;
		return bm;
	}

	/**
	 * Gets the action name.
	 *
	 * @return the action name
	 */
	public final String getActionName() {
		return actionName;
	}

	/**
	 * Sets the action name.
	 *
	 * @param actionName
	 *            the new action name
	 */
	public final void setActionName(final String actionName) {
		this.actionName = actionName;
	}

	/**
	 * Gets the type name.
	 *
	 * @return the type name
	 */
	public final String getTypeName() {
		return typeName;
	}

	/**
	 * Sets the type name.
	 *
	 * @param typeName
	 *            the new type name
	 */
	public final void setTypeName(final String typeName) {
		this.typeName = typeName;
	}

	/**
	 * Set Message Internal.
	 *
	 * @param messageObject
	 */
	public final void setMessageInternal(final Object messageObject) {
		this.messageBody = messageObject;
		/*
		 * if (messageObject != null) { try { this.messageBody =
		 * jsonMapper.writeValueAsString(messageObject); } catch
		 * (JsonGenerationException e) { e.printStackTrace(); } catch
		 * (JsonMappingException e) { e.printStackTrace(); } catch (IOException
		 * e) { e.printStackTrace(); } } else { this.messageBody = "{}"; }
		 */
	}

	/**
	 * Cloning of #{@link MessageType#REQUEST}
	 * 
	 * @see java.lang.Object#clone()
	 */
	@Override
	public BaseMessage clone() {
		return this.clone(MessageType.REQUEST);
	}

	/**
	 * Clones from an existing @{link BaseMessage}.
	 *
	 * @param msgType
	 *            the msg type
	 * @return the base message
	 */
	public final BaseMessage clone(MessageType msgType) {
		BaseMessage b0 = new BaseMessage();
		b0.messageId = this.messageId;
		b0.typeName = this.typeName;
		b0.actionName = this.actionName;
		b0.messageType = msgType;
		b0.baggage = this.baggage;
		return b0;
	}

	/**
	 * JSON creator function, this is used by Jackson to deserialize BaseMessage
	 * from JSON string.
	 *
	 * @param jsonString
	 *            the json string
	 * @return the base message
	 * @throws JsonParseException
	 *             the json parse exception
	 * @throws JsonMappingException
	 *             the json mapping exception
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@JsonCreator
	public static BaseMessage create(String jsonString) throws JsonParseException, JsonMappingException, IOException {
		BaseMessage bm = BaseMessage.jsonMapper.readValue(jsonString, BaseMessage.class);
		return bm;
	}

	/**
	 * Creates the {@link BaseMessage} for the given type, action and message
	 * body.
	 *
	 * @param typeName
	 *            the type name
	 * @param actionName
	 *            the action name
	 * @param messageBody
	 *            the message body
	 * @return the base message
	 */
	public static BaseMessage create(String typeName, final String actionName, final Object messageBody) {
		return BaseMessage.create(typeName, actionName, messageBody, new HashMap<String, Object>());
	}

	/**
	 * Creates the {@link BaseMessage} for the given type, action and message
	 * body.
	 *
	 * @param typeName
	 *            the type name
	 * @param actionName
	 *            the action name
	 * @param messageBody
	 *            the message body
	 * @return the base message
	 */
	public static BaseMessage create(String messageId, String typeName, final String actionName,
			final Object messageBody) {
		BaseMessage bm = BaseMessage.create(typeName, actionName, messageBody, new HashMap<String, Object>());
		bm.setMessageId(messageId);
		return bm;
	}

	/**
	 * Creates the {@link BaseMessage} for the given type, action, message body
	 * and baggage.
	 *
	 * @param typeName
	 *            the type name
	 * @param actionName
	 *            the action name
	 * @param messageBody
	 *            the message body
	 * @param baggage
	 *            the additional attributes that each {@link BaseMessage} has to
	 *            carry
	 * @return the base message
	 */
	public static BaseMessage create(String typeName, final String actionName, final Object messageBody,
			final HashMap<String, Object> baggage) {
		BaseMessage bm = new BaseMessage();
		bm.setMessageId(Long.toString(Helpers.rand()));
		bm.setTypeName(typeName);
		bm.setActionName(actionName);
		bm.setMessageType(MessageType.REQUEST);
		bm.setMessageInternal(messageBody);
		bm.baggage = baggage;
		return bm;
	}

	@Override
	public String toString() {
		return String.format("%s, %s, %s, %s", this.messageId, this.messageType.toString(), this.typeName,
				this.actionName);
	}
}
