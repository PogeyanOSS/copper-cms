package com.pogeyan.cmis.data.dynamo;

import java.io.IOException;

import org.apache.chemistry.opencmis.commons.enums.PropertyType;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

public class PropertyTypeEnumDeserializer extends StdDeserializer<PropertyType> {

	/**
	 * 
	*/
	private static final long serialVersionUID = 1L;

	protected PropertyTypeEnumDeserializer(Class<?> vc) {
		super(vc);
	}

	@Override
	public PropertyType deserialize(JsonParser jsonParser, DeserializationContext ctxt)
			throws IOException, JsonProcessingException {
		JsonNode node = jsonParser.getCodec().readTree(jsonParser);
		String value = node.textValue();
		return PropertyType.fromValue(value);
	}
}
