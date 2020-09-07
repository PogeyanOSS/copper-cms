package com.pogeyan.cmis.data.dynamo;

import java.io.IOException;

import org.apache.chemistry.opencmis.commons.enums.Cardinality;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

public class CardinalityEnumDeserializer extends StdDeserializer<Cardinality> {
 
	/**
	 * 
	*/
	private static final long serialVersionUID = 1L;
	
	protected CardinalityEnumDeserializer(Class<?> vc) {
		super(vc);
	}

	@Override
	public Cardinality deserialize(JsonParser jsonParser, DeserializationContext ctxt)
      throws IOException, JsonProcessingException {
        JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        String value = node.textValue();
        return Cardinality.fromValue(value);
    }

	
}