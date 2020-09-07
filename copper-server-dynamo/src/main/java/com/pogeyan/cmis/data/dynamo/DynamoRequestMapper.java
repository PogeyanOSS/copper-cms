package com.pogeyan.cmis.data.dynamo;

import java.io.IOException;
import java.util.Map;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.type.TypeReference;

import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;

public class DynamoRequestMapper {

	public static Map<String, DynamoPropertyDefinition<?>> getRequestMapper(
			Map<String, Object> dynamoPropertyDefinition) {

		Map<String, DynamoPropertyDefinition<?>> propertyDefinition = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			String json = mapper.writeValueAsString(dynamoPropertyDefinition);
			mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			propertyDefinition = mapper.readValue(json, new TypeReference<Map<String, DynamoPropertyDefinition<?>>>() {
			});
			return propertyDefinition;
		} catch (IOException e) {
			throw new RuntimeException("JSON Parser error");
		}
	}

}
