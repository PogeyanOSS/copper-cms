package com.pogeyan.cmis.data.dynamo;

import java.util.HashMap;
import java.util.Map;

public class DynamoClient {
	

	private static Map<String, DynamoRepo> dynamoClient = new HashMap<>();
	
	public DynamoClient() {
	}

	public static DynamoRepo get(String repoId) {
		return dynamoClient.get(repoId);
	}

	public static void put(String repoId, DynamoRepo dynamo) {
		dynamoClient.put(repoId, dynamo);
	}
	
	
}
