package com.pogeyan.cmis.data.dynamo;

import java.util.Map;


public class DynamoRepositoryManagerFactory {
	
	static Map<String,DynamoRepo> repos;
	
	public static Map<String,DynamoRepo> getRepos(){
		return repos;
	}
	
}
