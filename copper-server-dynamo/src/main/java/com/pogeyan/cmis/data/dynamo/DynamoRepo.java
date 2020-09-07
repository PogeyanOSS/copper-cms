package com.pogeyan.cmis.data.dynamo;

import com.amazonaws.services.dynamodbv2.document.DynamoDB;

public class DynamoRepo{
	
	private DynamoDB dynamoDb;
	private String repoId;
	
	public DynamoRepo(String repoId ,DynamoDB dynamoDb) {
		this.repoId = repoId;
		this.dynamoDb = dynamoDb;
	}
	
	
	public DynamoDB getDynamoDb() {
		return dynamoDb;
	}
	
	public void setDynamoDb(DynamoDB dynamoDb) {
		this.dynamoDb = dynamoDb;
	}
	
	public String getRepoId() {
		return repoId;
	}
	
	public void setRepoId(String repoId) {
		this.repoId = repoId;
	}
	
	
	

}
