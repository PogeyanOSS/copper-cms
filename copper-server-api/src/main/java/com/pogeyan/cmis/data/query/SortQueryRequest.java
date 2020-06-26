package com.pogeyan.cmis.data.query;

public class SortQueryRequest {
	
	private String field;
	private String operator;
	
	public String getField() {
		return field;
	}
	public void setField(String field) {
		this.field = field;
	}
	public String getOperator() {
		return operator;
	}
	public void setOperator(String operator) {
		this.operator = operator;
	}

}
