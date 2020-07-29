package com.pogeyan.cmis.data.query;

import java.util.List;
import java.util.Map;

public class QueryRequest {
	
	private int size;
	private List<FilterQueryRequest> filter;
	private List<SortQueryRequest> sort;
	private Map<String, QueryRequest> fields;
	private String direction;
	
	public int getSize() {
		return size;
	}
	public void setSize(int size) {
		this.size = size;
	}
	public List<FilterQueryRequest> getFilter() {
		return filter;
	}
	public void setFilter(List<FilterQueryRequest> filter) {
		this.filter = filter;
	}
	public List<SortQueryRequest> getSort() {
		return sort;
	}
	public void setSort(List<SortQueryRequest> sort) {
		this.sort = sort;
	}
	public Map<String, QueryRequest> getFields() {
		return fields;
	}
	public void setFields(Map<String, QueryRequest> fields) {
		this.fields = fields;
	}
	public String getDirection() {
		return direction;
	}
	public void setDirection(String direction) {
		this.direction = direction;
	}
	
}
