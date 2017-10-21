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
package com.pogeyan.cmis.data.objects;

import org.mongodb.morphia.annotations.Embedded;
import org.mongodb.morphia.annotations.Id;

@Embedded
public class MPropertyData {
	@Id
	private String id;
	private String localName;
	private String DisplayName;
	private String queryName;
	private String stringValue;
	private boolean booleanValue;
	private int integerValue;
	private Long dateValue;
	private double decimalValue;

	public MPropertyData() {
	}

	public MPropertyData(String id, String localName, String displayName, String queryName, String stringValue,
			boolean booleanValue, int integerValue, Long dateValue, double decimalValue) {
		super();
		this.id = id;
		this.localName = localName;
		DisplayName = displayName;
		this.queryName = queryName;
		this.stringValue = stringValue;
		this.booleanValue = booleanValue;
		this.integerValue = integerValue;
		this.dateValue = dateValue;
		this.decimalValue = decimalValue;
	}

	public String getId() {
		return this.id;
	}

	public String getLocalName() {
		return this.localName;
	}

	public String getDisplayName() {
		return this.DisplayName;
	}

	public String getQueryName() {
		return this.queryName;
	}

	public void setId(String id) {
		this.id = id;
	}

	public void setLocalName(String localName) {
		this.localName = localName;
	}

	public void setDisplayName(String displayName) {
		DisplayName = displayName;
	}

	public void setQueryName(String queryName) {
		this.queryName = queryName;
	}

	public String getStringValue() {
		return stringValue;
	}

	public void setStringValue(String stringValue) {
		this.stringValue = stringValue;
	}

	public boolean isBooleanValue() {
		return booleanValue;
	}

	public void setBooleanValue(boolean booleanValue) {
		this.booleanValue = booleanValue;
	}

	public int getIntegerValue() {
		return integerValue;
	}

	public void setIntegerValue(int integerValue) {
		this.integerValue = integerValue;
	}

	public Long getLongValue() {
		return dateValue;
	}

	public void setLongValue(Long dateValue) {
		this.dateValue = dateValue;
	}

	public double getDecimalValue() {
		return decimalValue;
	}

	public void setDecimalValue(double decimalValue) {
		this.decimalValue = decimalValue;
	}

}
