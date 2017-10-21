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
package com.pogeyan.cmis.api.messages;

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.ContentStream;

public class PostRequest extends QueryGetRequest {
	private boolean isMultipart = false;	
	private String cmisAction;
	private String token;
	private Map<String, List<String>> propertyData;
	private List<String> policies;
	private Acl addAcl;
	private Acl removeAcl;
	private ContentStream contentStream;
	private List<String> objectIds;
	private List<String> changeTokens;
	private List<String> addSecondaryTypes;
	private List<String> removeSecondaryTypes;
	private String policyId;
	private String aclPropagation;
	private String requestBody = "";	

	public String getAclPropagation() {
		return aclPropagation;
	}

	public void setAclPropagation(String string) {
		this.aclPropagation = string;
	}

	public boolean isMultipart() {
		return isMultipart;
	}

	public void setMultipart(boolean isMultipart) {
		this.isMultipart = isMultipart;
	}

	public String getCmisAction() {
		return cmisAction;
	}

	public void setCmisAction(String cmisAction) {
		this.cmisAction = cmisAction;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public Map<String, List<String>> getPropertyData() {
		return propertyData;
	}

	public void setPropertyData(Map<String, List<String>> propertyData) {
		this.propertyData = propertyData;
	}

	public List<String> getPolicies() {
		return policies;
	}

	public void setPolicies(List<String> policies) {
		this.policies = policies;
	}

	public Acl getAddAcl() {
		return addAcl;
	}

	public void setAddAcl(Acl addAcl) {
		this.addAcl = addAcl;
	}

	public Acl getRemoveAcl() {
		return removeAcl;
	}

	public void setRemoveAcl(Acl removeAcl) {
		this.removeAcl = removeAcl;
	}

	public ContentStream getContentStream() {
		return contentStream;
	}

	public void setContentStream(ContentStream contentStream) {
		this.contentStream = contentStream;
	}

	public List<String> getObjectIds() {
		return objectIds;
	}

	public void setObjectIds(List<String> objectIds) {
		this.objectIds = objectIds;
	}

	public List<String> getChangeTokens() {
		return changeTokens;
	}

	public void setChangeTokens(List<String> changeTokens) {
		this.changeTokens = changeTokens;
	}

	public List<String> getAddSecondaryTypes() {
		return addSecondaryTypes;
	}

	public void setAddSecondaryTypes(List<String> addSecondaryTypes) {
		this.addSecondaryTypes = addSecondaryTypes;
	}

	public List<String> getRemoveSecondaryTypes() {
		return removeSecondaryTypes;
	}

	public void setRemoveSecondaryTypes(List<String> removeSecondaryTypes) {
		this.removeSecondaryTypes = removeSecondaryTypes;
	}

	public String getPolicyId() {
		return policyId;
	}

	public void setPolicyId(String policyId) {
		this.policyId = policyId;
	}

	public String getRequestBody() {
		return requestBody;
	}

	public void setRequestBody(String requestBody) {
		this.requestBody = requestBody;
	}

}
