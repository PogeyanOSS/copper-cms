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

import java.util.List;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.data.MutablePrincipal;
import org.apache.chemistry.opencmis.commons.data.Principal;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;

public class MAceImpl implements Ace {
	private List<String> premission;
	private String principalId;
	private boolean isDirect;

	public MAceImpl() {

	}

	public MAceImpl(String principalId, List<String> premission) {
		this.premission = premission;
		this.principalId = principalId;
		this.isDirect = true;
	}

	@Override
	public List<CmisExtensionElement> getExtensions() {
		return null;
	}

	@Override
	public void setExtensions(List<CmisExtensionElement> extensions) {
	}

	@Override
	public Principal getPrincipal() {
		MutablePrincipal principal = new AccessControlPrincipalDataImpl();
		principal.setId(this.principalId);
		return principal;
	}

	@Override
	public String getPrincipalId() {
		return this.principalId;
	}

	@Override
	public List<String> getPermissions() {
		return this.premission;
	}

	@Override
	public boolean isDirect() {
		return this.isDirect;
	}

	public void setPremission(List<String> premission) {
		this.premission = premission;
	}

	public void setPrincipalId(String principalId) {
		this.principalId = principalId;
	}

	public void setDirect(boolean isDirect) {
		this.isDirect = isDirect;
	}

}
