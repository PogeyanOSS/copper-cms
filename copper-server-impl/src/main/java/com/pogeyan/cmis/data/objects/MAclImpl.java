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

import java.util.ArrayList;
import java.util.List;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.mongodb.morphia.annotations.Entity;

@Entity(noClassnameStored = true)
public class MAclImpl implements Acl {
	private List<MAce> ace;
	private boolean exact;
	private String aclPropagation;

	public MAclImpl() {
		this.aclPropagation = "";
	}

	public MAclImpl(List<MAce> ace, String aclPropagation, boolean exact) {
		this.ace = ace;
		this.exact = exact;
		this.setAclPropagation(aclPropagation);
	}

	@Override
	public Boolean isExact() {
		return exact;
	}

	public void setAces(List<MAce> ace) {
		this.ace = ace;
	}

	@Override
	public List<Ace> getAces() {

		List<Ace> listAce = new ArrayList<Ace>(ace.size());
		for (MAce ace : ace) {
			AccessControlEntryImpl ace2 = new AccessControlEntryImpl();
			ace2.setPermissions(ace.getPermissions());
			ace2.setExtensions(ace.getExtensions());
			ace2.setPrincipal(new AccessControlPrincipalDataImpl(ace.getPrincipalId()));
			listAce.add(ace2);
		}
		return listAce;
	}

	public List<CmisExtensionElement> getExtensions() {
		return null;
	}

	public void setExtensions(List<CmisExtensionElement> extensions) {

	}

	public String getAclPropagation() {
		return aclPropagation;
	}

	public void setAclPropagation(String aclPropagation) {
		this.aclPropagation = aclPropagation;
	}
}
