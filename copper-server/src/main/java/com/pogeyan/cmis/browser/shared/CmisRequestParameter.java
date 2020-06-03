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
package com.pogeyan.cmis.browser.shared;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlListImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;

import com.pogeyan.cmis.api.CustomContentStream;
import com.pogeyan.cmis.api.messages.PostRequest;
import com.pogeyan.cmis.api.messages.QueryGetRequest;

public class CmisRequestParameter {
	public List<String> createPolicies(ControlParser controlParser, PostRequest request) {
		return controlParser.getValues(QueryGetRequest.CONTROL_POLICY);
	}

	public List<String> getObjectIds(ControlParser controlParser, PostRequest request) {
		return controlParser.getValues(QueryGetRequest.CONTROL_OBJECT_ID);
	}

	public List<String> getChangeTokens(ControlParser controlParser, PostRequest request) {
		return controlParser.getValues(QueryGetRequest.CONTROL_CHANGE_TOKEN);
	}

	public List<String> addSecondaryTypes(ControlParser controlParser, PostRequest request) {
		return controlParser.getValues(QueryGetRequest.CONTROL_ADD_SECONDARY_TYPE);
	}

	public List<String> removeSecondaryTypes(ControlParser controlParser, PostRequest request) {
		return controlParser.getValues(QueryGetRequest.CONTROL_REMOVE_SECONDARY_TYPE);
	}

	public Acl createAddAcl(ControlParser controlParser, PostRequest request) {
		List<String> principals = controlParser.getValues(QueryGetRequest.CONTROL_ADD_ACE_PRINCIPAL);
		if (principals == null) {
			return null;
		}

		List<Ace> aces = new ArrayList<Ace>();

		int i = 0;
		for (String principalId : principals) {
			aces.add(new AccessControlEntryImpl(new AccessControlPrincipalDataImpl(principalId),
					controlParser.getValues(QueryGetRequest.CONTROL_ADD_ACE_PERMISSION, i)));
			i++;
		}

		return new AccessControlListImpl(aces);
	}

	public String getAclPropagation(ControlParser controlParser, PostRequest request) {
		return controlParser.getValue(QueryGetRequest.PARAM_ACL_PROPAGATION);
	}

	public Acl createRemoveAcl(ControlParser controlParser, PostRequest request) {
		List<String> principals = controlParser.getValues(QueryGetRequest.CONTROL_REMOVE_ACE_PRINCIPAL);
		if (principals == null) {
			return null;
		}

		List<Ace> aces = new ArrayList<Ace>();

		int i = 0;
		for (String principalId : principals) {
			aces.add(new AccessControlEntryImpl(new AccessControlPrincipalDataImpl(principalId),
					controlParser.getValues(QueryGetRequest.CONTROL_REMOVE_ACE_PERMISSION, i)));
			i++;
		}

		return new AccessControlListImpl(aces);
	}

	public List<CustomContentStream> createContentStream(HttpServletRequest request) {

		if (request instanceof POSTHttpServletRequestWrapper) {
			POSTHttpServletRequestWrapper post = (POSTHttpServletRequestWrapper) request;
			return post.getStreamList();
		}
		return new ArrayList<CustomContentStream>();
	}

	public String getPolicyId(ControlParser controlParser, PostRequest request) {
		return controlParser.getValue(QueryGetRequest.CONTROL_POLICY_ID);
	}

}
