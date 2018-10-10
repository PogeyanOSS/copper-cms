package org.apache.chemistry.opencmis.tck.aclplugin;

import java.util.Map;

import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTestGroup;

public class AclPluginTestGroup extends AbstractSessionTestGroup {
	@Override
	public void init(Map<String, String> parameters) throws Exception {
		super.init(parameters);

		setName("Acl Plugin Test Group");
		setDescription("ACL Plugin tests.");

		addTest(new ACLPluginTest());
	}
}
