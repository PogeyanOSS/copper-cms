package org.apache.chemistry.opencmis.tck.tests.AclCasesTest;

import java.util.Map;

import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTestGroup;

public class AclCaseTestGroup extends AbstractSessionTestGroup {
	@Override
	public void init(Map<String, String> parameters) throws Exception {
		super.init(parameters);
		setName("Acl Case Test Group");
		setDescription("Acl Case Test.");
		addTest(new AclCaseTest());
	}
}