package org.apache.chemistry.opencmis.tck.tests.NavigationServices;

import java.util.Map;

import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTestGroup;

public class NavigationServiceTestGroup extends AbstractSessionTestGroup {
	@Override
	public void init(Map<String, String> parameters) throws Exception {
		super.init(parameters);
		setName("Navigation Service Test Group");
		setDescription("Navigation Service Test.");
		addTest(new NavigationServiceTest());
	}
}
