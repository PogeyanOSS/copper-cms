package org.apache.chemistry.opencmis.tck.awstest;

import java.util.Map;

import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTestGroup;

public class AwsLocalStorageTestGroup extends AbstractSessionTestGroup {
	@Override
	public void init(Map<String, String> parameters) throws Exception {
		super.init(parameters);
		setName("Aws Local Storage Test Group");
		setDescription("Aws Local Storage Test.");
		addTest(new AwsLocalStorageTest());
		addTest(new VirtualFolderAwsStorageTest());
	}
}
