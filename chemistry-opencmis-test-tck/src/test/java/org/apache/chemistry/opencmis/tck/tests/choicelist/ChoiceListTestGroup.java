package org.apache.chemistry.opencmis.tck.tests.choicelist;

import java.util.Map;

import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTestGroup;

public class ChoiceListTestGroup extends AbstractSessionTestGroup {
	@Override
	public void init(Map<String, String> parameters) throws Exception {
		super.init(parameters);
		setName("Choice List Test Group");
		setDescription("Choice List Test.");
		addTest(new ChoiceListTest());
		addTest(new HardChoiceListTest());
		addTest(new SecondaryTypeHardChoiceTest());
	}
}
