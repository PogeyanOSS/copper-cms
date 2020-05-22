package org.apache.chemistry.opencmis.tck.tests.crud;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.INFO;

import java.util.HashMap;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.ObjectId;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.tck.CmisTestResult;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;

public class MultipleFolderRenameTest extends AbstractSessionTest {

	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("Multiple Folders Rename Test");
		setDescription("Create Folders, rename Folders, Check path updated ");
	}

	public void run(Session session) {
		CmisTestResult f;

		try {
			// create a test folder
			Folder testFolder = createTestFolder(session);

			Folder newFolder = createFolder(session, testFolder, "folderA");
			Folder newFolder1 = createFolder(session, newFolder, "folderB");
			Folder newFolder2 = createFolder(session, newFolder1, "folderC");
			Folder newFolder3 = createFolder(session, newFolder2, "folderD");

			f = createResult(INFO, "Created the Folders.");

			// update folderB name
			Map<String, Object> properties = new HashMap<String, Object>();
			properties.put(PropertyIds.NAME, "folderE");
			ObjectId foldId = newFolder1.updateProperties(properties, false);
			newFolder1 = (Folder) session.getObject(foldId.getId());
			f = createResult(FAILURE, "Paths not correct");
			addResult(assertEquals(newFolder1.getPropertyValue(PropertyIds.PATH),
					"/" + testFolder.getName() + "/folderA/folderE", null, f));
			// update folderC name
			properties.put(PropertyIds.NAME, "folderF");
			foldId = newFolder2.updateProperties(properties, false);
			newFolder2 = (Folder) session.getObject(foldId.getId());
			addResult(assertEquals(newFolder2.getPropertyValue(PropertyIds.PATH),
					"/" + testFolder.getName() + "/folderA/folderE/folderF", null, f));
			newFolder3 = (Folder) session.getObject(newFolder3.getId());
			addResult(assertEquals(newFolder3.getPropertyValue(PropertyIds.PATH),
					"/" + testFolder.getName() + "/folderA/folderE/folderF/folderD", null, f));

		} finally {
			// delete the test folder
			deleteTestFolder();
		}
		addResult(createInfoResult("Tested Multiple Folders Rename"));

	}
}
