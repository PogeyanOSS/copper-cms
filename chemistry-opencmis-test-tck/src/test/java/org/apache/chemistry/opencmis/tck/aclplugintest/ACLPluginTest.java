package org.apache.chemistry.opencmis.tck.aclplugintest;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.INFO;
import java.util.HashMap;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.OperationContext;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.OperationContextImpl;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;

public class ACLPluginTest extends AbstractSessionTest {
	@Override
	public void run(Session session) throws Exception {
		OperationContext op = new OperationContextImpl();
		op.setIncludeAcls(true);
		final String CONTENT = "ACL Plugin test content";
		String user1 = System.getenv("USER1");
		String password1 = System.getenv("PASSWORD1");
		Session session_user1 = createsession(user1, password1);
		Folder testFolder = createTestFolder(session_user1);
		Document newDocument = createDocument(session_user1, testFolder, "doc", CONTENT);
		if (newDocument != null) {
			addResult(createResult(INFO, "Document created successfully by "
					+ session_user1.getSessionParameters().get(SessionParameter.USER)));
		}
		String user2 = System.getenv("USER2");
		String password2 = System.getenv("PASSWORD2");
		Session session_user2 = createsession(user2, password2);
		CmisObject object_user2 = session_user2.getObject(newDocument, op);
		if (object_user2 != null) {
			addResult(createResult(INFO,
					"the ace's are updated and document can be accessed by"
							+ session_user1.getSessionParameters().get(SessionParameter.USER) + "parent that is "
							+ session_user2.getSessionParameters().get(SessionParameter.USER)));
		}
		try {
			String user3 = System.getenv("USER3");
			String password3 = System.getenv("PASSWORD3");
			Session session_user3 = createsession(user3, password3);
			CmisObject object_user3 = session_user3.getObject(newDocument, op);
			if (object_user3 != null) {
				addResult(createResult(FAILURE, "does not have valid acces control permission to access this object"));
			}
		} catch (Exception e) {
			addResult(createResult(INFO, "does not have valid acces control permission to access this object"));
		}
	}

	public Session createsession(String username, String password) {
		Session session = null;
		if (username != null && password != null) {
			SessionFactory factory = SessionFactoryImpl.newInstance();
			Map<String, String> parameters = new HashMap<String, String>();
			parameters.put(SessionParameter.USER, username);
			parameters.put(SessionParameter.PASSWORD, password);
			parameters.put(SessionParameter.BROWSER_URL, AbstractRunner.BROWSER_URL);
			parameters.put(SessionParameter.BINDING_TYPE, BindingType.BROWSER.value());
			parameters.put(SessionParameter.REPOSITORY_ID, AbstractRunner.REPOSITORY_ID);
			session = factory.createSession(parameters);
		} else {
			addResult(createResult(FAILURE, "set the environment variables of username and password"));
		}
		return session;
	}
}
