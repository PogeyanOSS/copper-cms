package org.apache.chemistry.opencmis.tck.aclplugintest;

import java.util.HashMap;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.OperationContext;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.OperationContextImpl;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.INFO;


public class ACLPluginTest extends AbstractSessionTest {
	@Override
	public void run(Session session) throws Exception {
 		try {
 			OperationContext op = new OperationContextImpl();
			op.setIncludeAcls(true);
 			Session session_user1 = createsession(System.getenv("USER1"), System.getenv("PASSWORD1"));
			Map<String, Object> props = new HashMap<String, Object>();
			props.put(PropertyIds.OBJECT_ID, "0912");
			props.put(PropertyIds.OBJECT_TYPE_ID, "cmis:document");
			props.put(PropertyIds.NAME, "folder12");
			session_user1.createDocument(props, null, null, null);
 			Session session_user2 = createsession(System.getenv("USER2"), System.getenv("PASSWORD2"));
			CmisObject object_user2 = session_user2.getObject("0912", op);
 			Session session_user3 = createsession(System.getenv("USER3"), System.getenv("PASSWORD3"));
			CmisObject object_user3 = session_user3.getObject("0912", op);
		} catch (Exception e) {
			addResult(createResult(INFO, "does not have valid acces control permission to access this object"));
		}
	}
 	public Session createsession(String username, String password) {
		Session session = null;
		SessionFactory factory = SessionFactoryImpl.newInstance();
		Map<String, String> parameters = new HashMap<String, String>();
		parameters.put(SessionParameter.USER, username);
		parameters.put(SessionParameter.PASSWORD, password);
		parameters.put(SessionParameter.BROWSER_URL, AbstractRunner.BROWSER_URL);
		parameters.put(SessionParameter.BINDING_TYPE, BindingType.BROWSER.value());
		parameters.put(SessionParameter.REPOSITORY_ID, AbstractRunner.REPOSITORY_ID);
		session = factory.createSession(parameters);
		return session;
	}

}
