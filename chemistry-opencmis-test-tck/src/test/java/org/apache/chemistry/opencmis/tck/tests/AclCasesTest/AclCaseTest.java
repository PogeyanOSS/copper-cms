package org.apache.chemistry.opencmis.tck.tests.AclCasesTest;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.INFO;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.SKIPPED;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.Item;
import org.apache.chemistry.opencmis.client.api.ObjectFactory;
import org.apache.chemistry.opencmis.client.api.OperationContext;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.OperationContextImpl;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.BasicPermissions;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.enums.AclPropagation;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.commons.enums.CmisVersion;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;

public class AclCaseTest extends AbstractSessionTest {
	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("Create Type with AclCases");
		setDescription("Creates a item type with AclCases and creates object of that type");
	}

	@Override
	public void run(Session session) throws Exception {
		if (session.getRepositoryInfo().getCmisVersion() == CmisVersion.CMIS_1_0) {
			addResult(createResult(SKIPPED, "Type mutability is not supported by CMIS 1.0. Test skipped!"));
			return;
		}
		Session session1 = null;
		Session session2 = null;
		Item newItem = createItem(session, session.getRootFolder(), "tck:ACLTestItem");
		OperationContext op = new OperationContextImpl();
		op.setIncludeAcls(true);
		String user1 = System.getenv("USER1");
		String password1 = System.getenv("PASSWORD1");
		String user2 = System.getenv("USER2");
		String password2 = System.getenv("PASSWORD2");

		try {
			session1 = createsession(user1, password1);
			CmisObject item1 = session1.getObject(newItem, op);
			if (item1 != null) {
				addResult(createResult(FAILURE, user1 + "This user1 has doesnt have access but object is fetched"));
			}
		} catch (Exception e) {
			addResult(createResult(INFO,
					"This user1 has does not have valid acces control permission to access this object"));
		}
		ObjectFactory of = (ObjectFactory) session.getObjectFactory();
		List<Ace> addAces = new ArrayList<Ace>();
		addAces.add(of.createAce(user1, Collections.singletonList(BasicPermissions.ALL)));
		session.applyAcl(newItem, addAces, null, AclPropagation.OBJECTONLY);

		try {
			CmisObject item1 = session1.getObject(newItem, op);
			if (item1 != null) {
				addResult(createResult(INFO,
						user1 + " This user1 has valid acces control permission to access this object"));
			}
		} catch (Exception ex) {
			addResult(createResult(FAILURE,
					user1 + " This user1 has valid acces control permission but unable to access the object"));
		}

		session.applyAcl(newItem, null, addAces, AclPropagation.OBJECTONLY);

		try {
			CmisObject item1 = session1.getObject(newItem, op);
			if (item1 != null) {
				addResult(createResult(FAILURE,
						"removed " + user1 + "from ACl. Doesnt have access but object is fetched"));
			}
		} catch (Exception e) {
			addResult(createResult(INFO,
					"This user1 has does not have valid acces control permission to access this object"));
		}

		addAces.add(of.createAce(user2, Collections.singletonList(BasicPermissions.ALL)));
		List<Ace> removeAces = new ArrayList<Ace>();
		removeAces.add(of.createAce(AbstractRunner.USER_NAME, Collections.singletonList(BasicPermissions.ALL)));
		session.applyAcl(newItem, addAces, removeAces, AclPropagation.OBJECTONLY);

		try {
			CmisObject item1 = session.getObject(newItem, op);
			if (item1 != null) {
				addResult(createResult(FAILURE,
						"removed " + session + "from ACl. Doesnt have access but object is fetched"));
			}
		} catch (Exception e) {
			addResult(createResult(INFO,
					"This user1 has does not have valid acces control permission to access this object"));
		}
		// session2 access
		try {
			session2 = createsession(user2, password2);
			CmisObject item2 = session2.getObject(newItem, op);
			if (item2 != null) {
				addResult(createResult(INFO,
						user2 + "This user2 has valid acces control permission to access this object"));
			}
		} catch (Exception ex) {
			addResult(createResult(FAILURE,
					user2 + "This user2 has valid acces control permission but unable to access the object"));
		}
		session.applyAcl(newItem, addAces, removeAces, AclPropagation.OBJECTONLY);

		try {
			CmisObject item2 = session.getObject(newItem, op);
			if (item2 != null) {
				addResult(createResult(FAILURE,
						"removed " + session + "from ACl. Doesnt have access but object is fetched"));
			}
		} catch (Exception e) {
			addResult(createResult(INFO,
					"This user2 has does not have valid acces control permission to access this object"));
		}
		// AclPropagation
		session1.applyAcl(newItem, null, null, AclPropagation.REPOSITORYDETERMINED);
		try {
			CmisObject item1 = session.getObject(newItem, op);
			if (item1 != null) {
				addResult(createResult(INFO, user1
						+ "This user has valid acces control permission to access this object, since its repositorydetermined"));
			}
		} catch (Exception e) {
			addResult(createResult(FAILURE, "can access this object, since its repositorydetermined, but not able to"));
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