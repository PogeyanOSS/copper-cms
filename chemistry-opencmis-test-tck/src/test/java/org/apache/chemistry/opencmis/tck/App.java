package org.apache.chemistry.opencmis.tck;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.FileableCmisObject;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.ItemIterable;
import org.apache.chemistry.opencmis.client.api.OperationContext;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.api.Tree;
import org.apache.chemistry.opencmis.client.runtime.ObjectIdImpl;
import org.apache.chemistry.opencmis.client.runtime.OperationContextImpl;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BindingType;

public class App {
	public static void main(String args[]) {
		Session session = null;
		SessionFactory factory = SessionFactoryImpl.newInstance();
		Map<String, String> parameters = new HashMap<String, String>();
		// user credentials
		parameters.put(SessionParameter.USER, "admin");
		parameters.put(SessionParameter.PASSWORD, "admin123");
		parameters.put(SessionParameter.BROWSER_URL, "http://localhost:9089/MongoTest");
		parameters.put(SessionParameter.BINDING_TYPE, BindingType.BROWSER.value());
		parameters.put(SessionParameter.REPOSITORY_ID, "MongoTest");
		session = factory.createSession(parameters);
		session.setDefaultContext(session.createOperationContext());
		// Map<String, Object> propsource = new HashMap<String, Object>();
		// propsource.put(PropertyIds.NAME, "fhjsgdhdsfd");
		// propsource.put(PropertyIds.OBJECT_TYPE_ID, "Audit_Ticket");
		// session.getRootFolder().createDocument(propsource, null, null);
		Folder ff = (Folder) session.getObject("5ac88fe137638c103837a6f3");
		OperationContext op = new OperationContextImpl();
		op.setOrderBy("cmis:name desc");

		ItemIterable<CmisObject> descendants = ff.getChildren(op);
		descendants.forEach(t -> {
			t.getAcl();
		});

	}

}
