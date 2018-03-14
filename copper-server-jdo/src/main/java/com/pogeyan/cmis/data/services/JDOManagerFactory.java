package com.pogeyan.cmis.data.services;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IDBClientFactory;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDiscoveryServiceDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.repo.IRepository;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;

public class JDOManagerFactory implements IDBClientFactory {
	private static final Logger LOG = LoggerFactory.getLogger(JDOManagerFactory.class.getName());
	private static String MBASEOBJECTDAOIMPL = "MBaseObjectDAOImpl";
	private static String MDISCOVERYSERVICEDAO = "MDiscoveryServiceDAO";
	private static String MDOCUMENTOBJECTDAO = "MDocumentObjectDAO";
	private static String MDOCUMENTTYPEMANAGERDAO = "MDocumentTypeManagerDAO";
	private static String MNAVIGATIONSERVICEDAO = "MNavigationServiceDAO";
	private static String MTYPEMANAGERDAO = "MTypeManagerDAO";
	private Map<Class<?>, String> objectServiceClass = new HashMap<>();

	public JDOManagerFactory() {
		objectServiceClass.put(MBaseObjectDAO.class, JDOManagerFactory.MBASEOBJECTDAOIMPL);
		objectServiceClass.put(MDiscoveryServiceDAO.class, JDOManagerFactory.MDISCOVERYSERVICEDAO);
		objectServiceClass.put(MDocumentObjectDAO.class, JDOManagerFactory.MDOCUMENTOBJECTDAO);
		objectServiceClass.put(MNavigationServiceDAO.class, JDOManagerFactory.MNAVIGATIONSERVICEDAO);
		objectServiceClass.put(MDocumentTypeManagerDAO.class, JDOManagerFactory.MDOCUMENTTYPEMANAGERDAO);
		objectServiceClass.put(MTypeManagerDAO.class, JDOManagerFactory.MTYPEMANAGERDAO);
		JDOServiceImpl.getInstance().init();
	}

	public static IDBClientFactory createDatabaseService() {
		return new JDOManagerFactory();
	}

	@Override
	public String getType() {
		return "jdo";
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T getObjectService(String repositoryId, Class<?> objectServiceClass) {
		String className = this.objectServiceClass.get(objectServiceClass);
		if (LOG.isDebugEnabled()) {
			LOG.debug("className: {}", className);
		}
		if (className.equals(JDOManagerFactory.MTYPEMANAGERDAO)) {
			return (T) new JTypeManagerDAOImpl();
		} else if (className.equals(JDOManagerFactory.MDOCUMENTTYPEMANAGERDAO)) {
			return (T) new JDocumentTypeManagerDAOImpl();
		} else if (className.equals(JDOManagerFactory.MBASEOBJECTDAOIMPL)) {
			return (T) new JBaseObjectDAOImpl();
		} else if (className.equals(JDOManagerFactory.MDOCUMENTOBJECTDAO)) {
			return (T) new JDocumentObjectDAOImpl();
		} else if (className.equals(JDOManagerFactory.MDISCOVERYSERVICEDAO)) {
			return (T) new JDiscoveryServiceDAOImpl();
		}
		return null;
	}

	@Override
	public void addIndex(String repositoryId, String[] columnsToIndex) {

	}

	static Properties getProperties(String repositoryId) {
		Properties properties = new Properties();
		IRepository repository = RepositoryManagerFactory.getInstance().getRepository(repositoryId);
		properties.setProperty("javax.jdo.PersistenceManagerFactoryClass",
				"org.datanucleus.api.jdo.JDOPersistenceManagerFactory");
		properties.setProperty("javax.jdo.option.ConnectionDriverName", repository.getDBName().get("driver"));
		properties.setProperty("javax.jdo.option.ConnectionURL", repository.getDBName().get("url"));
		properties.setProperty("javax.jdo.option.ConnectionUserName", repository.getDBName().get("userName"));
		properties.setProperty("javax.jdo.option.ConnectionPassword", repository.getDBName().get("password"));
		properties.setProperty("datanucleus.schema.autoCreateTables", "true");
		return properties;
	}
}
