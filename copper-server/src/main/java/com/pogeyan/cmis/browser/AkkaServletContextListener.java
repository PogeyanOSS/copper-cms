/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.browser;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.codahale.metrics.ConsoleReporter;
import com.pogeyan.cmis.api.auth.IAuthFactory;
import com.pogeyan.cmis.api.data.ICacheProvider;
import com.pogeyan.cmis.api.data.IDBClientFactory;
import com.pogeyan.cmis.api.data.IObjectEncryptFactory;
import com.pogeyan.cmis.api.data.IObjectFlowFactory;
import com.pogeyan.cmis.api.data.ITracingService;
import com.pogeyan.cmis.api.data.ITypePermissionFactory;
import com.pogeyan.cmis.api.repo.IRepositoryManager;
import com.pogeyan.cmis.api.repo.IRepositoryStore;
import com.pogeyan.cmis.api.storage.IStorageFactory;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
import com.pogeyan.cmis.impl.factory.EncryptionFactory;
import com.pogeyan.cmis.impl.factory.LoginAuthServiceFactory;
import com.pogeyan.cmis.impl.factory.ObjectFlowFactory;
import com.pogeyan.cmis.impl.factory.StorageServiceFactory;
import com.pogeyan.cmis.impl.factory.TypeServiceFactory;
import com.pogeyan.cmis.server.GatewayActor;
import com.pogeyan.cmis.tracing.TracingApiServiceFactory;

import akka.actor.ActorSystem;
import akka.actor.Props;
import io.prometheus.client.dropwizard.DropwizardExports;

@WebListener
public class AkkaServletContextListener implements ServletContextListener {
	private static final String CONFIG_INIT_PARAM = "org.apache.chemistry.opencmis.REPOSITORY_CONFIG_FILE";
	private static final String CONFIG_FILENAME = "repository.properties";
	private static final String PROPERTY_REPO_STORE_CLASS = "repositoryManagerClass";
	private static final String PROPERTY_AUTH_STORE_CLASS = "authenticationManagerClass";
	private static final String PROPERTY_FILE_STORE_CLASS = "storageManagerClass";
	private static final String PROPERTY_ACTOR_CLASS = "actorManagerClass";
	private static final String PROPERTY_CACHE_PROVIDER_CLASS = "cacheProviderManagerClass";
	private static final String PROPERTY_OBJECT_FLOW_CLASS = "objectFlowManagerClass";
	private static final String PROPERTY_ENCRYPTION_CLASS = "encryptionManagerClass";
	private static final String PROPERTY_TYPE_PERMISSION_CLASS = "typePermissionManagerClass";
	private static final String DEFAULT_TYPE_PERMISSION_CLASS = "com.pogeyan.cmis.impl.TypePermission.DefaultTypePermissionServiceFlowFactory";
	private static final String PROPERTY_INTERVAL_TIME = "intervalTime";
	private static final String DEFAULT_CLASS = "com.pogeyan.cmis.api.repo.RepositoryManagerFactory";
	private static final String DEFAULT_REPO_STORE_CLASS = "com.pogeyan.cmis.repo.MongoDBRepositoryStore";
	private static final String DEFAULT_AUTH_STORE_CLASS = "com.pogeyan.cmis.repo.local.LocalRepoAuthFactory";
	private static final String DEFAULT_FILE_STORE_CLASS = "com.pogeyan.cmis.impl.storage.FileSystemStorageFactory";
	private static final String DEFAULT_CACHE_PROVIDER_CLASS = "com.pogeyan.cmis.impl.cacheProvider.GoogleGuiceCacheProviderImpl";
	private static final String DEFAULT_TRACING_API_CLASS = "com.pogeyan.cmis.tracing.TracingDefaultImpl";
	private static final String PROPERTY_TRACING_API_CLASS = "tracingApiFactory";
	private static final String PROPERTY_DB_CLIENT_FACTORY = "cbmClientFactory";
	private static final String DEFAULT_DB_CLIENT_FACTORY = "com.pogeyan.cmis.data.mongo.services.MongoClientFactory";

	static final Logger LOG = LoggerFactory.getLogger(AkkaServletContextListener.class);


	@Override
	public void contextInitialized(ServletContextEvent sce) {
		ActorServiceFactory.getInstance().setSystem(ActorSystem.create("GatewaySystem"));
		sce.getServletContext().setAttribute("ActorSystem", ActorServiceFactory.getInstance().getSystem());

		String configFilename = sce.getServletContext().getInitParameter(CONFIG_INIT_PARAM);
		if (configFilename == null) {
			configFilename = CONFIG_FILENAME;
		}

		// DatabaseServiceFactory.add(MongoClientFactory.createDatabaseService());

		LOG.info("Registering gateway actor to main actor system");
		ActorServiceFactory.getInstance().getSystem().actorOf(Props.create(GatewayActor.class), "gateway");

		LOG.info("Initializing service factory instances");
		try {
			boolean factory = createServiceFactory(sce, configFilename);
			if (!factory) {
				throw new IllegalArgumentException("Repository manager class not initilaized");
			}
		} catch (Exception e) {
			LOG.error("Service factory couldn't be created: {}", e);
		}

		if (Helpers.isPerfMode()) {
			ConsoleReporter reporter = ConsoleReporter.forRegistry(MetricsInputs.get().getMetrics())
					.convertRatesTo(TimeUnit.SECONDS).convertDurationsTo(TimeUnit.MILLISECONDS).build();
			reporter.start(10, TimeUnit.SECONDS);
			if (Helpers.isPrometheusMode()) {
				MetricsInputs.collectorRegistry().register(new DropwizardExports(MetricsInputs.get().getMetrics()));
			}
		}
	}

	@Override
	public void contextDestroyed(ServletContextEvent sce) {
		ActorSystem system = (ActorSystem) sce.getServletContext().getAttribute("GatewaySystem");
		sce.getServletContext().removeAttribute("ActorSystem");
		system.terminate();
	}

	private boolean createServiceFactory(ServletContextEvent sce, String fileName) throws FileNotFoundException {
		// load properties
		InputStream stream = null;
		try {
			String filePath = null;
			String propertyFileLocation = System.getenv("REPOSITORY_PROPERTY_FILE_LOCATION");
			if (propertyFileLocation == null) {
				filePath = AkkaServletContextListener.class.getClassLoader().getResource(fileName).getPath();
			} else {
				filePath = propertyFileLocation + fileName;
			}
			stream = new FileInputStream(new File(filePath));
		} catch (FileNotFoundException e) {
			LOG.warn("REPOSITORY_PROPERTY_FILE_LOCATION is not found due to: {}", e.getMessage());
			LOG.info("Loading default extensions");
			initializeTracingApiServiceFactory(DEFAULT_TRACING_API_CLASS);
			return initializeExtensions(DEFAULT_CLASS, DEFAULT_REPO_STORE_CLASS, DEFAULT_AUTH_STORE_CLASS,
					DEFAULT_FILE_STORE_CLASS, DEFAULT_CACHE_PROVIDER_CLASS, null, 30 * 60);
		}

		Properties props = new Properties();
		try {
			props.load(stream);
		} catch (IOException e) {
			LOG.error("Cannot load configuration: {}", e);
			return false;
		} finally {
			IOUtils.closeQuietly(stream);
		}

		Map<String, String> parameters = new HashMap<String, String>();

		for (Enumeration<?> e = props.propertyNames(); e.hasMoreElements();) {
			String key = (String) e.nextElement();
			String value = props.getProperty(key);
			parameters.put(key, value);
		}

		// get 'repoManagerclass' property
		String repoStoreClassName = props.getProperty(PROPERTY_REPO_STORE_CLASS);
		if (repoStoreClassName == null) {
			repoStoreClassName = DEFAULT_REPO_STORE_CLASS;
		}
		String authStoreClassName = props.getProperty(PROPERTY_AUTH_STORE_CLASS);
		if (authStoreClassName == null) {
			authStoreClassName = DEFAULT_AUTH_STORE_CLASS;
		}
		String fileStorageClassName = props.getProperty(PROPERTY_FILE_STORE_CLASS);
		if (fileStorageClassName == null) {
			fileStorageClassName = DEFAULT_FILE_STORE_CLASS;
		}

		// checking cbm adapter class is enable or not
		String DBClientFactory = props.getProperty(PROPERTY_DB_CLIENT_FACTORY);
		if (DBClientFactory == null) {
			DBClientFactory = DEFAULT_DB_CLIENT_FACTORY;
		}

		initializeDBClientServiceFactory(DBClientFactory);

		String cacheProviderClassName = props.getProperty(PROPERTY_CACHE_PROVIDER_CLASS);
		if (cacheProviderClassName == null) {
			cacheProviderClassName = DEFAULT_CACHE_PROVIDER_CLASS;
		}

		String interval = props.getProperty(PROPERTY_INTERVAL_TIME);
		long intevalTime;
		if (interval == null) {
			intevalTime = 30 * 60;
		} else {
			intevalTime = Long.parseLong(interval);
		}

		String traceApiClass = props.getProperty(PROPERTY_TRACING_API_CLASS);
		if (traceApiClass == null) {
			traceApiClass = DEFAULT_TRACING_API_CLASS;
		}

		String externalActorClassName = props.getProperty(PROPERTY_ACTOR_CLASS);
		String ObjectFlowServiceClass = props.getProperty(PROPERTY_OBJECT_FLOW_CLASS);
		String encryptionServiceClass = props.getProperty(PROPERTY_ENCRYPTION_CLASS);

		String typePermissionServiceClass = props.getProperty(PROPERTY_TYPE_PERMISSION_CLASS);
		if (typePermissionServiceClass == null) {
			typePermissionServiceClass = DEFAULT_TYPE_PERMISSION_CLASS;
		}

		typePermissionFlowFactoryClassinitializeExtensions(typePermissionServiceClass);
		initializeTracingApiServiceFactory(traceApiClass);

		boolean mainCLassInitialize = initializeExtensions(DEFAULT_CLASS, repoStoreClassName, authStoreClassName,
				fileStorageClassName, cacheProviderClassName, externalActorClassName, intevalTime);
		if (mainCLassInitialize) {
			boolean encryptServicePermission = encryptionServiceClass != null
					? initializeEncryptionFactory(encryptionServiceClass)
					: true;
			boolean checkObjectServicePermission = ObjectFlowServiceClass != null
					? ObjectFlowFactoryClassinitializeExtensions(sce, ObjectFlowServiceClass)
					: true;
			if (checkObjectServicePermission && encryptServicePermission) {
				return true;
			}
		}
		return false;
	}

	private static boolean initializeExtensions(String className, String repoClassName, String authStoreClassName,
			String fileStorageClassName, String cacheProviderClassName, String externalActorClassName,
			long intervaltime) {
		LOG.info("Initialized External Services Factory Classes");
		boolean checkExternalActor = false;
		if (repoFactoryClassinitializeExtensions(className, repoClassName)) {
			if (authFactoryClassinitializeExtensions(authStoreClassName)) {
				if (fileStorageFactoryClassInit(fileStorageClassName)) {
					if (cacheProviderFactoryClassInit(cacheProviderClassName, intervaltime)) {
						checkExternalActor = externalActorClassName != null
								? externalActorFactoryClassinitializeExtensions(externalActorClassName)
								: true;
					}
				}
			}
		}
		return checkExternalActor;
	}

	private static boolean authFactoryClassinitializeExtensions(String authFactoryClassName) {
		try {
			String[] authFactoryClassNames = authFactoryClassName.split(",");
			for (String authClassName : authFactoryClassNames) {
				LOG.info("Initialized External Auth Services Factory Class: {}", authClassName);
				Class<?> authClassFactory = Class.forName(authClassName);
				IAuthFactory authFactory = (IAuthFactory) authClassFactory.newInstance();
				LoginAuthServiceFactory.add(authFactory);
			}
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e);
			return false;
		}
		return true;

	}

	private static boolean fileStorageFactoryClassInit(String fileStorageFactoryClassName) {
		try {
			String[] fileStorageFactoryClassNames = fileStorageFactoryClassName.split(",");
			for (String fileStorageClassName : fileStorageFactoryClassNames) {
				LOG.info("Initialized External Storage Services Factory Class: {}", fileStorageClassName);
				Class<?> storageClassFactory = Class.forName(fileStorageClassName);
				IStorageFactory fileFactory = (IStorageFactory) storageClassFactory.newInstance();
				StorageServiceFactory.add(fileFactory);
			}
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e);
			return false;
		}
		return true;

	}

	private static boolean cacheProviderFactoryClassInit(String cacheProviderFactoryClassName, long intervalTime) {
		try {
			LOG.info("Initialized Cache Provider Services Factory Class: {}", cacheProviderFactoryClassName);
			Class<?> c = Class.forName(cacheProviderFactoryClassName);
			ICacheProvider cacheProviderFactory = (ICacheProvider) c.newInstance();
			ICacheProvider UserCacheProviderFactory = (ICacheProvider) c.newInstance();
			CacheProviderServiceFactory.addTypeCacheService(cacheProviderFactory);
			CacheProviderServiceFactory.addUserCacheService(UserCacheProviderFactory);
			cacheProviderFactory.init(intervalTime);
			UserCacheProviderFactory.init(intervalTime);
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e);
			return false;
		}
		return true;
	}

	private static boolean externalActorFactoryClassinitializeExtensions(String externalActorClassName) {
		String[] externalActorFactoryClassNames = externalActorClassName.split(",");
		ActorServiceFactory.getInstance().setExternalActors(externalActorFactoryClassNames);
		return true;
	}

	private static boolean ObjectFlowFactoryClassinitializeExtensions(ServletContextEvent sce,
			String ObjectFlowServiceClassName) {
		try {
			String[] objectFlowFactoryClassNames = ObjectFlowServiceClassName.split(",");
			for (String objectFlowFactoryClassName : objectFlowFactoryClassNames) {
				Class<?> ObjectFlowServiceClassFactory = Class.forName(objectFlowFactoryClassName);
				IObjectFlowFactory ObjectFlowActorFactory = (IObjectFlowFactory) ObjectFlowServiceClassFactory
						.newInstance();
				ObjectFlowFactory.setObjectFlow(ObjectFlowActorFactory);
				LOG.info("Initialized Object Flow Services Factory Class: {}", ObjectFlowServiceClassName);
			}
			ObjectFlowFactory.setSystem((ActorSystem) sce.getServletContext().getAttribute("ActorSystem"));
		} catch (Exception e) {
			LOG.error("Could not create a ObjectFlowFactoryClass services factory instance: {}", e);
			return false;
		}
		return true;
	}

	private static void typePermissionFlowFactoryClassinitializeExtensions(String typePermissionServiceClassName) {
		try {

			LOG.info("Initialized Type Permission Flow Services Factory Class: {}", typePermissionServiceClassName);
			Class<?> typePermissionFlowServiceClassFactory = Class.forName(typePermissionServiceClassName);
			ITypePermissionFactory typePermissionFlowActorFactory = (ITypePermissionFactory) typePermissionFlowServiceClassFactory
					.newInstance();
			TypeServiceFactory.setTypePermissionFactory(typePermissionFlowActorFactory);
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e);

		}

	}

	private static boolean repoFactoryClassinitializeExtensions(String clientFactoryClassName, String repoClassName) {
		IRepositoryManager factory = null;
		IRepositoryStore repoStoreSetting = null;
		try {
			factory = (IRepositoryManager) Class.forName(clientFactoryClassName).newInstance();
			repoStoreSetting = (IRepositoryStore) Class.forName(repoClassName).newInstance();
		} catch (Exception e) {
			LOG.error("Could not create a repository services factory instance: {}", e);
			return false;
		}

		factory.init(repoStoreSetting);
		LOG.info("Initialized Services Factory: {}", factory.getClass().getName());
		return true;
	}

	private static void initializeTracingApiServiceFactory(String traceApiClass) {
		try {
			Class<?> traceApiServiceClass = Class.forName(traceApiClass);
			ITracingService apiService = (ITracingService) traceApiServiceClass.newInstance();
			TracingApiServiceFactory.add(apiService);
			LOG.info("Initialized Tracing Api Services Class: {}", traceApiClass);
		} catch (Exception e) {
			LOG.error("Could not create a Tracing Api services factory instance: {}", e);
		}
	}

	private static void initializeDBClientServiceFactory(String DBClientFactory) {
		try {
			Class<?> DBClientServiceFactory = Class.forName(DBClientFactory);
			IDBClientFactory apiService = (IDBClientFactory) DBClientServiceFactory.newInstance();
			DatabaseServiceFactory.add(apiService);
			LOG.info("Initialized CbmAdapter Services Class: {}", DBClientFactory);
		} catch (Exception e) {
			LOG.error("Could not create a CbmAdapter services factory instance: {}", e);
		}
	}

	private static boolean initializeEncryptionFactory(String encryptionServiceClassName) {
		try {
			LOG.info("Initialized Encryption Services Factory Class: {}", encryptionServiceClassName);
			Class<?> encryptionServiceClassFactory = Class.forName(encryptionServiceClassName);
			IObjectEncryptFactory encryptActorFactory = (IObjectEncryptFactory) encryptionServiceClassFactory
					.newInstance();
			EncryptionFactory.setEncryptFactory(encryptActorFactory);
		} catch (Exception e) {
			LOG.error("Could not create a encryption services factory instance: {}", e);
			return false;
		}
		return true;
	}
}