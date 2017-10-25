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
import com.pogeyan.cmis.actors.AclActor;
import com.pogeyan.cmis.actors.DiscoveryActor;
import com.pogeyan.cmis.actors.NavigationActor;
import com.pogeyan.cmis.actors.ObjectActor;
import com.pogeyan.cmis.actors.PolicyActor;
import com.pogeyan.cmis.actors.RelationshipActor;
import com.pogeyan.cmis.actors.RepositoryActor;
import com.pogeyan.cmis.actors.VersioningActor;
import com.pogeyan.cmis.api.IActorService;
import com.pogeyan.cmis.api.auth.IAuthFactory;
import com.pogeyan.cmis.api.repo.IRepositoryManager;
import com.pogeyan.cmis.api.repo.IRepositoryStore;
import com.pogeyan.cmis.api.storage.IStorageFactory;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
import com.pogeyan.cmis.auth.LoginActor;
import com.pogeyan.cmis.server.GatewayActor;
import com.pogeyan.cmis.service.factory.LoginAuthServiceFactory;
import com.pogeyan.cmis.service.factory.StorageServiceFactory;

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
	private static final String DEFAULT_CLASS = "com.pogeyan.cmis.factory.RepositoryManagerFactory";
	private static final String DEFAULT_REPO_STORE_CLASS = "com.pogeyan.cmis.repo.MongoDBRepositoryStore";
	private static final String DEFAULT_AUTH_STORE_CLASS = "com.pogeyan.cmis.ldap.auth.LDAPAuthFactory";
	private static final String DEFAULT_FILE_STORE_CLASS = "com.pogeyan.cmis.factory.StorageManagerFactory";
	private static Map<Class<?>, String> externalActorClassMap = new HashMap<Class<?>, String>();

	static final Logger LOG = LoggerFactory.getLogger(AkkaServletContextListener.class);

	@Override
	public void contextInitialized(ServletContextEvent sce) {
		ActorSystem system = ActorSystem.create("GatewaySystem");
		sce.getServletContext().setAttribute("ActorSystem", system);

		String configFilename = sce.getServletContext().getInitParameter(CONFIG_INIT_PARAM);
		if (configFilename == null) {
			configFilename = CONFIG_FILENAME;
		}
		try {
			boolean factory = createServiceFactory(sce, configFilename);
			if (!factory) {
				throw new IllegalArgumentException("Repository manager class not initilaized");
			}
		} catch (Exception e) {
			LOG.error("Service factory couldn't be created: {}", e.toString(), e);
		}

		system.actorOf(Props.create(GatewayActor.class), "gateway");
		system.actorOf(Props.create(LoginActor.class), "login");
		system.actorOf(Props.create(RepositoryActor.class), "repository");
		system.actorOf(Props.create(ObjectActor.class), "object");
		system.actorOf(Props.create(NavigationActor.class), "navigation");
		system.actorOf(Props.create(RelationshipActor.class), "relationships");
		system.actorOf(Props.create(PolicyActor.class), "policy");
		system.actorOf(Props.create(VersioningActor.class), "versioning");
		system.actorOf(Props.create(AclActor.class), "acl");
		system.actorOf(Props.create(DiscoveryActor.class), "discovery");
		if (externalActorClassMap != null && !externalActorClassMap.isEmpty()) {
			externalActorClassMap.forEach((key, value) -> system.actorOf(Props.create(key), value));
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
			return initializeExtensions(DEFAULT_CLASS, DEFAULT_REPO_STORE_CLASS, DEFAULT_AUTH_STORE_CLASS,
					DEFAULT_FILE_STORE_CLASS, null);
		}

		Properties props = new Properties();
		try {
			props.load(stream);
		} catch (IOException e) {
			LOG.error("Cannot load configuration: {}", e.toString(), e);
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
		String externalActorClassName = props.getProperty(PROPERTY_ACTOR_CLASS);

		return initializeExtensions(DEFAULT_CLASS, repoStoreClassName, authStoreClassName, fileStorageClassName,
				externalActorClassName);
	}

	private static boolean initializeExtensions(String className, String repoClassName, String authStoreClassName,
			String fileStorageClassName, String externalActorClassName) {
		LOG.info("Initialized External Services Factory Classes");
		if (repoFactoryClassinitializeExtensions(className, repoClassName)) {
			if (authFactoryClassinitializeExtensions(authStoreClassName)) {
				if (fileStorageFactoryClassInit(fileStorageClassName)) {
					if (externalActorClassName != null) {
						if (externalActorFactoryClassinitializeExtensions(externalActorClassName)) {
							return true;
						}
					} else {
						return true;
					}
				}
			}
		}

		return false;
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
			LOG.error("Could not create a authentication services factory instance: {}", e.toString(), e);
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
			LOG.error("Could not create a authentication services factory instance: {}", e.toString(), e);
			return false;
		}
		return true;

	}

	private static boolean externalActorFactoryClassinitializeExtensions(String externalActorClassName) {
		try {
			String[] externalActorFactoryClassNames = externalActorClassName.split(",");
			for (String externalActorFactoryClassName : externalActorFactoryClassNames) {
				LOG.info("Initialized External Actor Services Factory Class: {}", externalActorFactoryClassName);
				Class<?> externalActorClassFactory = Class.forName(externalActorFactoryClassName);
				IActorService externalActorFactory = (IActorService) externalActorClassFactory.newInstance();
				externalActorClassMap.put(externalActorFactory.getActorClass(), externalActorFactory.getServiceURL());
			}
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e.toString(), e);
			return false;
		}
		return true;

	}

	private static boolean repoFactoryClassinitializeExtensions(String clientFactoryClassName, String repoClassName) {
		IRepositoryManager factory = null;
		IRepositoryStore repoStoreSetting = null;
		try {
			factory = (IRepositoryManager) Class.forName(clientFactoryClassName).newInstance();
			repoStoreSetting = (IRepositoryStore) Class.forName(repoClassName).newInstance();
		} catch (Exception e) {
			LOG.error("Could not create a repository services factory instance: {}", e.toString(), e);
			return false;
		}

		factory.init(repoStoreSetting);
		LOG.info("Initialized Services Factory: {}", factory.getClass().getName());
		return true;
	}
}