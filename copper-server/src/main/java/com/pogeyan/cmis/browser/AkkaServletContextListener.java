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
import com.pogeyan.cmis.actors.TypeCacheActor;
import com.pogeyan.cmis.actors.VersioningActor;
import com.pogeyan.cmis.api.IActorService;
import com.pogeyan.cmis.api.auth.IAuthFactory;
import com.pogeyan.cmis.api.data.ICacheProvider;
import com.pogeyan.cmis.api.data.IObjectFlowFactory;
import com.pogeyan.cmis.api.data.ITracingFacade;
import com.pogeyan.cmis.api.data.ITypePermissionFactory;
import com.pogeyan.cmis.api.repo.IRepositoryManager;
import com.pogeyan.cmis.api.repo.IRepositoryStore;
import com.pogeyan.cmis.api.storage.IStorageFactory;
import com.pogeyan.cmis.api.utils.Helpers;
import com.pogeyan.cmis.api.utils.MetricsInputs;
import com.pogeyan.cmis.auth.LoginActor;
import com.pogeyan.cmis.data.mongo.services.MongoClientFactory;
import com.pogeyan.cmis.impl.factory.CacheProviderServiceFactory;
import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;
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
	private static final String PROPERTY_TYPE_PERMISSION_CLASS = "typePermissionManagerClass";
	private static final String PROPERTY_INTERVAL_TIME = "intervalTime";
	private static final String DEFAULT_CLASS = "com.pogeyan.cmis.api.repo.RepositoryManagerFactory";
	private static final String DEFAULT_REPO_STORE_CLASS = "com.pogeyan.cmis.repo.MongoDBRepositoryStore";
	private static final String DEFAULT_AUTH_STORE_CLASS = "com.pogeyan.cmis.repo.local.LocalRepoAuthFactory";
	private static final String DEFAULT_FILE_STORE_CLASS = "com.pogeyan.cmis.impl.storage.FileSystemStorageFactory";
	private static final String DEFAULT_CACHE_PROVIDER_CLASS = "com.pogeyan.cmis.impl.cacheProvider.GoogleGuiceCacheProviderImpl";
	private static Map<Class<?>, String> externalActorClassMap = new HashMap<Class<?>, String>();
	private static final String DEFAULT_TRACING_API_CLASS = "com.pogeyan.cmis.tracing.TracingDefaultImpl";
	private static final String PROPERTY_TRACING_API_CLASS = "tracingApiClass";

	static final Logger LOG = LoggerFactory.getLogger(AkkaServletContextListener.class);

	@Override
	public void contextInitialized(ServletContextEvent sce) {
		ActorSystem system = ActorSystem.create("GatewaySystem");
		sce.getServletContext().setAttribute("ActorSystem", system);

		String configFilename = sce.getServletContext().getInitParameter(CONFIG_INIT_PARAM);
		if (configFilename == null) {
			configFilename = CONFIG_FILENAME;
		}

		DatabaseServiceFactory.add(MongoClientFactory.createDatabaseService());

		LOG.info("Registering actors to main actor system");
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
		system.actorOf(Props.create(TypeCacheActor.class), "cache");

		LOG.info("Initializing service factory instances");
		try {
			boolean factory = createServiceFactory(sce, configFilename);
			if (!factory) {
				throw new IllegalArgumentException("Repository manager class not initilaized");
			}
		} catch (Exception e) {
			LOG.error("Service factory couldn't be created: {}", e.toString(), e);
		}
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
			LOG.warn("REPOSITORY_PROPERTY_FILE_LOCATION is not found due to: {}", e.getMessage());
			LOG.info("Loading default extensions");
			return initializeExtensions(DEFAULT_CLASS, DEFAULT_REPO_STORE_CLASS, DEFAULT_AUTH_STORE_CLASS,
					DEFAULT_FILE_STORE_CLASS, DEFAULT_CACHE_PROVIDER_CLASS, null, null, null, 30 * 60,
					DEFAULT_TRACING_API_CLASS);
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
		String typePermissionServiceClass = props.getProperty(PROPERTY_TYPE_PERMISSION_CLASS);

		boolean mainCLassInitialize = initializeExtensions(DEFAULT_CLASS, repoStoreClassName, authStoreClassName,
				fileStorageClassName, cacheProviderClassName, externalActorClassName, ObjectFlowServiceClass,
				typePermissionServiceClass, intevalTime, traceApiClass);
		if (mainCLassInitialize) {
			if (typePermissionServiceClass != null) {
				if (typePermissionFlowFactoryClassinitializeExtensions(typePermissionServiceClass)) {
					return true;
				}
			}
			return true;
		}
		return false;
	}

	private static boolean initializeExtensions(String className, String repoClassName, String authStoreClassName,
			String fileStorageClassName, String cacheProviderClassName, String externalActorClassName,
			String ObjectFlowServiceClass, String typePermissionServiceClass, long intervaltime, String traceApiClass) {
		LOG.info("Initialized External Services Factory Classes");
		if (repoFactoryClassinitializeExtensions(className, repoClassName)) {
			if (authFactoryClassinitializeExtensions(authStoreClassName)) {
				if (fileStorageFactoryClassInit(fileStorageClassName)) {
					if (cacheProviderFactoryClassInit(cacheProviderClassName, intervaltime)) {
						if (initializeTracingApiServiceFactory(traceApiClass)) {
							TracingApiServiceFactory.getApiService().registerJaegarService();
							if (externalActorClassName != null) {
								if (externalActorFactoryClassinitializeExtensions(externalActorClassName)) {
									if (ObjectFlowServiceClass != null) {
										if (ObjectFlowFactoryClassinitializeExtensions(ObjectFlowServiceClass)) {
											return true;
										}
									} else {
										return true;
									}
								}
							} else if (ObjectFlowServiceClass != null) {
								if (ObjectFlowFactoryClassinitializeExtensions(ObjectFlowServiceClass)) {
									return true;
								}
							} else {
								return true;
							}
						}
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
			CacheProviderServiceFactory.addTypeCacheService(cacheProviderFactory);
			cacheProviderFactory.init(intervalTime);
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e);
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
			LOG.error("Could not create a authentication services factory instance: {}", e);
			return false;
		}
		return true;

	}

	private static boolean ObjectFlowFactoryClassinitializeExtensions(String ObjectFlowServiceClassName) {
		try {
			LOG.info("Initialized Object Flow Services Factory Class: {}", ObjectFlowServiceClassName);
			Class<?> ObjectFlowServiceClassFactory = Class.forName(ObjectFlowServiceClassName);
			IObjectFlowFactory ObjectFlowActorFactory = (IObjectFlowFactory) ObjectFlowServiceClassFactory
					.newInstance();
			ObjectFlowFactory.setObjectFlow(ObjectFlowActorFactory);
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e);
			return false;
		}
		return true;

	}

	private static boolean typePermissionFlowFactoryClassinitializeExtensions(String typePermissionServiceClassName) {
		try {

			LOG.info("Initialized Type Permission Flow Services Factory Class: {}", typePermissionServiceClassName);
			Class<?> typePermissionFlowServiceClassFactory = Class.forName(typePermissionServiceClassName);
			ITypePermissionFactory typePermissionFlowActorFactory = (ITypePermissionFactory) typePermissionFlowServiceClassFactory
					.newInstance();
			TypeServiceFactory.setTypePermissionFactory(typePermissionFlowActorFactory);
		} catch (Exception e) {
			LOG.error("Could not create a authentication services factory instance: {}", e);
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
			LOG.error("Could not create a repository services factory instance: {}", e);
			return false;
		}

		factory.init(repoStoreSetting);
		LOG.info("Initialized Services Factory: {}", factory.getClass().getName());
		return true;
	}

	private static boolean initializeTracingApiServiceFactory(String traceApiClass) {
		try {
			Class<?> traceApiServiceClass = Class.forName(traceApiClass);
			ITracingFacade apiService = (ITracingFacade) traceApiServiceClass.newInstance();
			TracingApiServiceFactory.add(apiService);
			LOG.info("Initialized Tracing Api Services Class: {}", traceApiClass);
		} catch (Exception e) {
			LOG.error("Could not create a Tracing Api services factory instance: {}", e);
			return false;
		}
		return true;
	}
}