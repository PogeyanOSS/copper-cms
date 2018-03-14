package com.pogeyan.cmis.data.services;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.jdo.JDOEnhancer;
import javax.jdo.JDOException;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;

import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.codehaus.groovy.control.CompilationUnit;
import org.codehaus.groovy.control.Phases;
import org.codehaus.groovy.tools.GroovyClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

import groovy.lang.GroovyClassLoader;

public class JDOServiceImpl {
	private static final Logger LOG = LoggerFactory.getLogger(JDOServiceImpl.class);
	private static JDOServiceImpl instance = new JDOServiceImpl();
	private GroovyClassLoader gcl = null;
	private PersistenceManager pm = null;
	private final String packageName = "com.pogeyan.cmis.data.jdo.";
	private Map<String, Cache<String, Class<?>>> repo = new HashMap<String, Cache<String, Class<?>>>();

	public static JDOServiceImpl getInstance() {
		return instance;
	}

	public Class<?> load(String repositoryId, String className, String fileName,
			Map<String, Object> JBaseObjectClassMap) {
		Class<?> enhancedClass = getCacheMapValue(repositoryId, packageName + className);
		if (enhancedClass != null) {
			return enhancedClass;
		} else {
			return getEnhancedClass(repositoryId, className, fileName, JBaseObjectClassMap);
		}
	}

	@SuppressWarnings("resource")
	public Class<?> getEnhanceClass(String repositoryId, String className) {
		try {
			Class<?> enhancedClass = getCacheMapValue(repositoryId, packageName + className);
			if (enhancedClass != null) {
				return enhancedClass;
			} else {
				JDOEnhancer jdoEnhancer = applyEnhancer(null, null);
				byte[] classByte = jdoEnhancer.getEnhancedBytes(packageName + className);
				if (classByte != null) {
					ByteClassLoader byloader = new ByteClassLoader(packageName + className, jdoEnhancer.getEnhancedBytes(packageName + className),
							gcl);
					enhancedClass = byloader.loadClass(packageName + className);
					putCacheMap(repositoryId, packageName + className, enhancedClass);
					return enhancedClass;
				}
			}
		} catch (JDOException e) {
			return null;
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
		return null;

	}

	public byte[] compileGroovyScript(final String className, final String script, final GroovyClassLoader gcl) {
		byte[] compiledScriptBytes = null;
		CompilationUnit compileUnit = new CompilationUnit(gcl);
		compileUnit.addSource(className, script);
		compileUnit.compile(Phases.CLASS_GENERATION);
		for (Object compileClass : compileUnit.getClasses()) {
			GroovyClass groovyClass = (GroovyClass) compileClass;
			compiledScriptBytes = groovyClass.getBytes();
		}
		return compiledScriptBytes;
	}

	@SuppressWarnings({ "rawtypes", "unused", "resource" })
	public Class<?> getEnhancedClass(String repositoryId, String className, String fileName,
			Map<String, Object> JBaseObjectClassMap) {
		try {
			String templateString = HandleBarService.Impl.getTemplateString(repositoryId, fileName,
					JBaseObjectClassMap);
			byte[] classBy = compileGroovyScript(packageName + className, templateString, getGroovyClassLoader());
			Class cc = getGroovyClassLoader().parseClass(templateString);
			JDOEnhancer jdoEnhancer = this.applyEnhancer(packageName + className, classBy);
			ByteClassLoader byloader = new ByteClassLoader(packageName + className, jdoEnhancer.getEnhancedBytes(packageName + className), gcl);
			Class<?> enhancedClass = byloader.loadClass(packageName + className);
			putCacheMap(repositoryId, packageName + className, enhancedClass);
			return enhancedClass;
		} catch (Exception e) {
			LOG.error("getEnhancedClass Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			throw new CmisInvalidArgumentException(e.toString());
		}
	}

	public <T> void putCacheMap(String repositoryId, String key, Class<?> object) {
		Cache<String, Class<?>> repoCacheMap = repo.get(repositoryId);
		if (repoCacheMap != null) {
			repoCacheMap.put(key, object);
		} else {
			Cache<String, Class<?>> typeCacheMap = CacheBuilder.newBuilder().expireAfterWrite(30 * 60, TimeUnit.SECONDS)
					.build();
			typeCacheMap.put(key, object);
			repo.put(repositoryId, typeCacheMap);
		}
	}

	public Class<?> getCacheMapValue(String repositoryId, String className) {
		Cache<String, Class<?>> typeCacheMap = repo.get(repositoryId);
		if (typeCacheMap != null) {
			return typeCacheMap.getIfPresent(className);
		}
		return null;
	}

	private JDOEnhancer applyEnhancer(String packName, byte[] classByte) {
		JDOEnhancer jdoEnhancer = JDOHelper.getEnhancer();
		jdoEnhancer.setVerbose(true);
		jdoEnhancer.setClassLoader(getGroovyClassLoader());
		if (packName != null && classByte != null) {
			jdoEnhancer.addClass(packName, classByte);
		}
		jdoEnhancer.enhance();
		return jdoEnhancer;
	}

	public PersistenceManager initializePersistenceManager(String repositoryId) {
		if (pm == null) {
			PersistenceManagerFactory pmf = JDOHelper
					.getPersistenceManagerFactory(JDOManagerFactory.getProperties(repositoryId));
			pm = pmf.getPersistenceManager();
		}

		return pm;
	}

	public GroovyClassLoader getGroovyClassLoader() {
		if (gcl == null) {
			gcl = new GroovyClassLoader(Thread.currentThread().getContextClassLoader());
		}
		return gcl;
	}

	public void init() {
		JDOEnhancer jdoEnhancer = JDOHelper.getEnhancer();
		jdoEnhancer.setVerbose(true);
		jdoEnhancer.addClasses("com.pogeyan.cmis.data.jdo.JAcleImpl", "com.pogeyan.cmis.data.jdo.JAclImpl",
				"com.pogeyan.cmis.data.jdo.JDocumentTypeObject", "com.pogeyan.cmis.data.jdo.JPropertyDefinitionImpl",
				"com.pogeyan.cmis.data.jdo.JTokenImpl", "com.pogeyan.cmis.data.jdo.JTypeDefinition",
				"com.pogeyan.cmis.data.jdo.JTypeMutability", "com.pogeyan.cmis.data.jdo.JTypeObject");
		int enhancedClasses = jdoEnhancer.enhance();
		LOG.info("JDO enhance step completed with: {} types", enhancedClasses);
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}