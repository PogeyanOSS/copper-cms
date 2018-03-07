package com.pogeyan.cmis.data.services;

import java.util.HashMap;
import java.util.Map;
import javax.jdo.JDOEnhancer;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;

import org.codehaus.groovy.control.CompilationUnit;
import org.codehaus.groovy.control.Phases;
import org.codehaus.groovy.tools.GroovyClass;

import com.pogeyan.cmis.impl.factory.DatabaseServiceFactory;

import groovy.lang.GroovyClassLoader;

public class JDOConnection {
	private static JDOConnection instance = new JDOConnection();
	private Map<String, byte[]> classDefs = new HashMap<String, byte[]>();
	private JDOEnhancer jdoEnhancer = null;;
	private GroovyClassLoader gcl = null;
	private ByteClassLoader byteClassLoader = null;
	private PersistenceManager pm = null;

	public PersistenceManager initializePersistenceManager(String repositoryId) {
		if (pm == null) {
			PersistenceManagerFactory pmf = JDOHelper.getPersistenceManagerFactory(
					DatabaseServiceFactory.getInstance(repositoryId).getProperties(repositoryId));
			pm = pmf.getPersistenceManager();
		}
		return pm;
	}

	public Map<String, byte[]> setEnhancer(Map<String, byte[]> classObject, GroovyClassLoader gcl) {
		JDOEnhancer enhancer = JDOHelper.getEnhancer();
		enhancer.setVerbose(true);
		enhancer.setClassLoader(gcl);
		classObject.forEach((k, v) -> {
			enhancer.addClass(k, v);
		});
		enhancer.enhance();

		classObject.forEach((k, v) -> {
			if (classDefs.get(k) != null) {
				classDefs.remove(k);
			}
			classDefs.put(k, enhancer.getEnhancedBytes(k));
		});
		return classDefs;
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

	public GroovyClassLoader getGroovyClassLoader() {
		if (gcl == null) {
			gcl = new GroovyClassLoader(Thread.currentThread().getContextClassLoader());
		}
		return gcl;
	}

	public JDOEnhancer compileEnhancer() {
		if (jdoEnhancer == null) {
			jdoEnhancer = JDOHelper.getEnhancer();
			jdoEnhancer.setVerbose(true);
			jdoEnhancer.setClassLoader(getGroovyClassLoader());
			jdoEnhancer.addPersistenceUnit("TypeDef");
			jdoEnhancer.enhance();
			try {
				Thread.sleep(2000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		return jdoEnhancer;
	}

	public static JDOConnection get() {
		return instance;
	}

	public ByteClassLoader getByteClassLoader() {
		return byteClassLoader;
	}

	public void setByteClassLoader(Map<String, byte[]> classByte) {
		this.byteClassLoader = new ByteClassLoader(classByte);
	}
}
