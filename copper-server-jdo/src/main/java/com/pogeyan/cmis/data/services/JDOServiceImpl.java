package com.pogeyan.cmis.data.services;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.jdo.JDOEnhancer;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;

import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.codehaus.groovy.control.CompilationUnit;
import org.codehaus.groovy.control.Phases;
import org.codehaus.groovy.tools.GroovyClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.pogeyan.cmis.data.jdo.JTypeDefinition;
import com.pogeyan.cmis.impl.utils.DBUtils;

import groovy.lang.GroovyClassLoader;

public class JDOServiceImpl {
	private static final Logger LOG = LoggerFactory.getLogger(JDOServiceImpl.class);
	private static JDOServiceImpl instance = new JDOServiceImpl();
	private PersistenceManager pm = null;
	private final String packageName = "com.pogeyan.cmis.data.jdo.";
	private Map<String, Cache<String, Class<?>>> repo = new HashMap<String, Cache<String, Class<?>>>();
	private Cache<String, GroovyClassLoader> classLoader = CacheBuilder.newBuilder()
			.expireAfterWrite(30 * 60, TimeUnit.SECONDS).build();

	public static JDOServiceImpl getInstance() {
		return instance;
	}

	public byte[] compileGroovyScript(String repositoryId, final String className, final String script,
			final GroovyClassLoader gcl, String folderlocaltion, String typeId, boolean base) {
		byte[] compiledScriptBytes = null;
		CompilationUnit compileUnit = new CompilationUnit(gcl);
		getParentCompileProp(repositoryId, className, typeId, folderlocaltion, compileUnit, base);
		compileUnit.addSource(className, script);
		compileUnit.compile(Phases.CLASS_GENERATION);
		for (Object compileClass : compileUnit.getClasses()) {
			GroovyClass groovyClass = (GroovyClass) compileClass;
			compiledScriptBytes = groovyClass.getBytes();
		}
		return compiledScriptBytes;
	}

	private void getParentCompileProp(String repositoryId, String className, String typeId, String folderLocation,
			CompilationUnit compileUnit, boolean base) {
		try {
			if (className.equalsIgnoreCase(packageName + "JDocumentObject")) {
				String templateString = getTxtFromFile(folderLocation, "JBaseObject", null);
				if (templateString == null) {
					templateString = HandleBarService.Impl.getTemplateString(repositoryId, FileNameType.BASEOBJECT,
							new HashMap<>());
				}
				compileUnit.addSource(packageName + "JBaseObject", templateString);
			}
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
					Arrays.asList(typeId));
			if (typeDef.size() > 0) {

				JTypeDefinition type = (JTypeDefinition) typeDef.get(0);
				if (type.getParentTypeId() != null) {
					List<? extends TypeDefinition> parentDef = DBUtils.TypeServiceDAO.getById(repositoryId,
							Arrays.asList(type.getParentTypeId()));
					if (parentDef.size() > 0) {
						JTypeDefinition pType = (JTypeDefinition) parentDef.get(0);
						String classparentName = com.pogeyan.cmis.data.services.JDOHelper.Impl
								.getJDOTypeId(type.getParentTypeId(), base);
						if (classparentName.equalsIgnoreCase("JDocumentObject")) {
							String templateString = getTxtFromFile(folderLocation, "JDocumentObject", null);
							if (templateString == null) {
								templateString = HandleBarService.Impl.getTemplateString(repositoryId,
										FileNameType.DOCUMENTOBJECT, new HashMap<>());
							}
							compileUnit.addSource(packageName + "JDocumentObject", templateString);
							String baseString = getTxtFromFile(folderLocation, "JBaseObject", null);
							if (baseString == null) {
								baseString = HandleBarService.Impl.getTemplateString(repositoryId,
										FileNameType.BASEOBJECT, new HashMap<>());
							}
							compileUnit.addSource(packageName + "JBaseObject", baseString);
						} else if (classparentName.equalsIgnoreCase("JBaseObject")) {
							String baseString = getTxtFromFile(folderLocation, "JBaseObject", null);
							if (baseString == null) {
								baseString = HandleBarService.Impl.getTemplateString(repositoryId,
										FileNameType.BASEOBJECT, new HashMap<>());
							}
							compileUnit.addSource(packageName + "JBaseObject", baseString);
						} else {
							Map<String, Object> map = com.pogeyan.cmis.data.services.JDOHelper.Impl.getClassMap(pType,
									base);
							String templateStrings = getTxtFromFile(folderLocation, classparentName,
									pType.getModifiedAt());
							if (templateStrings == null) {
								templateStrings = HandleBarService.Impl.getTemplateString(repositoryId,
										FileNameType.PROPERTYOBJECT, map);
							}
							compileUnit.addSource(packageName + classparentName, templateStrings);
						}

					}
				}
			}
		} catch (Exception e) {

		}

	}

	@SuppressWarnings({ "rawtypes", "unused", "resource" })
	public Class<?> getEnhancedClass(String repositoryId, String typeId, String className, FileNameType fileName,
			Map<String, Object> JBaseObjectClassMap, boolean baseObject, Long modifiedDate) {
		try {
			Class<?> enhancedClass = null;
			byte[] classBy = null;
			GroovyClassLoader groovyClassLoader = classLoader.getIfPresent(repositoryId);
			if (groovyClassLoader == null) {
				groovyClassLoader = new GroovyClassLoader(Thread.currentThread().getContextClassLoader());
				classLoader.put(repositoryId, groovyClassLoader);
			}
			File targetClassesDir = new File(
					JDOServiceImpl.class.getProtectionDomain().getCodeSource().getLocation().getPath());
			File targetDir = targetClassesDir.getParentFile();
			if (JBaseObjectClassMap.get("parentClassName") != null) {
				com.pogeyan.cmis.data.services.JDOHelper.Impl.load(repositoryId,
						JBaseObjectClassMap.get("parentClassName").toString(), baseObject);
			}
			byte[] classFileBytes = getBytesFromFile(targetDir.getAbsolutePath() + "\\classes\\", className,
					modifiedDate);
			Map<String, byte[]> getByteMap = null;
			if (classFileBytes == null) {
				String templateString = getTxtFromFile(targetDir.getAbsolutePath() + "\\classes\\", className,
						modifiedDate);
				if (templateString == null) {
					templateString = HandleBarService.Impl.getTemplateString(repositoryId, fileName,
							JBaseObjectClassMap);
					saveTemplateTxt(templateString, targetDir.getAbsolutePath() + "\\classes\\", className,
							modifiedDate);
				}
				classBy = compileGroovyScript(repositoryId, packageName + className, templateString, groovyClassLoader,
						targetDir.getAbsolutePath() + "\\classes\\", typeId, baseObject);

				Class cc = groovyClassLoader.parseClass(templateString);

				JDOEnhancer jdoEnhancer = JDOHelper.getEnhancer();
				jdoEnhancer.setVerbose(true);
				jdoEnhancer.setClassLoader(groovyClassLoader);
				jdoEnhancer.addClass(packageName + className, classBy);
				jdoEnhancer.enhance();
				Map<String, byte[]> mapByte = new HashMap<>();
				getByteMap = getByteMap(repositoryId, className, typeId, jdoEnhancer, mapByte, baseObject,
						targetDir.getAbsolutePath() + "\\classes\\", modifiedDate);
			} else {
				Map<String, byte[]> mapByte = new HashMap<>();
				getByteMap = getByteFileByteMap(repositoryId, className, typeId, classFileBytes, mapByte, baseObject,
						targetDir.getAbsolutePath() + "\\classes\\", modifiedDate);
			}

			ByteClassLoader byloader = new ByteClassLoader(getByteMap, groovyClassLoader);
			enhancedClass = byloader.loadClass(packageName + className);
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

	public void remove(String repositoryId, String key) {
		Cache<String, Class<?>> typeCacheMap = repo.get(repositoryId);
		if (typeCacheMap != null) {
			typeCacheMap.invalidate(key);
		}
	}

	public PersistenceManager initializePersistenceManager(String repositoryId) {
		if (pm == null) {
			PersistenceManagerFactory pmf = JDOHelper
					.getPersistenceManagerFactory(JDOManagerFactory.getProperties(repositoryId));
			pm = pmf.getPersistenceManager();
		}

		return pm;
	}

	public void closePersistenceManager(String repositoryId) {
		pm = null;

	}

	public void init() {
		JDOEnhancer jdoEnhancer = JDOHelper.getEnhancer();
		jdoEnhancer.setVerbose(true);
		jdoEnhancer.addClasses("com.pogeyan.cmis.data.jdo.JAclImpl", "com.pogeyan.cmis.data.jdo.JPermission",
				"com.pogeyan.cmis.data.jdo.JAceImpl", "com.pogeyan.cmis.data.jdo.JRelationship",
				"com.pogeyan.cmis.data.jdo.JDocumentTypeObject", "com.pogeyan.cmis.data.jdo.JChoiceImpl",
				"com.pogeyan.cmis.data.jdo.JPropertyDefinitionImpl", "com.pogeyan.cmis.data.jdo.JTokenImpl",
				"com.pogeyan.cmis.data.jdo.JTypeDefinition", "com.pogeyan.cmis.data.jdo.JTypeMutability");
		int enhancedClasses = jdoEnhancer.enhance();
		LOG.info("JDO enhance step completed with: {} types", enhancedClasses);
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private Map<String, byte[]> getByteMap(String repositoryId, String className, String typeId,
			JDOEnhancer jdoEnhancer, Map<String, byte[]> map, boolean baseObject, String folderLocation,
			Long modifiedDate) {
		try {
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
					Arrays.asList(typeId));
			if (typeDef == null || typeDef != null && typeDef.size() == 0) {
				return map;
			}
			JTypeDefinition type = (JTypeDefinition) typeDef.get(0);
			// String className =
			// com.pogeyan.cmis.data.services.JDOHelper.Impl.getJDOTypeId(typeId,
			// baseObject);
			String fileName = className;
			if (modifiedDate != null && modifiedDate > 0) {
				fileName = fileName + modifiedDate.toString();
			}
			saveEnhancedBytes(jdoEnhancer.getEnhancedBytes(packageName + className), folderLocation, fileName);
			map.put(packageName + className, jdoEnhancer.getEnhancedBytes(packageName + className));
			if (className.equalsIgnoreCase("JDocumentObject")) {
				byte[] parent = Files.readAllBytes(new File(folderLocation + "JBaseObject" + ".class").toPath());
				map.put(packageName + "JBaseObject", parent);
			}
			if (type.getParentTypeId() != null) {
				getParentClassBytes(repositoryId, type.getParentTypeId(), map, baseObject, folderLocation,
						modifiedDate);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return map;
	}

	private Map<String, byte[]> getByteFileByteMap(String repositoryId, String className, String typeId,
			byte[] classByte, Map<String, byte[]> map, boolean baseObject, String folderLocation, Long modifiedDate) {
		try {
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
					Arrays.asList(typeId));
			if (typeDef == null || typeDef != null && typeDef.size() == 0) {
				return map;
			}
			JTypeDefinition type = (JTypeDefinition) typeDef.get(0);
			// String className =
			// com.pogeyan.cmis.data.services.JDOHelper.Impl.getJDOTypeId(typeId,
			// baseObject);
			String fileName = className;
			if (modifiedDate != null && modifiedDate > 0) {
				fileName = fileName + modifiedDate.toString();
			}
			map.put(packageName + className, classByte);
			if (className.equalsIgnoreCase("JDocumentObject")) {
				byte[] parent = Files.readAllBytes(new File(folderLocation + "JBaseObject" + ".class").toPath());
				map.put(packageName + "JBaseObject", parent);
			}
			if (type.getParentTypeId() != null) {
				getParentClassBytes(repositoryId, type.getParentTypeId(), map, baseObject, folderLocation,
						modifiedDate);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return map;
	}

	private Map<String, byte[]> getParentClassBytes(String repositoryId, String typeId, Map<String, byte[]> map,
			boolean baseObject, String folderLocation, Long modifiedDate) {
		try {
			List<? extends TypeDefinition> typeDef = DBUtils.TypeServiceDAO.getById(repositoryId,
					Arrays.asList(typeId));
			if (typeDef == null || typeDef != null && typeDef.size() == 0) {
				return map;
			}
			JTypeDefinition type = (JTypeDefinition) typeDef.get(0);
			String className = com.pogeyan.cmis.data.services.JDOHelper.Impl.getJDOTypeId(typeId, baseObject);
			if (className.equalsIgnoreCase("JDocumentObject")) {
				byte[] parent = Files.readAllBytes(new File(folderLocation + "JBaseObject" + ".class").toPath());
				byte[] docParent = Files.readAllBytes(new File(folderLocation + "JDocumentObject" + ".class").toPath());
				map.put(packageName + "JBaseObject", parent);
				map.put(packageName + "JDocumentObject", docParent);
			}
			if (className.equalsIgnoreCase("JBaseObject")) {
				byte[] parent = Files.readAllBytes(new File(folderLocation + "JBaseObject" + ".class").toPath());
				map.put(packageName + "JBaseObject", parent);
			}
			if (type.getParentTypeId() != null) {
				String fileName = className;
				if (type.getModifiedAt() != null && type.getModifiedAt() > 0) {
					fileName = className + type.getModifiedAt().toString();
				}
				byte[] parent = Files.readAllBytes(new File(folderLocation + fileName + ".class").toPath());
				map.put(packageName + className, parent);
				getParentClassBytes(repositoryId, type.getParentTypeId(), map, baseObject, folderLocation,
						modifiedDate);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return map;
	}

	private static void saveEnhancedBytes(byte[] enhancer, String folder, String fileName) {
		FileOutputStream out = null;
		try {
			File enhancedFile = new File(folder + fileName + ".class");
			if (!enhancedFile.exists()) {
				enhancedFile.createNewFile();
			}
			out = new FileOutputStream(enhancedFile);
			out.write(enhancer);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (out != null) {
				try {
					out.close();
					out = null;
				} catch (Exception ignore) {

				}
			}
		}
	}

	private static void saveTemplateTxt(String templateString, String folder, String fileName, Long modifiedDate) {
		PrintStream out = null;
		try {
			fileName = fileName + "template";
			if (modifiedDate != null && modifiedDate > 0) {
				fileName = fileName + modifiedDate.toString();
			}
			File enhancedFile = new File(folder + fileName + ".txt");
			if (!enhancedFile.exists()) {
				enhancedFile.createNewFile();
			}
			out = new PrintStream(enhancedFile);
			out.print(templateString);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (out != null) {
				try {
					out.close();
					out = null;
				} catch (Exception ignore) {

				}
			}
		}
	}

	private static byte[] getBytesFromFile(String folderLocation, String className, Long modifiedDate) {
		byte[] classBytes = null;
		try {
			if (modifiedDate != null && modifiedDate > 0) {
				className = className + modifiedDate.toString();
			}
			return classBytes = Files.readAllBytes(new File(folderLocation + className + ".class").toPath());
		} catch (Exception e) {
			return classBytes;
		}
	}

	private static String getTxtFromFile(String folderLocation, String className, Long modifiedDate) {
		String templateString = null;
		try {
			className = className + "template";
			if (modifiedDate != null && modifiedDate > 0) {
				className = className + modifiedDate.toString();
			}
			return templateString = new String(
					Files.readAllBytes(new File(folderLocation + className + ".txt").toPath()));

		} catch (Exception e) {
			return templateString;
		}
	}

}