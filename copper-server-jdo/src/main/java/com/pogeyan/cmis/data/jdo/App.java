package com.pogeyan.cmis.data.jdo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.jdo.JDOEnhancer;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.jdo.Query;
import javax.jdo.Transaction;

public class App {
	@SuppressWarnings("unchecked")
	public static void main(String args[]) {
		JDOEnhancer enhancer = JDOHelper.getEnhancer();
		enhancer.setVerbose(true);
		enhancer.setClassLoader(Thread.currentThread().getContextClassLoader());
		enhancer.addPersistenceUnit("TypeDef");
		enhancer.enhance();
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Properties properties = new Properties();
		properties.setProperty("javax.jdo.PersistenceManagerFactoryClass",
				"org.datanucleus.api.jdo.JDOPersistenceManagerFactory");
		properties.setProperty("javax.jdo.option.ConnectionDriverName", "com.mysql.jdbc.Driver");
		properties.setProperty("javax.jdo.option.ConnectionURL", "jdbc:mysql://localhost:3306/cmis");
		properties.setProperty("javax.jdo.option.ConnectionUserName", "root");
		properties.setProperty("javax.jdo.option.ConnectionPassword", "");
		properties.setProperty("datanucleus.schema.autoCreateTables", "true");

		PersistenceManagerFactory pmf = JDOHelper.getPersistenceManagerFactory(properties);
		PersistenceManager pm = pmf.getPersistenceManager();

		Transaction tx = pm.currentTransaction();
		try {
			String p = "azar";
			List<String>v=new ArrayList<>();
			v.add("cmis:folder");
			v.add("cmis:document");
			
//			Query q = pm.newQuery("SELECT FROM com.pogeyan.cmis.data.jdo.JTypeObject "
//					+ "WHERE this.typeId == typeDef.typeID && typeDef.parent == '" + p
//					+ "' || this.typeId == docDef.id && docDef.parent == '" + p
//					+ "' VARIABLES com.pogeyan.cmis.data.jdo.JTypeDefinition typeDef; com.pogeyan.cmis.data.jdo.JDocumentTypeObject docDef;");
//			List<JTypeObject> typeObject = (List<JTypeObject>) q.execute();
//			for (JTypeObject pp : typeObject) {
//				System.out.println(pp.getTypeId());
//			}
//			Query q = pm.newQuery("SELECT FROM com.pogeyan.cmis.data.jdo.JTypeObject "
//					+ "WHERE typeId == ids");
//			q.declareParameters("java.util.List ids");
//			String sql="select from com.pogeyan.cmis.data.jdo.JTypeObject where typeId in(:ids)";
			Query query = pm.newQuery(JTypeDefinition.class);
			query.declareParameters("java.util.List docId");
			query.setFilter("typeId  docId");
			Map<String, List<String>> paramValues = new HashMap<>();
			paramValues.put("docId", v);
			List<JTypeDefinition> typeObject = (List<JTypeDefinition>) query.executeWithMap(paramValues);
//			List<JTypeObject> result = new ArrayList<>();
//			Map idsMap = Collections.singletonMap("ids", v);
//			result = jdbcTemplate.query(sql, idsMap, ParameterizedBeanPropertyRowMapper.newInstance(JTypeObject.class));
			//List<JTypeObject> typeObject = (List<JTypeObject>) q.executeWithArray(v);
			System.out.println(typeObject);
		} finally {
			if (tx.isActive()) {
				tx.rollback();
			}
			pm.close();
		}
	}
}
