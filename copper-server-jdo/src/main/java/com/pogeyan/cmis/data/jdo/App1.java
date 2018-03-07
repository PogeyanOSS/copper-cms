package com.pogeyan.cmis.data.jdo;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.enums.PropertyType;

import com.pogeyan.cmis.api.data.common.PropertyDefinitionImpl;
import com.pogeyan.cmis.api.data.common.TypeDefinitionImpl;

import groovy.lang.GroovyClassLoader;
import groovy.lang.Writable;
import groovy.text.SimpleTemplateEngine;
import groovy.text.Template;

public class App1 {

	static String readFile(String path, Charset encoding) throws IOException {
		byte[] encoded = Files.readAllBytes(Paths.get(path));
		return new String(encoded, encoding);
	}

	public static <T> void main(String args[]) {

		TypeDefinitionImpl ff = new TypeDefinitionImpl();
		ff.setId("order");
		PropertyDefinitionImpl<T> p = new PropertyDefinitionImpl<T>();
		p.setId("orderID");
		p.setPropertyType(PropertyType.STRING);
		PropertyDefinitionImpl<T> p1 = new PropertyDefinitionImpl<T>();
		p1.setId("orderDetails");
		p1.setPropertyType(PropertyType.BOOLEAN);
		PropertyDefinitionImpl<T> p2 = new PropertyDefinitionImpl<T>();
		p2.setId("customerID");
		p2.setPropertyType(PropertyType.INTEGER);
		Map<String, PropertyDefinitionImpl<?>> map = new HashMap<>();
		map.put(p.getId(), p);
		map.put(p1.getId(), p1);
		map.put(p2.getId(), p2);
		ff.setPropertyDefinition(map);
		Map<String, Object> maps = new HashMap<>();
		maps.put("order", "String");
		maps.put("orderId", "String");
		Map<String, Object> maps1 = new HashMap<>();
		maps1.put("id", "orderID");
		maps1.put("propertyType", "String");
		ClassLoader classLoader = App1.class.getClassLoader();
		File file = new File(classLoader.getResource("JProperties.txt").getFile());
		SimpleTemplateEngine engine = new groovy.text.SimpleTemplateEngine();

		try {
			String templateText = readFile(file.getPath(), Charset.defaultCharset());
			Template template = engine.createTemplate(templateText);
			Map<String, Object> maprr = new HashMap<>();
			maprr.put("PropClassName", ff.getId());
			maprr.put("propertyDef", maps);
			maprr.put("primaryKey", "order");
			Writable write = template.make(maprr);

			GroovyClassLoader gcl = new GroovyClassLoader(Thread.currentThread().getContextClassLoader());

			// parse class in GCL so GCL can have a cached instance of the Class definition
			@SuppressWarnings("rawtypes")
			Class cc = gcl.parseClass(write.toString());
			System.out.println(cc.getName());

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
