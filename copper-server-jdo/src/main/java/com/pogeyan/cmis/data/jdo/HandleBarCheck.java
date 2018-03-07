package com.pogeyan.cmis.data.jdo;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.jknack.handlebars.Context;
import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;

public class HandleBarCheck {
	static String readFile(String path, Charset encoding) throws IOException {
		byte[] encoded = Files.readAllBytes(Paths.get(path));
		return new String(encoded, encoding);
	}

	public static void main(String args[]) {
		try {
			Handlebars handlebars = new Handlebars();
			ClassLoader classLoader = HandleBarCheck.class.getClassLoader();
			Map<String, Object> map1 = new HashMap<>();
			map1.put("id", "order");
			map1.put("property", "String");
			map1.put("primary", true);
			Map<String, Object> map2 = new HashMap<>();
			map2.put("id", "orderId");
			map2.put("property", "String");
			map2.put("primary", false);
			List<Map<String, Object>> propf = new ArrayList<>();
			propf.add(map1);
			propf.add(map2);
			Map<String, Object> map = new HashMap<>();
			map.put("className", "order");
			map.put("propDef", propf);
			File file = new File(classLoader.getResource("JProperties.txt").getFile());
			String templateText = readFile(file.getPath(), Charset.defaultCharset());
			Template template = handlebars.compileInline(templateText);
			Context context = Context.newBuilder(map).build();

			String output = template.apply(context);
			System.out.println(output);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
