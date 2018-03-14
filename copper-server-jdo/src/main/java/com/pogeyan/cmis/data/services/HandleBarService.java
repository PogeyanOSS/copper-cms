package com.pogeyan.cmis.data.services;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.jknack.handlebars.Context;
import com.github.jknack.handlebars.Handlebars;
import com.github.jknack.handlebars.Template;
import com.pogeyan.cmis.data.jdo.HandleBarCheck;

public class HandleBarService {
	private static final Logger LOG = LoggerFactory.getLogger(HandleBarService.class);

	public static class Impl {

		public static String getTemplateString(String repositoryId, String fileName, Map<String, Object> fields)
				throws IOException {
			LOG.info("getTemplateString for {}", fileName);
			Handlebars handlebars = new Handlebars();
			ClassLoader classLoader = HandleBarCheck.class.getClassLoader();
			File file = new File(classLoader.getResource(fileName + ".txt").getFile());
			String templateText = readFile(file.getPath(), Charset.defaultCharset());
			Template template = handlebars.compileInline(templateText);
			Context context = Context.newBuilder(fields).build();
			return template.apply(context);
		}

		static String readFile(String path, Charset encoding) throws IOException {
			byte[] encoded = Files.readAllBytes(Paths.get(path));
			return new String(encoded, encoding);
		}
	}
}
