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

		public static String getTemplateString(String repositoryId, FileNameType fileType, Map<String, Object> fields)
				throws IOException {
			try {
				String fileName = null;
				LOG.info("getTemplateString for {}", fileType);
				if (FileNameType.BASEOBJECT.equals(fileType)) {
					fileName = "JBaseObject.txt";
				} else if (FileNameType.DOCUMENTOBJECT.equals(fileType)) {
					fileName = "JDocumentObject.txt";
				} else if (FileNameType.PROPERTYOBJECT.equals(fileType)) {
					fileName = "JProperties.txt";
				}
				Handlebars handlebars = new Handlebars();
				ClassLoader classLoader = HandleBarCheck.class.getClassLoader();
				File file = new File(classLoader.getResource(fileName).getFile());
				String templateText = readFile(file.getPath(), Charset.defaultCharset());
				Template template = handlebars.compileInline(templateText);
				Context context = Context.newBuilder(fields).build();
				return template.apply(context);
			} catch (Exception e) {
				e.printStackTrace();
			}
			return null;
		}

		static String readFile(String path, Charset encoding) throws IOException {
			byte[] encoded = Files.readAllBytes(Paths.get(path));
			return new String(encoded, encoding);
		}
	}
}
