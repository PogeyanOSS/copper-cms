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
package com.pogeyan.cmis.repo;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.mongodb.morphia.converters.SimpleValueConverter;
import org.mongodb.morphia.converters.StringConverter;
import org.mongodb.morphia.converters.TypeConverter;
import org.mongodb.morphia.mapping.MappedField;
import org.mongodb.morphia.mapping.MappingException;
import org.mongodb.morphia.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.codehaus.jackson.map.ObjectMapper;

import com.mongodb.BasicDBList;
import com.mongodb.MongoClient;
import com.pogeyan.cmis.api.repo.IRepository;
import com.pogeyan.cmis.api.repo.IRepositoryStore;

public class MongoDBRepositoryStore implements IRepositoryStore {
	private static final Logger LOG = LoggerFactory.getLogger(MongoDBRepositoryStore.class);
	final static MongoDBRepositoryStore instance = new MongoDBRepositoryStore();
	private Datastore adminMongoStore = null;
	private MongoClient adminMongoClient = null;
	private Morphia morphia = new Morphia();

	public MongoDBRepositoryStore get() {
		return instance;
	}

	@Override
	public List<IRepository> getRepositories(String repositoryId) {
		Datastore mongoStore = getMasterDBInstance();
		if (mongoStore != null) {
			Query<MRepository> query = mongoStore.createQuery(MRepository.class);
			List<MRepository> respositoryList = query.asList();
			List<IRepository> respository = respositoryList.stream().collect(Collectors.toCollection(ArrayList::new));
			return respository;
		}
		return null;
	}

	@Override
	public IRepository getRepository(String repositoryId) {
		MRepositoryDAO repositoryMorphiaDAO = new MRepositoryDAOImpl(MRepository.class, getMasterDBInstance());
		IRepository repository = repositoryMorphiaDAO.getByRepositoryId(repositoryId);
		return repository;
	}

	public Datastore getMasterDBInstance() {
		String envVariables = System.getenv("CMIS_MASTERDB");
		if (envVariables == null) {
			LOG.error("CMIS_MASTERDB not defined, unable to initialize CMIS");
			return null;
		}
		List<String> properties = readingMongoCilentProperties(envVariables);
		int port = Integer.valueOf(properties.get(1));
		if (adminMongoClient == null) {
			LOG.info("Initializing MasterDB client: {}", properties);
			adminMongoClient = new MongoClient(properties.get(0), port);
			morphia.getMapper().getConverters().addConverter(new BaseTypeIdConverter());
			morphia.getMapper().getConverters().addConverter(new UserFieldConverter());
			adminMongoStore = morphia.createDatastore(adminMongoClient, properties.get(2));
		}

		return adminMongoStore;
	}

	/**
	 * Finds all substrings in MongoCilent connection details from the
	 * corresponding Environmental property.
	 */
	private List<String> readingMongoCilentProperties(String s) {

		List<String> properties = new ArrayList<String>();
		String[] result = s.split(";");
		String[] resultHost = result[0].split(":");
		properties.add(resultHost[0]);
		properties.add(resultHost[1]);
		properties.add(result[1]);
		return properties;

	}

	public static class BaseTypeIdConverter extends TypeConverter implements SimpleValueConverter {

		public BaseTypeIdConverter() {
			super(BaseTypeId.class);
		}

		@Override
		public Object decode(final Class<?> targetClass, final Object fromDBObject, final MappedField optionalExtraInfo)
				throws MappingException {
			if (fromDBObject == null) {
				return BaseTypeId.CMIS_FOLDER;
			}

			return BaseTypeId.fromValue(fromDBObject.toString().toLowerCase());
		}

		@Override
		public Object encode(final Object value, final MappedField optionalExtraInfo) {
			if (value == null) {
				return null;
			}

			return ((BaseTypeId) value).value();
		}
	}

	public static class UserFieldConverter extends StringConverter {
		static ObjectMapper jsonMapper = new ObjectMapper();

		@Override
		public Object decode(final Class targetClass, final Object fromDBObject, final MappedField optionalExtraInfo) {
			if (fromDBObject == null) {
				return null;
			}

			if (targetClass.equals(fromDBObject.getClass())) {
				return fromDBObject;
			}

			if (fromDBObject instanceof List) {
				try {
					return jsonMapper.writeValueAsString(fromDBObject);
				} catch (IOException e) {
					return null;
				}
			}

			return super.decode(targetClass, fromDBObject);
		}

	}
}
