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
package com.pogeyan.cmis.data.mongo.services;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.bson.Document;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.mongodb.morphia.converters.SimpleValueConverter;
import org.mongodb.morphia.converters.TypeConverter;
import org.mongodb.morphia.mapping.MappedField;
import org.mongodb.morphia.mapping.MappingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.BasicDBObject;
import com.mongodb.MongoClient;
import com.mongodb.MongoException;
import com.mongodb.client.MongoCollection;
import com.pogeyan.cmis.api.data.IDBClientFactory;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDiscoveryServiceDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.repo.IRepository;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.data.mongo.MBaseObject;
import com.pogeyan.cmis.data.mongo.MDocumentObject;
import com.pogeyan.cmis.data.mongo.MTypeDocumentObject;
import com.pogeyan.cmis.data.mongo.MTypeObject;

public class MongoClientFactory implements IDBClientFactory {
	private static final Logger LOG = LoggerFactory.getLogger(MongoClientFactory.class.getName());
	private static String MBASEOBJECTDAOIMPL = "MBaseObjectDAOImpl";
	private static String MDISCOVERYSERVICEDAO = "MDiscoveryServiceDAO";
	private static String MDOCUMENTOBJECTDAO = "MDocumentObjectDAO";
	private static String MDOCUMENTTYPEMANAGERDAO = "MDocumentTypeManagerDAO";
	private static String MNAVIGATIONSERVICEDAO = "MNavigationServiceDAO";
	private static String MTYPEMANAGERDAO = "MTypeManagerDAO";
	private Map<Class<?>, String> objectServiceClass = new HashMap<>();
	private final Map<String, Datastore> clientDatastores = new HashMap<String, Datastore>();
	private final Map<String, MongoClient> mongoClient = new HashMap<String, MongoClient>();
	private Morphia morphia = new Morphia();

	public MongoClientFactory() {
		objectServiceClass.put(MBaseObjectDAO.class, MongoClientFactory.MBASEOBJECTDAOIMPL);
		objectServiceClass.put(MDiscoveryServiceDAO.class, MongoClientFactory.MDISCOVERYSERVICEDAO);
		objectServiceClass.put(MDocumentObjectDAO.class, MongoClientFactory.MDOCUMENTOBJECTDAO);
		objectServiceClass.put(MNavigationServiceDAO.class, MongoClientFactory.MNAVIGATIONSERVICEDAO);
		objectServiceClass.put(MDocumentTypeManagerDAO.class, MongoClientFactory.MDOCUMENTTYPEMANAGERDAO);
		objectServiceClass.put(MTypeManagerDAO.class, MongoClientFactory.MTYPEMANAGERDAO);
		morphia.getMapper().getConverters().addConverter(new BaseTypeIdConverter());
		morphia.getMapper().getConverters().addConverter(new AceConverter());
		morphia.getMapper().getConverters().addConverter(new TokenConverter());
	}

	public static IDBClientFactory createDatabaseService() {
		return new MongoClientFactory();
	}

	@Override
	public String getType() {
		return "mongo";
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T getObjectService(String repositoryId, Class<?> objectServiceClass) {
		String className = this.objectServiceClass.get(objectServiceClass);
		if (className.equals(MongoClientFactory.MBASEOBJECTDAOIMPL)) {
			return (T) getContentDBMongoClient(repositoryId, (t) -> new MBaseObjectDAOImpl(MBaseObject.class, t));
		} else if (className.equals(MongoClientFactory.MDISCOVERYSERVICEDAO)) {
			return (T) getContentDBMongoClient(repositoryId, (t) -> new MDiscoveryServiceDAOImpl(MBaseObject.class, t));
		}
		if (className.equals(MongoClientFactory.MDOCUMENTOBJECTDAO)) {
			return (T) getContentDBMongoClient(repositoryId,
					(t) -> new MDocumentObjectDAOImpl(MDocumentObject.class, t));
		}
		if (className.equals(MongoClientFactory.MNAVIGATIONSERVICEDAO)) {
			return (T) getContentDBMongoClient(repositoryId,
					(t) -> new MNavigationServiceDAOImpl(MBaseObject.class, t));
		}
		if (className.equals(MongoClientFactory.MDOCUMENTTYPEMANAGERDAO)) {
			return (T) getContentDBMongoClient(repositoryId,
					(t) -> new MDocumentTypeManagerDAOImpl(MTypeDocumentObject.class, t));
		}
		if (className.equals(MongoClientFactory.MTYPEMANAGERDAO)) {
			return (T) getContentDBMongoClient(repositoryId, (t) -> new MTypeManagerDAOImpl(MTypeObject.class, t));
		}
		return null;
	}

	@Override
	public void addIndex(String repositoryId, String[] columnsToIndex) {
		IRepository repository = RepositoryManagerFactory.getInstance().getRepository(repositoryId);
		String dataBaseName = repository.getDBName().get("connectionString");
		List<String> properties = getClientProperties(dataBaseName);
		if (LOG.isDebugEnabled()) {
			LOG.debug("Host Name: {}, Content DB: {}", properties.get(0), properties.get(2));
		}
		Map<Object, Object> indexIds = new HashMap<>();
		Stream<String> indexId = Arrays.stream(columnsToIndex);
		indexId.forEach(x -> indexIds.put(x, 1));
		MongoCollection<Document> contentMongoClient = getMongoClient(repositoryId, properties.get(0),
				Integer.valueOf(properties.get(1))).getDatabase(properties.get(2)).getCollection("objectData");
		contentMongoClient.createIndex(new BasicDBObject(indexIds));
	}

	private <T> T getContentDBMongoClient(String repositoryId, Function<Datastore, T> fun) {
		Datastore clientDatastore = this.clientDatastores.get(repositoryId);
		if (clientDatastore == null) {
			IRepository repository = RepositoryManagerFactory.getInstance().getRepository(repositoryId);
			String dataBaseName = repository.getDBName().get("connectionString");
			List<String> properties = getClientProperties(dataBaseName);
			LOG.info("ContentDB Name-" + properties.get(2));
			LOG.info("HostId" + properties.get(0));
			int port = Integer.valueOf(properties.get(1));
			clientDatastore = morphia.createDatastore(getMongoClient(repositoryId, properties.get(0), port),
					properties.get(2));
			this.clientDatastores.put(repositoryId, clientDatastore);
		}

		return fun.apply(clientDatastore);
	}

	private MongoClient getMongoClient(String repositoryId, String host, int port) {
		MongoClient mClient = this.mongoClient.get(repositoryId);
		if (mClient == null) {
			mClient = new MongoClient(host, port);
			this.mongoClient.put(repositoryId, mClient);
		}
		return mClient;
	}

	/**
	 * Finds all substrings in MongoCilent connection details from the
	 * corresponding Environmental property.
	 */
	private List<String> getClientProperties(String props) {
		List<String> properties = new ArrayList<String>();
		String[] result = props.split(";");
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

	public static class AceConverter extends TypeConverter implements SimpleValueConverter {

		public AceConverter() {
			super(Ace.class);
		}

		@Override
		public Object decode(final Class<?> targetClass, final Object fromDBObject, final MappedField optionalExtraInfo)
				throws MappingException {
			if (fromDBObject != null) {
				JSONParser parser = new JSONParser();
				JSONObject jsonObject = null;
				try {
					jsonObject = (JSONObject) parser.parse(fromDBObject.toString());
					@SuppressWarnings("unchecked")
					List<String> permissions = (List<String>) jsonObject.get("permissions");
					JSONObject principal = (JSONObject) jsonObject.get("principal");
					String principalId = (String) principal.get("principalId");
					return new AccessControlEntryImpl(new AccessControlPrincipalDataImpl(principalId), permissions);
				} catch (ParseException e) {
					LOG.error("AceConverter Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
					throw new MongoException(e.toString());
				}
			}
			return null;
		}

		@Override
		public Object encode(final Object value, final MappedField optionalExtraInfo) {
			return value;
		}
	}

	public static class TokenConverter extends TypeConverter implements SimpleValueConverter {

		public TokenConverter() {
			super(TokenChangeType.class);
		}

		@Override
		public Object decode(final Class<?> targetClass, final Object fromDBObject, final MappedField optionalExtraInfo)
				throws MappingException {
			return TokenChangeType.getTokenValue(Integer.parseInt(fromDBObject.toString()));
		}

		@Override
		public Object encode(final Object value, final MappedField optionalExtraInfo) {
			return TokenChangeType.toValue(value.toString());
		}
	}

}