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
package com.pogeyan.cmis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.bson.Document;
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
import com.mongodb.client.MongoCollection;
import com.pogeyan.cmis.data.IDBClientFactory;
import com.pogeyan.cmis.api.repo.IRepository;
import com.pogeyan.cmis.data.dao.MBaseObjectDAO;
import com.pogeyan.cmis.data.dao.MDiscoveryServiceDAO;
import com.pogeyan.cmis.data.dao.MDocumentObjectDAO;
import com.pogeyan.cmis.data.dao.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.data.dao.MNavigationServiceDAO;
import com.pogeyan.cmis.data.dao.MTypeManagerDAO;
import com.pogeyan.cmis.data.daoimpl.MBaseObjectDAOImpl;
import com.pogeyan.cmis.data.daoimpl.MDiscoveryServiceDAOImpl;
import com.pogeyan.cmis.data.daoimpl.MDocumentObjectDAOImpl;
import com.pogeyan.cmis.data.daoimpl.MDocumentTypeManagerDAOImpl;
import com.pogeyan.cmis.data.daoimpl.MNavigationServiceDAOImpl;
import com.pogeyan.cmis.data.daoimpl.MTypeManagerDAOImpl;
import com.pogeyan.cmis.data.objects.MBaseObject;
import com.pogeyan.cmis.data.objects.MCmisDocumentTypeDefinition;
import com.pogeyan.cmis.data.objects.MDocumentObject;
import com.pogeyan.cmis.data.objects.MTypeObject;
import com.pogeyan.cmis.repo.impl.RepositoryManager;

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
	}

	public static IDBClientFactory createDatabaseService() {
		return new MongoClientFactory();

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
					(t) -> new MDocumentTypeManagerDAOImpl(MCmisDocumentTypeDefinition.class, t));
		}
		if (className.equals(MongoClientFactory.MTYPEMANAGERDAO)) {
			return (T) getContentDBMongoClient(repositoryId, (t) -> new MTypeManagerDAOImpl(MTypeObject.class, t));
		}
		return null;
	}

	@Override
	public void addIndex(String repositoryId, String[] columnsToIndex) {
		IRepository repository = RepositoryManager.getInstance().getRepository(repositoryId);
		String dataBaseName = repository.getDBName().get("connectionString");
		List<String> properties = getClientProperties(dataBaseName);
		LOG.info("ContentDB Name-" + properties.get(2));
		LOG.info("HostId" + properties.get(0));
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
			IRepository repository = RepositoryManager.getInstance().getRepository(repositoryId);
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

}