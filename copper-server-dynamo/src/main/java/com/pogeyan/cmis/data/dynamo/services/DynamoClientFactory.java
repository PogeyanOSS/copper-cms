package com.pogeyan.cmis.data.dynamo.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import com.amazonaws.services.dynamodbv2.document.DynamoDB;
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition;
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest;
import com.amazonaws.services.dynamodbv2.model.GlobalSecondaryIndex;
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement;
import com.amazonaws.services.dynamodbv2.model.KeyType;
import com.amazonaws.services.dynamodbv2.model.Projection;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput;
import com.pogeyan.cmis.api.data.IDBClientFactory;
import com.pogeyan.cmis.api.data.services.MBaseObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.api.data.services.MNavigationDocServiceDAO;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.api.data.services.MTypeManagerDAO;
import com.pogeyan.cmis.api.repo.IRepository;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.data.dynamo.DBaseObject;
import com.pogeyan.cmis.data.dynamo.DDocumentObject;
import com.pogeyan.cmis.data.dynamo.DTypeDocumentObject;
import com.pogeyan.cmis.data.dynamo.DTypeObject;
import com.pogeyan.cmis.data.dynamo.DynamoClient;
import com.pogeyan.cmis.data.dynamo.DynamoRepo;

public class DynamoClientFactory implements IDBClientFactory {

	private static final Logger LOG = LoggerFactory.getLogger(DynamoClientFactory.class.getName());
	private static String MBASEOBJECTDAOIMPL = "MBaseObjectDAOImpl";
	private static String MDISCOVERYSERVICEDAO = "MDiscoveryServiceDAO";
	private static String MDOCUMENTOBJECTDAO = "MDocumentObjectDAO";
	private static String MDOCUMENTTYPEMANAGERDAO = "MDocumentTypeManagerDAO";
	private static String MNAVIGATIONSERVICEDAO = "MNavigationServiceDAO";
	private static String MTYPEMANAGERDAO = "MTypeManagerDAO";
	private static String MNAVIGATIONDOCSERVICEDAO = "MNavigationDocServiceDAO";
	private Map<Class<?>, String> objectServiceClass = new HashMap<>();
	private Map<String, DynamoRepo> dc = new HashMap<>();
	private DynamoDB dynamoDB;

	// private DynamoClient dynamoClient = new DynamoClient();

	public DynamoClientFactory() {
		objectServiceClass.put(MBaseObjectDAO.class, DynamoClientFactory.MBASEOBJECTDAOIMPL);
		objectServiceClass.put(MTypeManagerDAO.class, DynamoClientFactory.MTYPEMANAGERDAO);
		objectServiceClass.put(MDocumentObjectDAO.class, DynamoClientFactory.MDOCUMENTOBJECTDAO);
		objectServiceClass.put(MNavigationServiceDAO.class, DynamoClientFactory.MNAVIGATIONSERVICEDAO);
		objectServiceClass.put(MDocumentTypeManagerDAO.class, DynamoClientFactory.MDOCUMENTTYPEMANAGERDAO);
		objectServiceClass.put(MNavigationDocServiceDAO.class, DynamoClientFactory.MNAVIGATIONDOCSERVICEDAO);
	}

	@Override
	public String getType() {
		return "dynamo";
	}

	public static IDBClientFactory createDatabaseService() {
		return new DynamoClientFactory();
	}

	@SuppressWarnings("unchecked")
	public <T> T getObjectService(String repositoryId, Class<?> objectServiceClass) {
		String className = this.objectServiceClass.get(objectServiceClass);

		if (className.equals(DynamoClientFactory.MBASEOBJECTDAOIMPL)) {
			return (T) getContentDBDynamoClient(repositoryId, (t) -> new DBaseObjectDAOImpl(DBaseObject.class, t));
		}
		if (className.equals(DynamoClientFactory.MDOCUMENTOBJECTDAO)) {
			return (T) getContentDBDynamoClient(repositoryId,
					(t) -> new DDocumentObjectDAOImpl(DDocumentObject.class, t));
		}
		if (className.equals(DynamoClientFactory.MNAVIGATIONSERVICEDAO)) {
			return (T) getContentDBDynamoClient(repositoryId,
					(t) -> new DNavigationServiceDAOImpl(DBaseObject.class, t));
		}
		if (className.equals(DynamoClientFactory.MDOCUMENTTYPEMANAGERDAO)) {
			return (T) getContentDBDynamoClient(repositoryId,
					(t) -> new DDocumentTypeManagerDAOImpl(DTypeDocumentObject.class, t));
		}
		if (className.equals(DynamoClientFactory.MTYPEMANAGERDAO)) {
			return (T) getContentDBDynamoClient(repositoryId, (t) -> new DTypeManagerDAOImpl(DTypeObject.class, t));
		}
		if (className.equals(DynamoClientFactory.MNAVIGATIONDOCSERVICEDAO)) {
			return (T) getContentDBDynamoClient(repositoryId,
					(t) -> new DNavigationDocServiceImpl(DDocumentObject.class, t));
		}

		return null;
	}

	public <T> T getContentDBDynamoClient(String repositoryId, Function<DynamoRepo, T> fun) {

		DynamoRepo dr = DynamoClient.get(repositoryId);

		if (dr == null) {
			// create table

			IRepository repository = RepositoryManagerFactory.getInstance().getRepository(repositoryId);
			String accessKeyId = repository.getLogin().get("accessKeyId");
			String secretAccessKey = repository.getLogin().get("secretAccessKey");
			String region = repository.getLogin().get("region");
			AmazonDynamoDB client = AmazonDynamoDBClientBuilder.standard()
					.withCredentials(
							new AWSStaticCredentialsProvider(new BasicAWSCredentials(accessKeyId, secretAccessKey)))
					.withRegion(secretAccessKey).build();

			DynamoDB dynamoDB = new DynamoDB(client);

			// Types Attributes

			// Attributes
			ArrayList<AttributeDefinition> attributeDefinitionsTypes = new ArrayList<AttributeDefinition>();
			attributeDefinitionsTypes.add(new AttributeDefinition().withAttributeName("PK").withAttributeType("S"));

			attributeDefinitionsTypes.add(new AttributeDefinition().withAttributeName("SK").withAttributeType("S"));

			// Attributes
			ArrayList<AttributeDefinition> attributeDefinitions = new ArrayList<AttributeDefinition>();
			attributeDefinitions.add(new AttributeDefinition().withAttributeName("PK").withAttributeType("S"));

			attributeDefinitions.add(new AttributeDefinition().withAttributeName("SK").withAttributeType("S"));

			attributeDefinitions.add(new AttributeDefinition().withAttributeName("time").withAttributeType("S"));

			attributeDefinitions.add(new AttributeDefinition().withAttributeName("name").withAttributeType("S"));

			attributeDefinitions.add(new AttributeDefinition().withAttributeName("path").withAttributeType("S"));

			attributeDefinitions
					.add(new AttributeDefinition().withAttributeName("internalPath").withAttributeType("S"));

			// Table Key schema
			ArrayList<KeySchemaElement> tableKeySchema = new ArrayList<KeySchemaElement>();
			tableKeySchema.add(new KeySchemaElement().withAttributeName("PK").withKeyType(KeyType.HASH)); // Partition
																											// key

			tableKeySchema.add(new KeySchemaElement().withAttributeName("SK").withKeyType(KeyType.RANGE)); // Sort
																											// key

			// Indexes' initial provisioned throughput
			ProvisionedThroughput ptIndex = new ProvisionedThroughput().withReadCapacityUnits(1L)
					.withWriteCapacityUnits(1L);

			// TimeIndex
			GlobalSecondaryIndex timeIndex = new GlobalSecondaryIndex().withIndexName("timeIndex")
					.withProvisionedThroughput(ptIndex)
					.withKeySchema(new KeySchemaElement().withAttributeName("SK").withKeyType(KeyType.HASH), // Partition
																												// key
							new KeySchemaElement().withAttributeName("time").withKeyType(KeyType.RANGE)) // Sort
																											// key
					.withProjection(new Projection().withProjectionType("ALL"));

			// NameIndex
			GlobalSecondaryIndex nameIndex = new GlobalSecondaryIndex().withIndexName("nameIndex")
					.withProvisionedThroughput(ptIndex)
					.withKeySchema(new KeySchemaElement().withAttributeName("SK").withKeyType(KeyType.HASH), // Partition
																												// key
							new KeySchemaElement().withAttributeName("name").withKeyType(KeyType.RANGE)) // Sort
																											// key
					.withProjection(new Projection().withProjectionType("ALL"));

			// pathIndex
			GlobalSecondaryIndex pathIndex = new GlobalSecondaryIndex().withIndexName("pathIndex")
					.withProvisionedThroughput(ptIndex)
					.withKeySchema(new KeySchemaElement().withAttributeName("SK").withKeyType(KeyType.HASH), // Partition
																												// key
							new KeySchemaElement().withAttributeName("path").withKeyType(KeyType.RANGE)) // Sort
																											// key
					.withProjection(new Projection().withProjectionType("ALL"));

			// internalPathIndex
			GlobalSecondaryIndex internalPathIndex = new GlobalSecondaryIndex().withIndexName("internalPathIndex")
					.withProvisionedThroughput(ptIndex)
					.withKeySchema(new KeySchemaElement().withAttributeName("SK").withKeyType(KeyType.HASH), // Partition
																												// key
							new KeySchemaElement().withAttributeName("internalPath").withKeyType(KeyType.RANGE)) // Sort
					// key
					.withProjection(new Projection().withProjectionType("ALL"));
			// DueDateIndex
			// GlobalSecondaryIndex dueDateIndex =
			// new
			// GlobalSecondaryIndex().withIndexName("DueDateIndex").withProvisionedThroughput(ptIndex)
			// .withKeySchema(
			// new
			// KeySchemaElement().withAttributeName("DueDate").withKeyType(KeyType.HASH)) //
			// Partition
			// // key
			// .withProjection(new Projection().withProjectionType("ALL"));

			CreateTableRequest createTableRequest = new CreateTableRequest().withTableName(repositoryId + "_objectData")
					.withProvisionedThroughput(new ProvisionedThroughput().withReadCapacityUnits((long) 1)
							.withWriteCapacityUnits((long) 1))
					.withAttributeDefinitions(attributeDefinitions).withKeySchema(tableKeySchema)
					.withGlobalSecondaryIndexes(timeIndex, nameIndex, pathIndex, internalPathIndex);

			CreateTableRequest requestTypes = new CreateTableRequest().withTableName(repositoryId + "_type")
					.withProvisionedThroughput(new ProvisionedThroughput().withReadCapacityUnits((long) 1)
							.withWriteCapacityUnits((long) 1))
					.withAttributeDefinitions(attributeDefinitionsTypes).withKeySchema(tableKeySchema);

			dynamoDB.createTable(createTableRequest);
			dynamoDB.createTable(requestTypes);

			dr = new DynamoRepo(repositoryId, dynamoDB);

			DynamoClient.put(repositoryId, dr);

		}
		return fun.apply(dr);

	}

	@Override
	public void addIndex(String repositoryId, String[] columnsToIndex) {
		// TODO Auto-generated method stub

	}

}
