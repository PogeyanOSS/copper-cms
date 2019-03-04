package org.apache.chemistry.opencmis.tck.awstest;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.INFO;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.SKIPPED;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.UNEXPECTED_EXCEPTION;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.WARNING;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.ObjectType;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.CreatablePropertyTypes;
import org.apache.chemistry.opencmis.commons.data.NewTypeSettableAttributes;
import org.apache.chemistry.opencmis.commons.definitions.Choice;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.enums.VersioningState;
import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractPropertyDefinition;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChoiceImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.DocumentTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyBooleanDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDateTimeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDecimalDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyHtmlDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIntegerDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyUriDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.json.JSONArray;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParseException;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParser;
import org.apache.chemistry.opencmis.tck.CmisTestResult;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;

public class AwsLocalStorageTest extends AbstractSessionTest {

	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("Create Type with Aws Local StorageTest");
		setDescription("Creates a Document type with AWS StorageTest and checking if folder exists for that type");
	}

	public void run(Session session) throws Exception {
		Session session1 = null;
		String user1 = System.getenv("USER1");
		String password1 = System.getenv("PASSWORD1");

		try {
			session1 = createsession(user1, password1);
		} catch (Exception e) {
			addResult(createResult(INFO, "CreateSession failed for " + user1));
		}

		ObjectType parentType = session1.getTypeDefinition(getDocumentTestTypeId());
		if (parentType.getTypeMutability() == null
				|| !Boolean.TRUE.equals(parentType.getTypeMutability().canCreate())) {
			addResult(createResult(SKIPPED, "Test document type doesn't allow creating a sub-type. Test skipped!"));
			return;
		}

		ObjectType newType = createTypeWithProperties(session1, parentType);
		Map<String, Object> map = createAWSStorageObject(session1, newType);

		// delete the test folder
		deleteTestFolder();
		deleteTypeFolderInAWS(session1, newType, (String) map.get("bucketName"), (AwsS3StorageService) map.get("AWS"));
		deleteType(session1, newType.getId());

		addResult(createInfoResult(
				"Tested the creation of a Document type with AWS StorageTest and checking if folder exists for that type"));

	}

	private void deleteTypeFolderInAWS(Session session1, ObjectType newType, String bucketName,
			AwsS3StorageService awsStorageService) {
		Folder typeFolder = (Folder) session1.getObjectByPath("/" + newType.getId());
		String FolderName = session1.getRepositoryInfo().getId().toLowerCase() + '/' + typeFolder.getName() + '/';
		awsStorageService.deleteFolder(FolderName, bucketName);
	}

	@SuppressWarnings("unchecked")
	private Map<String, Object> createAWSStorageObject(Session session1, ObjectType newType) {
		Map<String, Object> map = new HashMap<String, Object>();
		String envVariable = System.getenv("CMIS_REPO_JSON_LOCATION");
		if (envVariable == null) {
			addResult(createResult(FAILURE, "set the environment variables of CMIS_REPO_JSON_LOCATION"));
		}
		Object obj = null;
		try {
			obj = new JSONParser().parse(new FileReader(envVariable));
		} catch (FileNotFoundException e) {
			addResult(createResult(FAILURE, "FileNotFoundException while parsing the json"));
		} catch (IOException e) {
			addResult(createResult(FAILURE, "IOException while parsing the json"));
		} catch (JSONParseException e) {
			addResult(createResult(FAILURE, "JSONParseException while parsing the json"));
		}

		Map<String, String> parameters = new HashMap<String, String>();
		JSONArray repoArray = (JSONArray) obj;
		for (Object object : repoArray) {
			JSONObject jsonObject = (JSONObject) object;
			parameters = (Map<String, String>) jsonObject.get("file");
		}

		AwsS3StorageService awsStorageService = new AwsS3StorageService();
		awsStorageService.setClient(parameters.get("accessKeyId"), parameters.get("secretAccessKey"),
				parameters.get("region"));
		checkIfFolderExistsinAWS(session1, newType, parameters.get("bucket"), awsStorageService);
		map.put("AWS", awsStorageService);
		map.put("bucketName", parameters.get("bucket"));
		return map;
	}

	private void checkIfFolderExistsinAWS(Session session1, ObjectType newType, String bucketName,
			AwsS3StorageService awsService) {
		CmisTestResult failure = null;
		// check if folder created in localAws
		Folder typeFolder = (Folder) session1.getObjectByPath("/" + newType.getId());
		failure = createResult(FAILURE, "TypeId and folderName dont match!");
		addResult(assertEquals(typeFolder.getName(), newType.getId(), null, failure));
		String FolderName = session1.getRepositoryInfo().getId().toLowerCase() + '/' + typeFolder.getName() + '/';
		boolean checkFolder = awsService.folderExists(FolderName, bucketName);
		if (!checkFolder) {
			addResult(createResult(FAILURE, "folder doesnt exists", false));
		} else {
			addResult(createResult(INFO, "folder exists", false));
		}
	}

	private ObjectType createTypeWithProperties(Session session1, ObjectType parentType) {
		CmisTestResult failure = null;

		CreatablePropertyTypes cpt = session1.getRepositoryInfo().getCapabilities().getCreatablePropertyTypes();
		if (cpt == null || cpt.canCreate() == null || cpt.canCreate().isEmpty()) {
			addResult(createResult(FAILURE, "Repository Info does not indicate, which property types can be created!"));
			return null;
		}

		// define the type
		DocumentTypeDefinitionImpl newTypeDef = createDocumentTypeDefinition(session1, "tck:testid_with_properties",
				parentType);

		// add a property for each creatable property type
		for (PropertyType propType : PropertyType.values()) {
			if (!cpt.canCreate().contains(propType)) {
				continue;
			}

			newTypeDef.addPropertyDefinition(createPropertyDefinition(propType));
		}

		// create the type
		ObjectType newType = createType(session1, newTypeDef);
		if (newType == null) {
			return null;
		}

		// get the type
		ObjectType newType2 = null;
		try {
			newType2 = session1.getTypeDefinition(newType.getId());

			// assert type definitions
			failure = createResult(FAILURE,
					"The type definition returned by createType() doesn't match the type definition returned by getTypeDefinition()!");
			addResult(assertEquals(newType, newType2, null, failure));
		} catch (CmisObjectNotFoundException e) {
			addResult(createResult(FAILURE, "Newly created type can not be fetched. Id: " + newType.getId(), e, false));
		}
		return newType2;
	}

	private DocumentTypeDefinitionImpl createDocumentTypeDefinition(Session session, String typeId,
			ObjectType parentType) {
		CmisTestResult failure = null;

		NewTypeSettableAttributes settableAttributes = session.getRepositoryInfo().getCapabilities()
				.getNewTypeSettableAttributes();
		if (settableAttributes == null) {
			addResult(createResult(WARNING, "Capability NewTypeSettableAttributes is not set!"));
		}

		DocumentTypeDefinitionImpl result = new DocumentTypeDefinitionImpl();

		result.setBaseTypeId(parentType.getBaseTypeId());
		result.setParentTypeId(parentType.getId());

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetId())) {
			result.setId(typeId);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'id' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetId(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetLocalName())) {
			result.setLocalName("tck:testlocal");
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'localName' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetLocalName(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetLocalNamespace())) {
			result.setLocalNamespace("http://tck/testlocalnamespace");
		} else if (settableAttributes != null) {
			failure = createResult(WARNING,
					"Flag 'localNamespace' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetLocalNamespace(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetDisplayName())) {
			result.setDisplayName("TCK Document Type");
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'displayName' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetDisplayName(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetDescription())) {
			result.setDescription("This is the TCK document type");
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'description' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetDescription(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetQueryName())) {
			result.setQueryName("tck:testqueryname");
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'queryName' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetQueryName(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetQueryable())) {
			result.setIsQueryable(false);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'queryable' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetQueryable(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetFulltextIndexed())) {
			result.setIsFulltextIndexed(false);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING,
					"Flag 'fulltextIndexed' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetFulltextIndexed(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetIncludedInSupertypeQuery())) {
			result.setIsIncludedInSupertypeQuery(false);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING,
					"Flag 'includedInSupertypeQuery' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetIncludedInSupertypeQuery(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetControllableAcl())) {
			result.setIsControllableAcl(false);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING,
					"Flag 'controllableACL' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetControllableAcl(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetControllablePolicy())) {
			result.setIsControllablePolicy(false);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING,
					"Flag 'controllablePolicy' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetControllablePolicy(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetCreatable())) {
			result.setIsCreatable(true);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'creatable' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetCreatable(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetFileable())) {
			result.setIsFileable(true);
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'fileable' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetFileable(), null, failure));
		}

		result.setIsVersionable(true);
		result.setContentStreamAllowed(ContentStreamAllowed.ALLOWED);

		return result;
	}

	private AbstractPropertyDefinition<?> createPropertyDefinition(PropertyType propertyType) {
		switch (propertyType) {
		case BOOLEAN:
			PropertyBooleanDefinitionImpl result1 = new PropertyBooleanDefinitionImpl();
			// add choice
			List<Choice<Boolean>> choiceList1 = new LinkedList<Choice<Boolean>>();
			ChoiceImpl<Boolean> choice1 = new ChoiceImpl<Boolean>();
			choice1.setDisplayName("TCK Boolean List");
			List<Boolean> choiceValues1 = new ArrayList<Boolean>();
			choiceValues1.add(true);
			choiceValues1.add(false);
			choice1.setValue(choiceValues1);
			choiceList1.add(choice1);
			result1.setChoices(choiceList1);
			setOtherProperties(result1, propertyType);
			return result1;
		case ID:
			PropertyIdDefinitionImpl result2 = new PropertyIdDefinitionImpl();
			setOtherProperties(result2, propertyType);
			return result2;
		case INTEGER:
			PropertyIntegerDefinitionImpl result3 = new PropertyIntegerDefinitionImpl();
			// add choice
			List<Choice<BigInteger>> choiceList3 = new LinkedList<Choice<BigInteger>>();
			ChoiceImpl<BigInteger> choice3 = new ChoiceImpl<BigInteger>();
			choice3.setDisplayName("TCK Integer List");
			List<BigInteger> choiceValues3 = new ArrayList<BigInteger>();
			choiceValues3.add(BigInteger.valueOf(1));
			choiceValues3.add(BigInteger.valueOf(2));
			choice3.setValue(choiceValues3);
			choiceList3.add(choice3);
			result3.setChoices(choiceList3);
			setOtherProperties(result3, propertyType);
			return result3;
		case DATETIME:
			PropertyDateTimeDefinitionImpl result4 = new PropertyDateTimeDefinitionImpl();
			setOtherProperties(result4, propertyType);
			return result4;
		case DECIMAL:
			PropertyDecimalDefinitionImpl result5 = new PropertyDecimalDefinitionImpl();
			setOtherProperties(result5, propertyType);
			return result5;
		case HTML:
			PropertyHtmlDefinitionImpl result6 = new PropertyHtmlDefinitionImpl();
			setOtherProperties(result6, propertyType);
			return result6;
		case URI:
			PropertyUriDefinitionImpl result7 = new PropertyUriDefinitionImpl();
			setOtherProperties(result7, propertyType);
			return result7;
		case STRING:
			PropertyStringDefinitionImpl result = new PropertyStringDefinitionImpl();
			List<Choice<String>> choiceList = new LinkedList<Choice<String>>();
			ChoiceImpl<String> choice = new ChoiceImpl<String>();
			choice.setDisplayName("TCK String List");
			List<String> choiceValues = new ArrayList<String>();
			choiceValues.add("ABC");
			choiceValues.add("XYZ");
			choice.setValue(choiceValues);
			choiceList.add(choice);
			result.setChoices(choiceList);
			setOtherProperties(result, propertyType);
			return result;
		}
		return null;
	}

	private void setOtherProperties(AbstractPropertyDefinition<?> result, PropertyType propertyType) {
		result.setPropertyType(propertyType);
		result.setId("tck:" + propertyType.value());
		result.setLocalName("tck:local_" + propertyType.value());
		result.setLocalNamespace("tck:testlocalnamespace");
		result.setDisplayName("TCK " + propertyType.value() + " propertry");
		result.setQueryName("tck:" + propertyType.value());
		result.setDescription("TCK " + propertyType.value() + " propertry");
		if (result.getChoices().size() > 0) {
			result.setCardinality(Cardinality.MULTI);
		} else {
			result.setCardinality(Cardinality.SINGLE);
		}
		result.setUpdatability(Updatability.READWRITE);
		result.setIsInherited(false);
		result.setIsQueryable(false);
		result.setIsOrderable(false);
		result.setIsRequired(false);
		result.setIsOpenChoice(true);
	}

	/**
	 * Creates a document with custom properties
	 */
	protected Document createDocumentWithCustomProperties(Session session, Folder parent, String name,
			String objectTypeId, String[] secondaryTypeIds, String content) {

		if (parent == null) {
			throw new IllegalArgumentException("Parent is not set!");
		}
		if (name == null) {
			throw new IllegalArgumentException("Name is not set!");
		}
		if (objectTypeId == null) {
			throw new IllegalArgumentException("Object Type ID is not set!");
		}

		if (content == null) {
			content = "";
		}

		// check type
		ObjectType type;
		try {
			type = session.getTypeDefinition(objectTypeId);
		} catch (CmisObjectNotFoundException e) {
			addResult(createResult(UNEXPECTED_EXCEPTION,
					"Document type '" + objectTypeId + "' is not available: " + e.getMessage(), e, true));
			return null;
		}

		if (Boolean.FALSE.equals(type.isCreatable())) {
			addResult(createResult(SKIPPED, "Document type '" + objectTypeId + "' is not creatable!", true));
			return null;
		}

		// create
		Map<String, Object> properties = new HashMap<String, Object>();
		properties.put(PropertyIds.NAME, name);
		properties.put(PropertyIds.OBJECT_TYPE_ID, objectTypeId);

		if (secondaryTypeIds != null) {
			properties.put(PropertyIds.SECONDARY_OBJECT_TYPE_IDS, Arrays.asList(secondaryTypeIds));
		}

		type = session.getTypeDefinition(objectTypeId);
		if (!(type instanceof DocumentTypeDefinition)) {
			addResult(createResult(FAILURE, "Type is not a document type! Type: " + objectTypeId, true));
			return null;
		}

		DocumentTypeDefinition docType = (DocumentTypeDefinition) type;
		VersioningState versioningState = (Boolean.TRUE.equals(docType.isVersionable()) ? VersioningState.MAJOR
				: VersioningState.NONE);

		for (Map.Entry<String, PropertyDefinition<?>> propDef : docType.getPropertyDefinitions().entrySet()) {
			if (propDef.getValue().getChoices().size() > 0) {
				if (propDef.getValue().getPropertyType().equals(PropertyType.BOOLEAN)) {
					List<Boolean> booleanList = new ArrayList<Boolean>();
					booleanList.add((Boolean) propDef.getValue().getChoices().get(0).getValue().get(0));
					properties.put(propDef.getValue().getId(), booleanList);
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.STRING)) {
					List<String> stringList = new ArrayList<String>();
					stringList.add((String) propDef.getValue().getChoices().get(0).getValue().get(0));
					properties.put(propDef.getValue().getId(), stringList);
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.INTEGER)) {
					List<BigInteger> intList = new ArrayList<BigInteger>();
					intList.add((BigInteger) propDef.getValue().getChoices().get(0).getValue().get(0));
					properties.put(propDef.getValue().getId(), intList);
				}
			}
			if (!(propDef.getKey().equalsIgnoreCase(PropertyIds.NAME)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFIED_BY)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.OBJECT_TYPE_ID)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CREATED_BY)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.PATH)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.DESCRIPTION)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CHANGE_TOKEN)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.ALLOWED_CHILD_OBJECT_TYPE_IDS)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.SECONDARY_OBJECT_TYPE_IDS)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.PARENT_ID)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.BASE_TYPE_ID)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.OBJECT_ID)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFICATION_DATE)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CREATION_DATE)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_LENGTH)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_FILE_NAME)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_MIME_TYPE)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CHECKIN_COMMENT)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_BY)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_LABEL)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.IS_MAJOR_VERSION)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.IS_LATEST_VERSION)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_ID)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_ID)
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_ID)
					|| propDef.getKey().equalsIgnoreCase("cmis:previousVersionObjectId")
					|| propDef.getKey().equalsIgnoreCase(PropertyIds.IS_IMMUTABLE))) {
				if (propDef.getValue().getPropertyType().equals(PropertyType.ID)) {
					properties.put(propDef.getValue().getId(), "123");
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.DATETIME)) {
					properties.put(propDef.getValue().getId(), new GregorianCalendar());
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.DECIMAL)) {
					properties.put(propDef.getValue().getId(), new BigDecimal("0.2"));
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.HTML)) {
					properties.put(propDef.getValue().getId(), "<html>");
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.URI)) {
					properties.put(propDef.getValue().getId(), AbstractRunner.BROWSER_URL);
				}
			}
		}

		byte[] contentBytes = null;
		Document result = null;
		try {
			contentBytes = IOUtils.toUTF8Bytes(content);
			ContentStream contentStream = new ContentStreamImpl(name, BigInteger.valueOf(contentBytes.length),
					"text/plain", new ByteArrayInputStream(contentBytes));

			// create the document
			result = parent.createDocument(properties, contentStream, versioningState, null, null, null,
					SELECT_ALL_NO_CACHE_OC);

			contentStream.getStream().close();
		} catch (Exception e) {
			addResult(createResult(UNEXPECTED_EXCEPTION, "Document could not be created! Exception: " + e.getMessage(),
					e, true));
			return null;
		}

		try {
			CmisTestResult f;

			// check document name
			f = createResult(FAILURE, "Document name does not match!", false);
			addResult(assertEquals(name, result.getName(), null, f));

			// check content length
			f = createResult(WARNING, "Content length does not match!", false);
			addResult(assertEquals((long) contentBytes.length, result.getContentStreamLength(), null, f));

			// check the new document
			addResult(checkObject(session, result, getAllProperties(result), "New document object spec compliance"));

			// check content
			try {
				ContentStream contentStream = result.getContentStream();

				f = createResult(WARNING, "Document filename and the filename of the content stream do not match!",
						false);
				addResult(assertEquals(name, contentStream.getFileName(), null, f));

				f = createResult(WARNING,
						"cmis:contentStreamFileName and the filename of the content stream do not match!", false);
				addResult(assertEquals(result.getContentStreamFileName(), contentStream.getFileName(), null, f));

				String fetchedContent = getStringFromContentStream(result.getContentStream());
				if (!content.equals(fetchedContent)) {
					addResult(createResult(FAILURE,
							"Content of newly created document doesn't match the orign content!"));
				}
			} catch (IOException e) {
				addResult(createResult(UNEXPECTED_EXCEPTION,
						"Content of newly created document couldn't be read! Exception: " + e.getMessage(), e, true));
			}
		} catch (CmisBaseException e) {
			addResult(createResult(UNEXPECTED_EXCEPTION,
					"Newly created document is invalid! Exception: " + e.getMessage(), e, true));
		}

		return result;
	}

	public Session createsession(String username, String password) {
		Session session = null;
		if (username != null && password != null) {
			SessionFactory factory = SessionFactoryImpl.newInstance();
			Map<String, String> parameters = new HashMap<String, String>();
			parameters.put(SessionParameter.USER, username);
			parameters.put(SessionParameter.PASSWORD, password);
			parameters.put(SessionParameter.BROWSER_URL, AbstractRunner.BROWSER_URL);
			parameters.put(SessionParameter.BINDING_TYPE, BindingType.BROWSER.value());
			parameters.put(SessionParameter.REPOSITORY_ID, AbstractRunner.REPOSITORY_ID);
			session = factory.createSession(parameters);
		} else {
			addResult(createResult(FAILURE, "set the environment variables of username and password"));
		}
		return session;
	}
}
