package org.apache.chemistry.opencmis.tck.tests.versioning;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.SKIPPED;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.WARNING;

import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.ObjectId;
import org.apache.chemistry.opencmis.client.api.ObjectType;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.CreatablePropertyTypes;
import org.apache.chemistry.opencmis.commons.data.NewTypeSettableAttributes;
import org.apache.chemistry.opencmis.commons.definitions.Choice;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.definitions.SecondaryTypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.ContentStreamAllowed;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;
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
import org.apache.chemistry.opencmis.commons.impl.dataobjects.SecondaryTypeDefinitionImpl;
import org.apache.chemistry.opencmis.tck.CmisTestResult;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;

public class CustomSecondaryTypeIdWithVersioningTest extends AbstractSessionTest {
	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("Secondary Object TypeId Test");
		setDescription("Creates a document,and added the Secondary Object TypeId's");
	}

	@Override
	public void run(Session session) {
		DocumentTypeDefinition docType = null;
		SecondaryTypeDefinition docType1 = null;
		try {
			// create folder and document
			Folder testFolder = createTestFolder(session);

			ObjectType parentType = session.getTypeDefinition(getDocumentTestTypeId());
			docType = createTypeWithProperties(session, parentType);
			ObjectType secondaryType = session.getTypeDefinition(getSecondaryTestTypeId());
			docType1 = createTypeWithProperties1(session, secondaryType);

			PropertyDefinition<?> secTypesPropDef = docType1.getPropertyDefinitions()
					.get(PropertyIds.SECONDARY_OBJECT_TYPE_IDS);
			if (secTypesPropDef == null) {
				addResult(createResult(FAILURE,
						"Test document type has no " + PropertyIds.SECONDARY_OBJECT_TYPE_IDS + " property!"));
				return;
			} else if (secTypesPropDef.getUpdatability() != Updatability.READWRITE) {
				addResult(createResult(SKIPPED,
						"Test document type does not allow attaching secondary types. Test skipped!"));
				return;
			}

			Document doc = testFolder.createDocument(getProperties("versiondeletetest.txt", docType, docType1),
					getContentStream(), null, null, null, null, SELECT_ALL_NO_CACHE_OC);
			addResult(checkObject(session, doc, getAllProperties(doc), "Document spec compliance"));

			ObjectId pwcId = doc.checkOut();
			Document pwc = (Document) session.getObject(pwcId, SELECT_ALL_NO_CACHE_OC);

			addResult(checkObject(session, pwc, getAllProperties(pwc), "PWC spec compliance - test 1"));
			addResult(checkObject(session, pwc, getAllSecondaryProperties(pwc), "PWC secondary compliance - test 2"));
			checkCheckedOut(pwc);

			if (!docType.isVersionable()) {
				addResult(createResult(SKIPPED, "Test type is not versionable. Test skipped!"));
				doc.delete(true);
				return;
			}
		} finally {
			deleteTestFolder();
			deleteType(session, docType.getId());
			deleteType(session, docType1.getId());
		}
	}

	private String[] getAllSecondaryProperties(Document object) {
		object.getSecondaryTypes().get(0);
		String[] propertiesk = new String[object.getType().getPropertyDefinitions().size()];
		int i = 0;
		for (String propId : object.getType().getPropertyDefinitions().keySet()) {
			propertiesk[i++] = propId;
		}
		return propertiesk;
	}

	private DocumentTypeDefinition createTypeWithProperties(Session session, ObjectType parentType) {
		// define the type
		DocumentTypeDefinitionImpl newTypeDef = createDocumentTypeDefinition(session, "tck:testid_with_properties",
				parentType);
		CreatablePropertyTypes cpt = session.getRepositoryInfo().getCapabilities().getCreatablePropertyTypes();
		// add a property for each creatable property type
		for (PropertyType propType : PropertyType.values()) {
			if (!cpt.canCreate().contains(propType)) {
				continue;
			}
			newTypeDef.addPropertyDefinition(createPropertyDefinition(propType));
		}
		// create the type
		ObjectType newType = createType(session, newTypeDef);
		if (newType == null) {
			return null;
		}
		// get the type
		DocumentTypeDefinition docType = (DocumentTypeDefinition) session.getTypeDefinition(newType.getId());
		return docType;

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
		result.setUpdatability(Updatability.WHENCHECKEDOUT);
		result.setIsInherited(false);
		result.setIsQueryable(false);
		result.setIsOrderable(false);
		result.setIsRequired(false);
		result.setIsOpenChoice(true);
	}

	private Map<String, Object> getProperties(String name, DocumentTypeDefinition docType,
			SecondaryTypeDefinition docType1) {
		Map<String, Object> properties = new HashMap<String, Object>();
		properties.put(PropertyIds.NAME, name);
		properties.put(PropertyIds.OBJECT_TYPE_ID, docType.getId());
		ArrayList<String> secIds = new ArrayList<String>();
		secIds.add(docType1.getId());
		properties.put(PropertyIds.SECONDARY_OBJECT_TYPE_IDS, secIds);
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
		for (Map.Entry<String, PropertyDefinition<?>> propDef : docType1.getPropertyDefinitions().entrySet()) {
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

		return properties;
	}

	private ContentStream getContentStream() {
		byte[] contentBytes = IOUtils.toUTF8Bytes("v1");

		return new ContentStreamImpl("content.txt", BigInteger.valueOf(contentBytes.length), "text/plain",
				new ByteArrayInputStream(contentBytes));
	}

	private SecondaryTypeDefinition createTypeWithProperties1(Session session, ObjectType secondaryType) {
		// define the type
		SecondaryTypeDefinitionImpl newTypeDef = createSecondaryTypeDefinition(session, "tck:testid_with_properties1",
				secondaryType);
		CreatablePropertyTypes cpt = session.getRepositoryInfo().getCapabilities().getCreatablePropertyTypes();
		// add a property for each creatable property type
		for (PropertyType propType : PropertyType.values()) {
			if (!cpt.canCreate().contains(propType)) {
				continue;
			}
			newTypeDef.addPropertyDefinition(createSecondaryPropertyDefinition(propType));
		}
		// create the type
		ObjectType newType = null;
		try {
			newType = session.createType(newTypeDef);
			addResult(createInfoResult("Created type '" + newType.getId()
					+ "'. Repository assigned the following type ID: " + newType.getId()));
		} catch (CmisBaseException e) {
			addResult(createResult(FAILURE, "Creating type '" + newType.getId() + "' failed: " + e.getMessage(), e,
					false));
			return null;
		}
		// get the type
		SecondaryTypeDefinition docType1 = (SecondaryTypeDefinition) session.getTypeDefinition(newType.getId());
		return docType1;
	}

	private SecondaryTypeDefinitionImpl createSecondaryTypeDefinition(Session session, String typeId,
			ObjectType parentType) {
		CmisTestResult failure = null;
		NewTypeSettableAttributes settableAttributes = session.getRepositoryInfo().getCapabilities()
				.getNewTypeSettableAttributes();
		if (settableAttributes == null) {
			addResult(createResult(WARNING, "Capability NewTypeSettableAttributes is not set!"));
		}
		SecondaryTypeDefinitionImpl result = new SecondaryTypeDefinitionImpl();
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
		return result;
	}

	private AbstractPropertyDefinition<?> createSecondaryPropertyDefinition(PropertyType propertyType) {

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
			setOtherProperties1(result1, propertyType);
			return result1;
		case ID:
			PropertyIdDefinitionImpl result2 = new PropertyIdDefinitionImpl();
			setOtherProperties1(result2, propertyType);
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
			setOtherProperties1(result3, propertyType);
			return result3;
		case DATETIME:
			PropertyDateTimeDefinitionImpl result4 = new PropertyDateTimeDefinitionImpl();
			setOtherProperties(result4, propertyType);
			return result4;
		case DECIMAL:
			PropertyDecimalDefinitionImpl result5 = new PropertyDecimalDefinitionImpl();
			setOtherProperties1(result5, propertyType);
			return result5;
		case HTML:
			PropertyHtmlDefinitionImpl result6 = new PropertyHtmlDefinitionImpl();
			setOtherProperties(result6, propertyType);
			return result6;
		case URI:
			PropertyUriDefinitionImpl result7 = new PropertyUriDefinitionImpl();
			setOtherProperties1(result7, propertyType);
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
			setOtherProperties1(result, propertyType);
			return result;
		}
		return null;
	}

	private void setOtherProperties1(AbstractPropertyDefinition<?> result, PropertyType propertyType) {
		result.setPropertyType(propertyType);
		result.setId("tck:secodary:" + propertyType.value());
		result.setLocalName("tck:secodary:local_" + propertyType.value());
		result.setLocalNamespace("tck:secodary:testlocalnamespace");
		result.setDisplayName("TCK Secondary" + propertyType.value() + " propertry");
		result.setQueryName("tck:secodary:" + propertyType.value());
		result.setDescription("TCK Secondary" + propertyType.value() + " propertry");
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

	private void checkCheckedOut(Document pwc) {
		CmisTestResult f;

		f = createResult(FAILURE, "Version series has a PWC but cmis:isVersionSeriesCheckedOut is not TRUE!");
		addResult(assertIsTrue(pwc.isVersionSeriesCheckedOut(), null, f));

		if (pwc.getVersionSeriesCheckedOutId() == null) {
			addResult(createResult(WARNING, "cmis:versionSeriesCheckedOutId is not set!"));
		} else {
			f = createResult(FAILURE, "PWC id and cmis:versionSeriesCheckedOutId don't match!");
			addResult(assertEquals(pwc.getId(), pwc.getVersionSeriesCheckedOutId(), null, f));
		}

		f = createResult(WARNING, "PWC does not have a value for cmis:versionSeriesCheckedOutBy!");
		addResult(assertStringNotEmpty(pwc.getVersionSeriesCheckedOutBy(), null, f));
	}
}
