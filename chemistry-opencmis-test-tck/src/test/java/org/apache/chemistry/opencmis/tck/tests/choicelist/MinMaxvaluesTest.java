package org.apache.chemistry.opencmis.tck.tests.choicelist;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.SKIPPED;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.UNEXPECTED_EXCEPTION;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.WARNING;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.Item;
import org.apache.chemistry.opencmis.client.api.ObjectType;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.CreatablePropertyTypes;
import org.apache.chemistry.opencmis.commons.data.NewTypeSettableAttributes;
import org.apache.chemistry.opencmis.commons.definitions.Choice;
import org.apache.chemistry.opencmis.commons.definitions.ItemTypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import org.apache.chemistry.opencmis.commons.enums.Cardinality;
import org.apache.chemistry.opencmis.commons.enums.CmisVersion;
import org.apache.chemistry.opencmis.commons.enums.PropertyType;
import org.apache.chemistry.opencmis.commons.enums.Updatability;
import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AbstractPropertyDefinition;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ChoiceImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ItemTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyBooleanDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDateTimeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyDecimalDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyHtmlDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIntegerDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyUriDefinitionImpl;
import org.apache.chemistry.opencmis.tck.CmisTestResult;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;

public class MinMaxvaluesTest extends AbstractSessionTest {
	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("Create Type with ChoiceList");
		setDescription("Creates a Item type with choice list property definition and creates object of that type");
	}

	@Override
	public void run(Session session) throws Exception {
		if (session.getRepositoryInfo().getCmisVersion() == CmisVersion.CMIS_1_0) {
			addResult(createResult(SKIPPED, "Type mutability is not supported by CMIS 1.0. Test skipped!"));
			return;
		}

		ObjectType parentType = session.getTypeDefinition(getItemTestTypeId());
		if (parentType.getTypeMutability() == null
				|| !Boolean.TRUE.equals(parentType.getTypeMutability().canCreate())) {
			addResult(createResult(SKIPPED, "Test document type doesn't allow creating a sub-type. Test skipped!"));
			return;
		}

		createTypeWithProperties(session, parentType);
	}

	private void createTypeWithProperties(Session session, ObjectType parentType) {
		CmisTestResult failure = null;

		CreatablePropertyTypes cpt = session.getRepositoryInfo().getCapabilities().getCreatablePropertyTypes();
		if (cpt == null || cpt.canCreate() == null || cpt.canCreate().isEmpty()) {
			addResult(createResult(FAILURE, "Repository Info does not indicate, which property types can be created!"));
			return;
		}

		// define the type
		ItemTypeDefinitionImpl newTypeDef = createItemTypeDefinition(session, "tck:testid_with_properties", parentType);

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
			return;
		}

		// get the type
		ObjectType newType2 = null;
		try {
			newType2 = session.getTypeDefinition(newType.getId());

			// assert type definitions
			failure = createResult(FAILURE,
					"The type definition returned by createType() doesn't match the type definition returned by getTypeDefinition()!");
			addResult(assertEquals(newType, newType2, null, failure));
		} catch (CmisObjectNotFoundException e) {
			addResult(createResult(FAILURE, "Newly created type can not be fetched. Id: " + newType.getId(), e, false));
		}

		// add object of that type
		// create a test folder
		Folder testFolder = createTestFolder(session);
		Item newItem = null;
		try {
			newItem = createItemtWithCustomProperties(session, testFolder, "createItemPositiveCase", newType.getId());
			// newItem = checkwithMinMaxValues(session, testFolder,
			// "createItemNegativeCase", newType.getId());
		} finally {
			// delete the folder
			// deleteTestFolder();
			// delete the type
			// deleteType(session, newType.getId());
		}
		addResult(createInfoResult("Tested the MinMax values and adding object of that type"));
	}

	private ItemTypeDefinitionImpl createItemTypeDefinition(Session session, String typeId, ObjectType parentType) {
		CmisTestResult failure = null;

		NewTypeSettableAttributes settableAttributes = session.getRepositoryInfo().getCapabilities()
				.getNewTypeSettableAttributes();
		if (settableAttributes == null) {
			addResult(createResult(WARNING, "Capability NewTypeSettableAttributes is not set!"));
		}

		ItemTypeDefinitionImpl result = new ItemTypeDefinitionImpl();

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
			result.setDisplayName("TCK Item Type");
		} else if (settableAttributes != null) {
			failure = createResult(WARNING, "Flag 'displayName' in capability NewTypeSettableAttributes is not set!");
			addResult(assertNotNull(settableAttributes.canSetDisplayName(), null, failure));
		}

		if (settableAttributes == null || Boolean.TRUE.equals(settableAttributes.canSetDescription())) {
			result.setDescription("This is the TCK Item type");
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

		return result;
	}

	private AbstractPropertyDefinition<?> createPropertyDefinition(PropertyType propertyType) {

		switch (propertyType) {
		case BOOLEAN:
			PropertyBooleanDefinitionImpl result1 = new PropertyBooleanDefinitionImpl();
			setOtherProperties(result1, propertyType);
			return result1;
		case ID:
			PropertyIdDefinitionImpl result2 = new PropertyIdDefinitionImpl();
			setOtherProperties(result2, propertyType);
			return result2;
		case INTEGER:
			PropertyIntegerDefinitionImpl result3 = new PropertyIntegerDefinitionImpl();
			result3.setMinValue(new BigInteger("5"));
			result3.setMaxValue(new BigInteger("10"));
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
			result.setMaxLength(new BigInteger("10"));
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
	 * Creates a item.
	 */
	protected Item createItem(Session session, Folder parent, String name) {
		return createItem(session, parent, name, getItemTestTypeId());
	}

	/**
	 * Creates a item.
	 */
	protected Item createItemtWithCustomProperties(Session session, Folder parent, String name, String objectTypeId) {
		if (parent == null) {
			throw new IllegalArgumentException("Parent is not set!");
		}
		if (name == null) {
			throw new IllegalArgumentException("Name is not set!");
		}
		if (objectTypeId == null) {
			throw new IllegalArgumentException("Object Type ID is not set!");
		}

		// check type
		ObjectType type;
		try {
			type = session.getTypeDefinition(objectTypeId);
		} catch (CmisObjectNotFoundException e) {
			addResult(createResult(UNEXPECTED_EXCEPTION,
					"Item type '" + objectTypeId + "' is not available: " + e.getMessage(), e, true));
			return null;
		}

		if (Boolean.FALSE.equals(type.isCreatable())) {
			addResult(createResult(SKIPPED, "Item type '" + objectTypeId + "' is not creatable!", true));
			return null;
		}
		ItemTypeDefinition itemType = (ItemTypeDefinition) type;
		// create
		Map<String, Object> properties = new HashMap<String, Object>();
		properties.put(PropertyIds.NAME, name);
		properties.put(PropertyIds.OBJECT_TYPE_ID, objectTypeId);
		for (Map.Entry<String, PropertyDefinition<?>> propDef : itemType.getPropertyDefinitions().entrySet()) {
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
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.STRING)) {
					properties.put(propDef.getValue().getId(), "987");
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.BOOLEAN)) {
					properties.put(propDef.getValue().getId(), true);
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.INTEGER)) {
					properties.put(propDef.getValue().getId(), new BigInteger("5"));
				}
			}

		}

		Item result = null;
		try {
			// create the item
			result = parent.createItem(properties, null, null, null, SELECT_ALL_NO_CACHE_OC);
		} catch (CmisBaseException e) {
			addResult(createResult(UNEXPECTED_EXCEPTION, "Item could not be created! Exception: " + e.getMessage(), e,
					true));
			return null;
		}

		try {
			CmisTestResult f;

			// check item name
			f = createResult(FAILURE, "Item name does not match!", false);
			addResult(assertEquals(name, result.getName(), null, f));

			addResult(checkObject(session, result, getAllProperties(result), "New item object spec compliance"));
		} catch (CmisBaseException e) {
			addResult(createResult(UNEXPECTED_EXCEPTION, "Newly created item is invalid! Exception: " + e.getMessage(),
					e, true));
		}

		// check parents
		List<Folder> parents = result.getParents(SELECT_ALL_NO_CACHE_OC);
		boolean found = false;
		for (Folder folder : parents) {
			if (parent.getId().equals(folder.getId())) {
				found = true;
				break;
			}
		}

		if (!found) {
			addResult(createResult(FAILURE,
					"The folder the item has been created in is not in the list of the item parents!"));
		}

		return result;
	}

	// protected Item checkwithMinMaxValues(Session session, Folder parent,
	// String name, String objectTypeId) {
	//
	// if (parent == null) {
	// throw new IllegalArgumentException("Parent is not set!");
	// }
	// if (name == null) {
	// throw new IllegalArgumentException("Name is not set!");
	// }
	// if (objectTypeId == null) {
	// throw new IllegalArgumentException("Object Type ID is not set!");
	// }
	//
	// // check type
	// ObjectType type;
	// try {
	// type = session.getTypeDefinition(objectTypeId);
	// } catch (CmisObjectNotFoundException e) {
	// addResult(createResult(UNEXPECTED_EXCEPTION,
	// "Item type '" + objectTypeId + "' is not available: " + e.getMessage(),
	// e, true));
	// return null;
	// }
	//
	// if (Boolean.FALSE.equals(type.isCreatable())) {
	// addResult(createResult(SKIPPED, "Item type '" + objectTypeId + "' is not
	// creatable!", true));
	// return null;
	// }
	// ItemTypeDefinition itemType = (ItemTypeDefinition) type;
	// // create
	// Map<String, Object> properties = new HashMap<String, Object>();
	// properties.put(PropertyIds.NAME, name);
	// properties.put(PropertyIds.OBJECT_TYPE_ID, objectTypeId);
	// for (Map.Entry<String, PropertyDefinition<?>> propDef :
	// itemType.getPropertyDefinitions().entrySet()) {
	// if (!(propDef.getKey().equalsIgnoreCase(PropertyIds.NAME)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFIED_BY)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.OBJECT_TYPE_ID)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.CREATED_BY)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.PATH)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.DESCRIPTION)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.CHANGE_TOKEN)
	// ||
	// propDef.getKey().equalsIgnoreCase(PropertyIds.ALLOWED_CHILD_OBJECT_TYPE_IDS)
	// ||
	// propDef.getKey().equalsIgnoreCase(PropertyIds.SECONDARY_OBJECT_TYPE_IDS)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.PARENT_ID)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.BASE_TYPE_ID)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.OBJECT_ID)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.LAST_MODIFICATION_DATE)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.CREATION_DATE)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_LENGTH)
	// ||
	// propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_FILE_NAME)
	// ||
	// propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_MIME_TYPE)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.CHECKIN_COMMENT)
	// ||
	// propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_BY)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_LABEL)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.IS_MAJOR_VERSION)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.IS_LATEST_VERSION)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.CONTENT_STREAM_ID)
	// ||
	// propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_CHECKED_OUT_ID)
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.VERSION_SERIES_ID)
	// || propDef.getKey().equalsIgnoreCase("cmis:previousVersionObjectId")
	// || propDef.getKey().equalsIgnoreCase(PropertyIds.IS_IMMUTABLE))) {
	// if (propDef.getValue().getPropertyType().equals(PropertyType.ID)) {
	// properties.put(propDef.getValue().getId(), "123");
	// } else if
	// (propDef.getValue().getPropertyType().equals(PropertyType.DATETIME)) {
	// properties.put(propDef.getValue().getId(), new GregorianCalendar());
	// } else if
	// (propDef.getValue().getPropertyType().equals(PropertyType.DECIMAL)) {
	// properties.put(propDef.getValue().getId(), new BigDecimal("0.2"));
	// } else if
	// (propDef.getValue().getPropertyType().equals(PropertyType.HTML)) {
	// properties.put(propDef.getValue().getId(), "<html>");
	// } else if (propDef.getValue().getPropertyType().equals(PropertyType.URI))
	// {
	// properties.put(propDef.getValue().getId(), AbstractRunner.BROWSER_URL);
	// // } else if
	// // (propDef.getValue().getPropertyType().equals(PropertyType.STRING))
	// // {
	// // properties.put(propDef.getValue().getId(), "987");
	// // } else if
	// // (propDef.getValue().getPropertyType().equals(PropertyType.BOOLEAN))
	// // {
	// // properties.put(propDef.getValue().getId(), true);
	// // } else if
	// // (propDef.getValue().getPropertyType().equals(PropertyType.INTEGER))
	// // {
	// // properties.put(propDef.getValue().getId(), new
	// // BigInteger("5"));
	// // }
	// }
	//
	// }
	//
	// Item result = null;
	// CmisTestResult failure = null;
	// try {
	// // create the item
	// result = parent.createItem(properties, null, null, null,
	// SELECT_ALL_NO_CACHE_OC);
	// } catch (CmisBaseException e) {
	// addResult(createResult(UNEXPECTED_EXCEPTION, "Item could not be created!
	// Exception: " + e.getMessage(),
	// e, true));
	// return null;
	// }
	//
	// try {
	// CmisTestResult f;
	//
	// // check item name
	// f = createResult(FAILURE, "Item name does not match!", false);
	// addResult(assertEquals(name, result.getName(), null, f));
	//
	// addResult(checkObject(session, result, getAllProperties(result), "New
	// item object spec compliance"));
	// failure = createResult(FAILURE, "item colud not be created bcz Property
	// is not included in response!");
	// addResult(assertEquals(name, result.getName(), null, f));
	// } catch (CmisBaseException e) {
	// addResult(createResult(UNEXPECTED_EXCEPTION,
	// "Newly created item is invalid! Exception: " + e.getMessage(), e, true));
	// }
	//
	// // check parents
	// List<Folder> parents = result.getParents(SELECT_ALL_NO_CACHE_OC);
	// boolean found = false;
	// for (Folder folder : parents) {
	// if (parent.getId().equals(folder.getId())) {
	// found = true;
	// break;
	// }
	// }
	//
	// if (!found) {
	// addResult(createResult(FAILURE,
	// "The folder the item has been created in is not in the list of the item
	// parents!"));
	// }
	//
	// return result;
	// }
	// return null;
	// }
}
