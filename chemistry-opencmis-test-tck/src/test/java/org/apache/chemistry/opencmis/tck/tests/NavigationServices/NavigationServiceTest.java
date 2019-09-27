package org.apache.chemistry.opencmis.tck.tests.NavigationServices;

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

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.Item;
import org.apache.chemistry.opencmis.client.api.ItemIterable;
import org.apache.chemistry.opencmis.client.api.ObjectType;
import org.apache.chemistry.opencmis.client.api.OperationContext;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.runtime.OperationContextImpl;
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

public class NavigationServiceTest extends AbstractSessionTest {

	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("NavigationService Tests");
		setDescription("Creates a Item type with NavigationService");
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
		int numOfItems = 5;

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
		for (int i = 0; i < numOfItems; i++) {
			createItemtWithCustomProperties(session, testFolder, "itemWithNavigationService" + i, newType.getId(), i);
		}
		queryTesting(testFolder);
		deleteTestFolder();
		deleteType(session, newType.getId());

	}

	@SuppressWarnings("unchecked")
	private void queryTesting(Folder testFolder) {

		// tck:integer equal to 1
		OperationContext context1 = new OperationContextImpl();
		context1.setFilterString("tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:integer eq 1");

		ItemIterable<CmisObject> getChildren1 = testFolder.getChildren(context1);
		addResult(assertEquals(Long.valueOf(1), getChildren1.getTotalNumItems(), null,
				createResult(FAILURE, "Total numItems must be 1")));
		for (CmisObject child : getChildren1) {
			addResult(assertEquals(BigInteger.valueOf(1),
					((ArrayList<Integer>) child.getPropertyValue("tck:integer")).get(0), null,
					createResult(FAILURE, "tck:integer value should be equal to 1")));
		}

		// le 5
//		OperationContext context2 = new OperationContextImpl();
//		context2.setFilterString("tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:integer le 5");
//		ItemIterable<CmisObject> getChildren2 = testFolder.getChildren(context2);
//		addResult(assertEquals(Long.valueOf(5), getChildren2.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 5")));
//		int i = 1;
//		for (CmisObject child : getChildren2) {
//			addResult(assertEquals(BigInteger.valueOf(i),
//					((ArrayList<Integer>) child.getPropertyValue("tck:integer")).get(0), null,
//					createResult(FAILURE, "tck:integer value should be equal to " + i)));
//			i++;
//		}
//
//		// ge 3
//		OperationContext context3 = new OperationContextImpl();
//		context3.setFilterString("tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:integer ge 3");
//		ItemIterable<CmisObject> getChildren3 = testFolder.getChildren(context3);
//		addResult(assertEquals(Long.valueOf(3), getChildren3.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 3")));
//		i = 3;
//		for (CmisObject child : getChildren3) {
//			addResult(assertEquals(BigInteger.valueOf(i),
//					((ArrayList<Integer>) child.getPropertyValue("tck:integer")).get(0), null,
//					createResult(FAILURE, "tck:integer value should be equal to " + i)));
//			i++;
//		}
//
//		// lt 5
//		OperationContext context4 = new OperationContextImpl();
//		context4.setFilterString("tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:integer lt 5");
//		ItemIterable<CmisObject> getChildren4 = testFolder.getChildren(context4);
//		addResult(assertEquals(Long.valueOf(4), getChildren4.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 5")));
//		i = 1;
//		for (CmisObject child : getChildren4) {
//			addResult(assertEquals(BigInteger.valueOf(i),
//					((ArrayList<Integer>) child.getPropertyValue("tck:integer")).get(0), null,
//					createResult(FAILURE, "tck:integer value should be equal to " + i)));
//			i++;
//		}
//
//		// gt 3
//		OperationContext context5 = new OperationContextImpl();
//		context5.setFilterString("tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:integer gt 3");
//		ItemIterable<CmisObject> getChildren5 = testFolder.getChildren(context5);
//		addResult(assertEquals(Long.valueOf(2), getChildren5.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 2")));
//		i = 4;
//		for (CmisObject child : getChildren5) {
//			addResult(assertEquals(BigInteger.valueOf(i),
//					((ArrayList<Integer>) child.getPropertyValue("tck:integer")).get(0), null,
//					createResult(FAILURE, "tck:integer value should be equal to " + i)));
//			i++;
//		}
//
//		// ne 1
//		OperationContext context6 = new OperationContextImpl();
//		context6.setFilterString("tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:integer ne 1");
//		ItemIterable<CmisObject> getChildren6 = testFolder.getChildren(context6);
//		addResult(assertEquals(Long.valueOf(4), getChildren6.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 4")));
//		i = 2;
//		for (CmisObject child : getChildren6) {
//			addResult(assertEquals(BigInteger.valueOf(i),
//					((ArrayList<Integer>) child.getPropertyValue("tck:integer")).get(0), null,
//					createResult(FAILURE, "tck:integer value should be equal to " + i)));
//			i++;
//		}
//		// tck:string eq GHI
//		OperationContext context7 = new OperationContextImpl();
//		context7.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and tck:string eq GHI");
//		ItemIterable<CmisObject> getChildren7 = testFolder.getChildren(context7);
//		addResult(assertEquals(Long.valueOf(1), getChildren7.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 1")));
//		for (CmisObject child : getChildren7) {
//			addResult(assertEquals("GHI", ((ArrayList<Integer>) child.getPropertyValue("tck:string")).get(0), null,
//					createResult(FAILURE, "tck:string value should be equal to GHI")));
//		}
//
//		// ne GHI
//		OperationContext context8 = new OperationContextImpl();
//		context8.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and tck:string ne GHI");
//		ItemIterable<CmisObject> getChildren8 = testFolder.getChildren(context8);
//		addResult(assertEquals(Long.valueOf(4), getChildren8.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 4")));
//
//		// eq ABC DEF
//		OperationContext context9 = new OperationContextImpl();
//		context9.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and tck:string eq 'ABC DEF'");
//		ItemIterable<CmisObject> getChildren9 = testFolder.getChildren(context9);
//		addResult(assertEquals(Long.valueOf(1), getChildren9.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 1")));
//
//		// boolean true
//		OperationContext context10 = new OperationContextImpl();
//		context10.setFilterString("tck:boolean,cmis:objectTypeId eq tck:testid_with_properties and tck:boolean eq true");
//		ItemIterable<CmisObject> getChildren10 = testFolder.getChildren(context10);
//		addResult(assertEquals(Long.valueOf(5), getChildren10.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be true")));
//
//		// boolean false
//		OperationContext context11 = new OperationContextImpl();
//		context11.setFilterString("tck:boolean,cmis:objectTypeId eq tck:testid_with_properties and tck:boolean eq false");
//		ItemIterable<CmisObject> getChildren11 = testFolder.getChildren(context11);
//		addResult(assertEquals(Long.valueOf(0), getChildren11.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be false")));
//
//		// checking and operator
//		OperationContext context12 = new OperationContextImpl();
//		context12.setFilterString("tck:string,tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:string eq GHI and tck:integer eq 2");
//		ItemIterable<CmisObject> getChildren12 = testFolder.getChildren(context12);
//		addResult(assertEquals(Long.valueOf(1), getChildren12.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 1")));
//
//		// checking or operator
//		OperationContext context13 = new OperationContextImpl();
//		context13.setFilterString("tck:string,tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:string ne GHI or tck:integer eq 1");
//		ItemIterable<CmisObject> getChildren13 = testFolder.getChildren(context13);
//		addResult(assertEquals(Long.valueOf(4), getChildren13.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 4")));
//
//		// checking multiple and operator
//		OperationContext context14 = new OperationContextImpl();
//		context14.setFilterString("tck:string,tck:integer,cmis:objectTypeId eq tck:testid_with_properties and tck:string ne GHI and tck:integer eq 4 and tck:integer gt 3");
//		ItemIterable<CmisObject> getChildren14 = testFolder.getChildren(context14);
//		addResult(assertEquals(Long.valueOf(1), getChildren14.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 1")));
//
//		// * gives all irrespective of filter
//		OperationContext context15 = new OperationContextImpl();
//		context15.setFilterString("*,cmis:objectTypeId eq tck:testid_with_properties and tck:string ne 'ABC DEF'");
//		ItemIterable<CmisObject> getChildren15 = testFolder.getChildren(context15);
//		addResult(assertEquals(Long.valueOf(5), getChildren15.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 5")));
//
//		OperationContext context16 = new OperationContextImpl();
//		context16.setFilterString("cmis:objectTypeId eq tck:testid_with_properties and tck:string ne 'ABC DEF'");
//		ItemIterable<CmisObject> getChildren16 = testFolder.getChildren(context16);
//		addResult(assertEquals(Long.valueOf(4), getChildren16.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 4")));
//
//		// checking multiple or operator
//		OperationContext context17 = new OperationContextImpl();
//		context17.setFilterString(
//				"tck:string,tck:integer,tck:boolean,cmis:objectTypeId eq tck:testid_with_properties and tck:string ne GHI or tck:string eq 'ABC DEF' or tck:integer eq 1 or tck:boolean eq true");
//		ItemIterable<CmisObject> getChildren17 = testFolder.getChildren(context17);
//		addResult(assertEquals(Long.valueOf(1), getChildren17.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 5")));
//
//		// eq STU/VW
//		OperationContext context18 = new OperationContextImpl();
//		context18.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and tck:string eq 'STU/VW'");
//		ItemIterable<CmisObject> getChildren18 = testFolder.getChildren(context18);
//		addResult(assertEquals(Long.valueOf(0), getChildren18.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 0")));
//
//		// startsWith
//		OperationContext context19 = new OperationContextImpl();
//		context19.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and startswith (tck:string::'A')");
//		ItemIterable<CmisObject> getChildren19 = testFolder.getChildren(context19);
//		addResult(assertEquals(Long.valueOf(1), getChildren19.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 1")));
//
//		// contains
//		OperationContext context20 = new OperationContextImpl();
//		context20.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and contains (tck:string::'ABC')");
//		ItemIterable<CmisObject> getChildren20 = testFolder.getChildren(context20);
//		addResult(assertEquals(Long.valueOf(1), getChildren20.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 1")));
//
//		// endsWith
//		OperationContext context21 = new OperationContextImpl();
//		context21.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and endswith (tck:string::'I')");
//		ItemIterable<CmisObject> getChildren21 = testFolder.getChildren(context21);
//		addResult(assertEquals(Long.valueOf(1), getChildren21.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 1")));
//
//		// exists -> tck:string exists for 5 items
//		OperationContext context22 = new OperationContextImpl();
//		context22.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and exists (tck:string::'TRUE')");
//		ItemIterable<CmisObject> getChildren22 = testFolder.getChildren(context22);
//		addResult(assertEquals(Long.valueOf(5), getChildren22.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 5")));
//
//		// exists set to false -> tck:string exists for 5 items
//		OperationContext context23 = new OperationContextImpl();
//		context23.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and exists (tck:string::'FALSE')");
//		ItemIterable<CmisObject> getChildren23 = testFolder.getChildren(context23);
//		addResult(assertEquals(Long.valueOf(0), getChildren23.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 0")));
//
//		// exists set to true -> tck:strings does not exists any any items
//		OperationContext context24 = new OperationContextImpl();
//		context24.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and exists (tck:strings::'TRUE')");
//		ItemIterable<CmisObject> getChildren24 = testFolder.getChildren(context24);
//		addResult(assertEquals(Long.valueOf(0), getChildren24.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 0")));
//
//		// exists set to false -> tck:strings does not exists for any items
//		OperationContext context25 = new OperationContextImpl();
//		context25.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and exists (tck:strings::'FALSE')");
//		ItemIterable<CmisObject> getChildren25 = testFolder.getChildren(context25);
//		addResult(assertEquals(Long.valueOf(5), getChildren25.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 5")));
//
//		// tck:string eq ''
//		OperationContext context26 = new OperationContextImpl();
//		context26.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and tck:string eq ''");
//		ItemIterable<CmisObject> getChildren26 = testFolder.getChildren(context26);
//		addResult(assertEquals(Long.valueOf(0), getChildren26.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 0")));
//		for (CmisObject child : getChildren26) {
//			addResult(assertEquals("", ((ArrayList<Integer>) child.getPropertyValue("tck:string")).get(0), null,
//					createResult(FAILURE, "tck:string value should be equal to ''")));
//		}
//
//		// tck:string eq null
//		OperationContext context27 = new OperationContextImpl();
//		context27.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and tck:string eq null");
//		ItemIterable<CmisObject> getChildren27 = testFolder.getChildren(context27);
//		addResult(assertEquals(Long.valueOf(0), getChildren27.getTotalNumItems(), null,
//				createResult(FAILURE, "Total numItems must be 0")));
//		for (CmisObject child : getChildren27) {
//			addResult(assertEquals("", ((ArrayList<Integer>) child.getPropertyValue("tck:string")).get(0), null,
//					createResult(FAILURE, "tck:string value should be equal to ''")));
//		}
//
//		// fetch given properties
//		OperationContext context28 = new OperationContextImpl();
//		context28.setFilterString("tck:string,cmis:objectTypeId eq tck:testid_with_properties and tck:integer,tck:boolean");
//		ItemIterable<CmisObject> getChildren28 = testFolder.getChildren(context28);
//		// int k = 0;
//		int count = 1;
//		int k = 0;
//		for (CmisObject child : getChildren28) {
//			String s = child.getProperties().get(k).getId();
//			if (!(s.equals(PropertyIds.BASE_TYPE_ID) || s.equals(PropertyIds.OBJECT_TYPE_ID)
//					|| s.equals(PropertyIds.OBJECT_ID))) {
//				count++;
//			}
//			k++;
//		}
//		addResult(assertEquals(3, count, null, createResult(FAILURE, "Total properties is 3")));
//		addResult(createInfoResult("Navigation Service with filter queries Testing Done"));
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
			choiceValues3.add(BigInteger.valueOf(3));
			choiceValues3.add(BigInteger.valueOf(4));
			choiceValues3.add(BigInteger.valueOf(5));
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
			choiceValues.add("ABC DEF");
			choiceValues.add("GHI");
			choiceValues.add("JKL");
			choiceValues.add("MNO");
			choiceValues.add("PQR");
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
	 * Creates a item.
	 */
	protected Item createItem(Session session, Folder parent, String name) {
		return createItem(session, parent, name, getItemTestTypeId());
	}

	/**
	 * Creates a item.
	 */
	protected Item createItemtWithCustomProperties(Session session, Folder parent, String name, String objectTypeId,
			int i) {
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
			if (propDef.getValue().getChoices().size() > 0) {
				if (propDef.getValue().getPropertyType().equals(PropertyType.BOOLEAN)) {
					List<Boolean> booleanList = new ArrayList<Boolean>();
					booleanList.add((Boolean) propDef.getValue().getChoices().get(0).getValue().get(0));
					properties.put(propDef.getValue().getId(), booleanList);
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.STRING)) {
					List<String> stringList = new ArrayList<String>();
					stringList.add((String) propDef.getValue().getChoices().get(0).getValue().get(i));
					properties.put(propDef.getValue().getId(), stringList);
				} else if (propDef.getValue().getPropertyType().equals(PropertyType.INTEGER)) {
					List<BigInteger> intList = new ArrayList<BigInteger>();
					intList.add((BigInteger) propDef.getValue().getChoices().get(0).getValue().get(i));
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
}
