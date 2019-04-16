package org.apache.chemistry.opencmis.tck.aclplugintest;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.INFO;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.AsyncSession;
import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.Item;
import org.apache.chemistry.opencmis.client.api.ObjectId;
import org.apache.chemistry.opencmis.client.api.ObjectType;
import org.apache.chemistry.opencmis.client.api.OperationContext;
import org.apache.chemistry.opencmis.client.api.Property;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.ObjectIdImpl;
import org.apache.chemistry.opencmis.client.runtime.OperationContextImpl;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.client.runtime.async.AsyncSessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.commons.impl.JSONConverter;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParseException;
import org.apache.chemistry.opencmis.commons.impl.json.parser.JSONParser;
import org.apache.chemistry.opencmis.tck.CmisTestResult;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

public class AclTypeAndItemCreationTest extends AbstractSessionTest {
	CmisTestResult failure;
	String object = null;
	String templateId = null;
	String objectId = null;
	String assignToHierarchyId = null;
	String objectId11 = null;

	@SuppressWarnings("unchecked")
	@Override
	public void run(Session session1) throws Exception {
		Session session = null;
		String user1 = "fdf37256-23f6-409b-9b26-0ec74aece2ea";
		String password1 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJmZGYzNzI1Ni0yM2Y2LTQwOWItOWIyNi0wZWM3NGFlY2UyZWEiLCJjb2duaXRvOmdyb3VwcyI6WyJUZXN0VGVuYW50X0NvbXBhbnkiLCJUZXN0VGVuYW50X0NvbXBhbnlfQURNSU4iXSwiZW1haWxfdmVyaWZpZWQiOnRydWUsImN1c3RvbTphZGRyZXNzIjoiTGF2ZWxsZSBtYW5zaW9uIiwiY3VzdG9tOmxhc3ROYW1lIjoiTSIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6ImZkZjM3MjU2LTIzZjYtNDA5Yi05YjI2LTBlYzc0YWVjZTJlYSIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiRmFoYWQiLCJjdXN0b206cHJlZmVycmVkTGFuZyI6ImVuIiwiY3VzdG9tOnRlbmFudElkIjoiMTRjOTFkNzAtMzliMS0xMWU5LWE4NTktOWY3ZWNiYzMyOTRjIiwiY29nbml0bzpyb2xlcyI6WyJhcm46YXdzOmlhbTo6NTI2ODIxNTM3NDY2OnJvbGVcL0NvZ25pdG9fVGVuYW50SURQb29sVW5hdXRoX1JvbGUiXSwiYXVkIjoiN2Nha2gxYW9odXVwaTd1OTRqMzI5ZzRzZ3QiLCJldmVudF9pZCI6IjAxZjQ5OTllLTUwNGEtMTFlOS04NTZlLTYxN2ZmYzMxNzM4OSIsImN1c3RvbTpmaXJzdE5hbWUiOiJTaGl2YXJhaiIsInRva2VuX3VzZSI6ImlkIiwiYXV0aF90aW1lIjoxNTUzNjYxNDQwLCJjdXN0b206Y29tcGFueU5hbWUiOiJUZXN0VGVuYW50IiwiZXhwIjoxNTUzNjY1MDQwLCJpYXQiOjE1NTM2NjE0NDAsImVtYWlsIjoic2hpdmFyYWoubUBwb2dleWFuLmNvbSJ9.tneBLCpM4S3dKCKLAqbXiuzL2ssg92ISPp2xHPlThAECB1s6abTPs6X_APpCJntpK954vYqEFZ5S58jY1PnniIlZcEbWY6S73uushS58yhC2JYyQGvSGbsPyh8hvXUdibuDP5XS6QWJ8hM_HXdVvYCCFrpjE6nntReDnDDrGp8kVLoHwbI51Qfb0MDHCSPWGlAbAPUVXBRCmoloSAiup7-hptULLnVuXMTeVd5yw4tf1lVsjMfVnW3_EjnD3vBmnHff8Kt_oFR3T6uCI55x0LBNiJpYhvjNO0bjNXfUk6qrrfrocceH0DKC6lANmcRuHmhvL_PoMghCNxqLqlA7EwQ";

		try {
			session = createsession(user1, password1);
		} catch (Exception e) {
			addResult(createResult(INFO, "CreateSession failed for " + user1));
		}
		try {

			JSONObject itemJson = (JSONObject) new JSONParser()
					.parse(new InputStreamReader(this.getClass().getResourceAsStream("/fv_item.json")));
			TypeDefinition item = JSONConverter.convertTypeDefinition((Map<String, Object>) itemJson);
			ObjectType fv_item = session.createType(item);

			JSONObject partyJson = (JSONObject) new JSONParser()
					.parse(new InputStreamReader(this.getClass().getResourceAsStream("/fv_party.json")));
			TypeDefinition party = JSONConverter.convertTypeDefinition((Map<String, Object>) partyJson);
			ObjectType fv_party = session.createType(party);

			JSONObject topdownJson = (JSONObject) new JSONParser()
					.parse(new InputStreamReader(this.getClass().getResourceAsStream("/fv_topdown.json")));
			TypeDefinition topdown = JSONConverter.convertTypeDefinition((Map<String, Object>) topdownJson);
			ObjectType fv_topdown = session.createType(topdown);

			JSONObject projectJson = (JSONObject) new JSONParser()
					.parse(new InputStreamReader(this.getClass().getResourceAsStream("/project.json")));
			TypeDefinition project = JSONConverter.convertTypeDefinition((Map<String, Object>) projectJson);
			ObjectType project1 = session.createType(project);

			JSONObject templateJson = (JSONObject) new JSONParser()
					.parse(new InputStreamReader(this.getClass().getResourceAsStream("/template.json")));
			TypeDefinition template = JSONConverter.convertTypeDefinition((Map<String, Object>) templateJson);
			ObjectType template1 = session.createType(template);

			JSONObject revisionJson = (JSONObject) new JSONParser()
					.parse(new InputStreamReader(this.getClass().getResourceAsStream("/revision.json")));
			TypeDefinition revision1 = JSONConverter.convertTypeDefinition((Map<String, Object>) revisionJson);
			ObjectType revision = session.createType(revision1);

			Map<String, Object> props = new HashMap<String, Object>();
			props.put(PropertyIds.OBJECT_TYPE_ID, "cmis_ext:relationmd");
			props.put(PropertyIds.NAME, "checky:project_checky:template");
			props.put("cmis:name", "checky:project_checky:template");
			props.put("source_table", "checky:project");
			props.put("target_table", "checky:template");
			String id = session.getObjectByPath("/cmis_ext:relationmd").getId();
			session.createItem(props, new ObjectIdImpl(id));
			// resetcache(session.getRepositoryInfo().getId());

			JSONObject jsonobj1 = createHierarchyObject(session.getRepositoryInfo().getId(), "checky:project", "p1",
					"0", "2cbf6015-1a47-4f03-be41-56fc243f109e");
			LinkedHashMap<Object, Object> ob = (LinkedHashMap<Object, Object>) jsonobj1.get("succinctProperties");
			ArrayList<String> objects = (ArrayList<String>) ob.get("cmis:objectId");
			String objectIdc = objects.get(0);
			ArrayList<Object> item3 = new ArrayList<Object>();
			item3.add(objectIdc);

			JSONObject jsonObj1 = createHierarchyObject(session.getRepositoryInfo().getId(), "checky:project", "p1.1",
					"p1", "10ebd8fe-0e58-40ff-8b88-39fc21654dd5");
			LinkedHashMap<Object, Object> objs = (LinkedHashMap<Object, Object>) jsonObj1.get("succinctProperties");
			ArrayList<String> objectids = (ArrayList<String>) objs.get("cmis:objectId");
			objectId11 = objectids.get(0);
			ArrayList<Object> item1 = new ArrayList<Object>();
			item1.add(objectId11);
			//
			JSONObject jsonObj = createHierarchyObject(session.getRepositoryInfo().getId(), "checky:project", "p1.1.1",
					"p1.1", "fdf37256-23f6-409b-9b26-0ec74aece2ea");

			LinkedHashMap<Object, Object> obj = (LinkedHashMap<Object, Object>) jsonObj.get("succinctProperties");
			ArrayList<String> objectIds = (ArrayList<String>) obj.get("cmis:objectId");
			objectId = objectIds.get(0);
			ArrayList<Object> item2 = new ArrayList<Object>();
			item2.add(objectId);

			JSONObject jsonObj2 = createHierarchyObject(session.getRepositoryInfo().getId(), "checky:project", "p1.2",
					"p1", "8cfafbf5-0ca4-4fce-8249-7e96e61e74dc");
			LinkedHashMap<Object, Object> obj2 = (LinkedHashMap<Object, Object>) jsonObj2.get("succinctProperties");
			ArrayList<String> objectIds2 = (ArrayList<String>) obj2.get("cmis:objectId");
			String objectId2 = objectIds2.get(0);
			ArrayList<Object> item4 = new ArrayList<Object>();
			item4.add(objectId2);

			createHierarchyObject(session.getRepositoryInfo().getId(), "checky:project", "p1.2.1", "p1.2",
					"d9094d03-2272-4d5a-a9dc-2b92c92fd6af");

			// // reset the cache once all the hierarchyObjects are created
			resetcache(session.getRepositoryInfo().getId());

			String user_1 = "10ebd8fe-0e58-40ff-8b88-39fc21654dd5";
			String password_1 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJjdXN0b206emlwQ29kZSI6IjYzMiIsImN1c3RvbTpjb3VudHJ5IjoiaW5kaWEiLCJzdWIiOiIxMGViZDhmZS0wZTU4LTQwZmYtOGI4OC0zOWZjMjE2NTRkZDUiLCJjb2duaXRvOmdyb3VwcyI6WyJpYnJhaGltX2tfcG9nZXlhbl9jb21fQ29tcGFueSIsIlRlbmFudEdyb3VwIl0sImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJjdXN0b206YWRkcmVzcyI6IjMsc3RyZWV0LHl5IiwiY3VzdG9tOmxhc3ROYW1lIjoiUG9ucyIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6IjEwZWJkOGZlLTBlNTgtNDBmZi04Yjg4LTM5ZmMyMTY1NGRkNSIsImN1c3RvbTpzdGF0ZSI6InRhbWlsbmFkdSIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiZmFpc2FsIiwiY3VzdG9tOnRlbmFudElkIjoiMTJhZmM2NDAtM2MxNS0xMWU5LWJlZGQtYmI0M2IzYzQ5NDcyIiwiY29nbml0bzpyb2xlcyI6WyJhcm46YXdzOmlhbTo6NTI2ODIxNTM3NDY2OnJvbGVcL0NvZ25pdG9fVGVuYW50SURQb29sVW5hdXRoX1JvbGUiXSwiYXVkIjoiN2Nha2gxYW9odXVwaTd1OTRqMzI5ZzRzZ3QiLCJldmVudF9pZCI6IjU2YzNjN2NmLTQ5ZmYtMTFlOS04NjVmLWY5MTQxNjViNGM0YyIsImN1c3RvbTpmaXJzdE5hbWUiOiJtZCIsInRva2VuX3VzZSI6ImlkIiwiY3VzdG9tOnBob25lTnVtYmVyIjoiNzM1ODIxNjczMyIsImF1dGhfdGltZSI6MTU1Mjk2OTY2MywiY3VzdG9tOmNvbXBhbnlOYW1lIjoiaWJyYWhpbV9rX3BvZ2V5YW5fY29tIiwiZXhwIjoxNTUyOTczMjYzLCJpYXQiOjE1NTI5Njk2NjMsImVtYWlsIjoiaWJyYWhpbS5rQHBvZ2V5YW4uY29tIn0.nY3_grss7G2JIeJ0Ro3mcBhbikY9SEGuhWeMSaLoEbYidPW93X4AYuE28aSAm8usdoo0HBUJUTtK-JfsWuaKjLT3m3gtvUskbU3sPqOS-TmH-iNKRGD1y6RYXFExKOB6ABCcqDP8Z2PpePzyQNwBt-qmljn4IRFTNjrM_Ui4cEmr7F6KP-sCqAqVueGFz_nbTTDWyzPYiouVaCJOdExnBOUU0jzt19u3RjGWgAAOXTvBazoU8CTAjRNijYjQrLNSJ7JRI07tTXkcEhXRevJYpW8PElXISgCBYZXejsKXfMhSxKo2IS3WKixztvpKvyg28GRnLEjh52TJC5FZxtOklA";
			Session session_user1 = createsession(user_1, password_1);
			CmisObject object1 = session_user1.getObject(objectId);
			if (object1 != null) {
				addResult(createResult(INFO,
						"the ace's are updated and hierarchyobject can be accessed by "
								+ session_user1.getSessionParameters().get(SessionParameter.USER) + " parent of "
								+ session.getSessionParameters().get(SessionParameter.USER)));
			}

			String user_2 = "2cbf6015-1a47-4f03-be41-56fc243f109e";
			String password_2 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiIyY2JmNjAxNS0xYTQ3LTRmMDMtYmU0MS01NmZjMjQzZjEwOWUiLCJjb2duaXRvOmdyb3VwcyI6WyJzaGVldjI3OTRfZ21haWxfY29tX0NvbXBhbnkiLCJzaGVldjI3OTRfZ21haWxfY29tX0NvbXBhbnlfQURNSU4iXSwiZW1haWxfdmVyaWZpZWQiOnRydWUsImN1c3RvbTphZGRyZXNzIjoiTGF2ZWxsZSBtYW5zaW9uIiwiY3VzdG9tOmxhc3ROYW1lIjoiSyIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6IjJjYmY2MDE1LTFhNDctNGYwMy1iZTQxLTU2ZmMyNDNmMTA5ZSIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiRmFoYWQiLCJjdXN0b206dGVuYW50SWQiOiJiMDhkZDFjMC0yNjBmLTExZTktOGJiMi0yYmE1YmI5MjE3OGYiLCJjb2duaXRvOnJvbGVzIjpbImFybjphd3M6aWFtOjo1MjY4MjE1Mzc0NjY6cm9sZVwvQ29nbml0b19UZW5hbnRJRFBvb2xVbmF1dGhfUm9sZSJdLCJhdWQiOiI3Y2FraDFhb2h1dXBpN3U5NGozMjlnNHNndCIsImV2ZW50X2lkIjoiZTRmMmU3NDktNWNkZS0xMWU5LWFiNzEtOGQ1OWY5M2MyNmUxIiwiY3VzdG9tOmZpcnN0TmFtZSI6IktvdGlyYXRuYW0iLCJ0b2tlbl91c2UiOiJpZCIsImF1dGhfdGltZSI6MTU1NTA0NDgwMCwiY3VzdG9tOmNvbXBhbnlOYW1lIjoic2hlZXYyNzk0X2dtYWlsX2NvbSIsImV4cCI6MTU1NTA0ODQwMCwiaWF0IjoxNTU1MDQ0ODAwLCJlbWFpbCI6ImtvdGlyYXRuYW1rb3RpQGdtYWlsLmNvbSJ9.V8S6kwvWwJtobSylE9CdCG1ue-EU36YN1G1fkrjVlszkN_nfK2DURKHDZ8dFsE5GphkzbLR3CrGxe989PEmDzqvIbgSgivU_9L0kjTykhzVpnLyZYpi-zgqFZAP8XB3IdGfF5XYRWXRUldBNtJR6iXR6F4_ut04Ki_Bzn0k-EJBZWOlPn06cT3tPs3E-xSZP1ZEdOjB0tzUOw71If4NBljnHx0DxMSfk4fC-UMTLLe4WuON0vhfrtag9_RCjmbSQOlMnRx42cn88TXebkONcWDj2hKt2Nr9dmbF9o2GrSr34JkN8spzvaJiQhogFt6o66DOvlkl0bRYZP38Hs3BOYw";
			Session session2_user2 = createsession(user_2, password_2);
			CmisObject object2 = session2_user2.getObject(objectId);
			if (object2 != null) {
				addResult(createResult(INFO,
						"the ace's are updated and hierarchyobject can be accessed by "
								+ session2_user2.getSessionParameters().get(SessionParameter.USER) + " parentof "
								+ session_user1.getSessionParameters().get(SessionParameter.USER)));
			}

			// // create template object

			Map<String, Object> props6 = new HashMap<String, Object>();
			props6.put(PropertyIds.OBJECT_TYPE_ID, "checky:template");
			props6.put(PropertyIds.OBJECT_ID, "123456789");
			props6.put(PropertyIds.NAME, "SwathiA_TemplateA");
			props6.put("checky:createdOn", "2019-03-21T10:59:15.224Z");
			props6.put("checky:name", "SwathiTemplateA");
			props6.put("checky:author", "Swathi");
			props6.put("checky:template_headerSection",
					"[{\"id\":\"c3662769-6278-4513-032d-eec0027be80c\",\"sectionType\":\"SingleLineText\",\"sectionOrder\":1,\"labelText\":\"Documentno.\",\"cardState\":\"Default\",\"lastModifiedDate\":1552993155,\"isMandatory\":false,\"enableClone\":true,\"canDelete\":true,\"categoryType\":\"DocumentNo.\",\"inputType\":\"text\",\"section_options\":[{\"id\":\"db52c7ba-1077-d35e-3002-b5014fd7fa98\",\"min\":0,\"max\":0,\"prefix\":\"\",\"suffix\":\"\",\"formula\":\"\",\"isRangeInput\":false,\"useDecimal\":false,\"useMaskText\":false,\"numericMaskType\":\"pattern\",\"useMaskNumeric\":false,\"useFormula\":false,\"textPattern\":\"\",\"numericPattern\":\"\",\"isGuided\":true,\"isKeepCharPositions\":false,\"maskPlaceHolderChar\":\"_\",\"lastModifiedDate\":1552993155,\"relatedTo\":-1,\"hasNumericError\":false}],\"section_id\":\"H_S_1\"},{\"id\":\"e8d7559e-13a8-5e6f-90b3-7dbbf898f995\",\"sectionType\":\"SingleLineText\",\"sectionOrder\":1,\"labelText\":\"Audittitle\",\"cardState\":\"Default\",\"lastModifiedDate\":1552993155,\"isMandatory\":true,\"enableClone\":true,\"canDelete\":false,\"categoryType\":\"AuditTitle\",\"inputType\":\"text\",\"section_options\":[{\"id\":\"9913b339-8f03-fa78-f21e-6f2488d2478a\",\"min\":0,\"max\":0,\"prefix\":\"\",\"suffix\":\"\",\"formula\":\"\",\"isRangeInput\":false,\"useDecimal\":false,\"useMaskText\":false,\"numericMaskType\":\"pattern\",\"useMaskNumeric\":false,\"useFormula\":false,\"textPattern\":\"\",\"numericPattern\":\"\",\"isGuided\":true,\"isKeepCharPositions\":false,\"maskPlaceHolderChar\":\"_\",\"lastModifiedDate\":1552993155,\"relatedTo\":-1,\"hasNumericError\":false}],\"section_id\":\"H_S_2\"},{\"id\":\"0b341284-19af-60e4-6a52-c92811ad65ed\",\"sectionType\":\"SingleLineText\",\"sectionOrder\":1,\"labelText\":\"Preparedby\",\"cardState\":\"Default\",\"lastModifiedDate\":1552993155,\"isMandatory\":false,\"enableClone\":true,\"canDelete\":true,\"categoryType\":\"Preparedby\",\"inputType\":\"text\",\"section_options\":[{\"id\":\"d7ad525e-190e-f40f-a8f8-1f605e56271e\",\"min\":0,\"max\":0,\"prefix\":\"\",\"suffix\":\"\",\"formula\":\"\",\"isRangeInput\":false,\"useDecimal\":false,\"useMaskText\":false,\"numericMaskType\":\"pattern\",\"useMaskNumeric\":false,\"useFormula\":false,\"textPattern\":\"\",\"numericPattern\":\"\",\"isGuided\":true,\"isKeepCharPositions\":false,\"maskPlaceHolderChar\":\"_\",\"lastModifiedDate\":1552993155,\"relatedTo\":-1,\"hasNumericError\":false}],\"section_id\":\"H_S_3\"},{\"id\":\"a99dd866-fd83-cda0-f0a5-54a615899dfe\",\"sectionType\":\"SingleLineText\",\"sectionOrder\":1,\"labelText\":\"Location\",\"cardState\":\"Default\",\"lastModifiedDate\":1552993155,\"isMandatory\":false,\"enableClone\":true,\"canDelete\":true,\"categoryType\":\"Location\",\"inputType\":\"text\",\"section_options\":[{\"id\":\"c373222c-9ac7-7112-46cf-2b4d48dfb6f7\",\"min\":0,\"max\":0,\"prefix\":\"\",\"suffix\":\"\",\"formula\":\"\",\"isRangeInput\":false,\"useDecimal\":false,\"useMaskText\":false,\"numericMaskType\":\"pattern\",\"useMaskNumeric\":false,\"useFormula\":false,\"textPattern\":\"\",\"numericPattern\":\"\",\"isGuided\":true,\"isKeepCharPositions\":false,\"maskPlaceHolderChar\":\"_\",\"lastModifiedDate\":1552993155,\"relatedTo\":-1,\"hasNumericError\":false}],\"section_id\":\"H_S_4\"}]");
			props6.put("checky:template_pages",
					"[{\"sectionName\":\"Inspection\",\"pages_sections\":[],\"id\":\"4647608e-a85a-8e85-17a7-db225eacff5d\",\"lastModifiedDate\":1552993155}]");
			props6.put("cmis:secondaryObjectTypeIds", Arrays.asList("cmis:revision"));
			props6.put("checky:last_section_Id", 4);
			props6.put("checky:isDeleted", false);
			props6.put("checky:headerSectionName", "Header");
			props6.put("type", "checky:template");
			props6.put("revisionId", Arrays.asList("1-16b9c50b8gf242gc988ddb2930773f8d"));
			props6.put("checky:templateId", "Testing HierarchyB");
			props6.put("checky:template_answerGroups",
					"[{\"answerGroups_options\":[{\"id\":\"eceed32a-c00a-c75d-c839-08fbfc5a910a\",\"optionId\":1,\"text\":\"Yes\",\"canDelete\":false,\"value\":1,\"color\":\"#01D23C\",\"lastModifiedDate\":1552993155},{\"id\":\"589880bc-dec1-9282-feed-0270c810aa86\",\"optionId\":2,\"text\":\"No\",\"value\":0,\"color\":\"#FF524F\",\"canDelete\":false,\"lastModifiedDate\":1552993155},{\"id\":\"bd76d153-9258-c25c-c319-60f34742eac9\",\"optionId\":3,\"text\":\"NA\",\"value\":-1,\"canDelete\":false,\"color\":\"grey\",\"lastModifiedDate\":1552993155}],\"displayText\":\"Yes-No-NA\",\"answerGroupId\":1,\"canDelete\":false,\"id\":\"52580853-0e82-4ef7-4618-73c41edca63e\",\"lastModifiedDate\":1552993155},{\"answerGroups_options\":[{\"id\":\"873644c4-19ad-403c-89e7-4e164da74849\",\"optionId\":1,\"text\":\"High\",\"value\":1,\"canDelete\":false,\"color\":\"#01D23C\",\"lastModifiedDate\":1552993155},{\"id\":\"7e94f15c-143e-4f16-2f07-61ae5011c631\",\"optionId\":2,\"text\":\"Medium\",\"value\":0.5,\"color\":\"#FF524F\",\"canDelete\":false,\"lastModifiedDate\":1552993155},{\"id\":\"e0826d14-0435-8d7b-bbe5-c1c4b8cb10b4\",\"optionId\":3,\"text\":\"Low\",\"value\":0,\"color\":\"grey\",\"canDelete\":false,\"lastModifiedDate\":1552993155}],\"displayText\":\"High-Medium-Low\",\"answerGroupId\":2,\"canDelete\":false,\"id\":\"8338540e-7853-b331-9fcf-cad0c6879627\",\"lastModifiedDate\":1552993155}]");
			props6.put("checky:description", "SwathiTemplateA");
			props6.put("checky:userId", "2661ae73-432c-4e24-bd97-5b1376ce10fd");
			props6.put("checky:changedOn", "2019-03-19T10:59:15.225Z");
			props6.put("hasAttachments", true);
			String id5 = session.getObjectByPath("/checky:template").getId();
			ObjectId newItem = session.createItem(props6, new ObjectIdImpl(id5));
			ArrayList<Object> docIds = new ArrayList<Object>();
			docIds.add(newItem);
			templateId = newItem.getId();
			resetcache(session.getRepositoryInfo().getId());

			// // assignToHierarchy
			try {

				assignToHierarchy(session.getRepositoryInfo().getId(), item1, templateId, objectId11);
				//
				String user2 = "10ebd8fe-0e58-40ff-8b88-39fc21654dd5";
				String password2 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJjdXN0b206emlwQ29kZSI6IjYzMiIsImN1c3RvbTpjb3VudHJ5IjoiaW5kaWEiLCJzdWIiOiIxMGViZDhmZS0wZTU4LTQwZmYtOGI4OC0zOWZjMjE2NTRkZDUiLCJjb2duaXRvOmdyb3VwcyI6WyJpYnJhaGltX2tfcG9nZXlhbl9jb21fQ29tcGFueSIsIlRlbmFudEdyb3VwIl0sImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJjdXN0b206YWRkcmVzcyI6IjMsc3RyZWV0LHl5IiwiY3VzdG9tOmxhc3ROYW1lIjoiUG9ucyIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6IjEwZWJkOGZlLTBlNTgtNDBmZi04Yjg4LTM5ZmMyMTY1NGRkNSIsImN1c3RvbTpzdGF0ZSI6InRhbWlsbmFkdSIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiZmFpc2FsIiwiY3VzdG9tOnRlbmFudElkIjoiMTJhZmM2NDAtM2MxNS0xMWU5LWJlZGQtYmI0M2IzYzQ5NDcyIiwiY29nbml0bzpyb2xlcyI6WyJhcm46YXdzOmlhbTo6NTI2ODIxNTM3NDY2OnJvbGVcL0NvZ25pdG9fVGVuYW50SURQb29sVW5hdXRoX1JvbGUiXSwiYXVkIjoiN2Nha2gxYW9odXVwaTd1OTRqMzI5ZzRzZ3QiLCJldmVudF9pZCI6IjU2YzNjN2NmLTQ5ZmYtMTFlOS04NjVmLWY5MTQxNjViNGM0YyIsImN1c3RvbTpmaXJzdE5hbWUiOiJtZCIsInRva2VuX3VzZSI6ImlkIiwiY3VzdG9tOnBob25lTnVtYmVyIjoiNzM1ODIxNjczMyIsImF1dGhfdGltZSI6MTU1Mjk2OTY2MywiY3VzdG9tOmNvbXBhbnlOYW1lIjoiaWJyYWhpbV9rX3BvZ2V5YW5fY29tIiwiZXhwIjoxNTUyOTczMjYzLCJpYXQiOjE1NTI5Njk2NjMsImVtYWlsIjoiaWJyYWhpbS5rQHBvZ2V5YW4uY29tIn0.nY3_grss7G2JIeJ0Ro3mcBhbikY9SEGuhWeMSaLoEbYidPW93X4AYuE28aSAm8usdoo0HBUJUTtK-JfsWuaKjLT3m3gtvUskbU3sPqOS-TmH-iNKRGD1y6RYXFExKOB6ABCcqDP8Z2PpePzyQNwBt-qmljn4IRFTNjrM_Ui4cEmr7F6KP-sCqAqVueGFz_nbTTDWyzPYiouVaCJOdExnBOUU0jzt19u3RjGWgAAOXTvBazoU8CTAjRNijYjQrLNSJ7JRI07tTXkcEhXRevJYpW8PElXISgCBYZXejsKXfMhSxKo2IS3WKixztvpKvyg28GRnLEjh52TJC5FZxtOklA";
				Session session_user2 = createsession(user2, password2);
				CmisObject object_user2 = session_user2.getObject(templateId);
				if (object_user2 != null) {
					addResult(createResult(INFO, "the template object is assigned to the project succesfully"));
				}

			} catch (Exception e) {
				addResult(createResult(FAILURE, "template object is not assigned to the project"));
			}
			//
			try {
				String username = "fdf37256-23f6-409b-9b26-0ec74aece2ea";
				String password = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJmZGYzNzI1Ni0yM2Y2LTQwOWItOWIyNi0wZWM3NGFlY2UyZWEiLCJjb2duaXRvOmdyb3VwcyI6WyJUZXN0VGVuYW50X0NvbXBhbnkiLCJUZXN0VGVuYW50X0NvbXBhbnlfQURNSU4iXSwiZW1haWxfdmVyaWZpZWQiOnRydWUsImN1c3RvbTphZGRyZXNzIjoiTGF2ZWxsZSBtYW5zaW9uIiwiY3VzdG9tOmxhc3ROYW1lIjoiTSIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6ImZkZjM3MjU2LTIzZjYtNDA5Yi05YjI2LTBlYzc0YWVjZTJlYSIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiRmFoYWQiLCJjdXN0b206cHJlZmVycmVkTGFuZyI6ImVuIiwiY3VzdG9tOnRlbmFudElkIjoiMTRjOTFkNzAtMzliMS0xMWU5LWE4NTktOWY3ZWNiYzMyOTRjIiwiY29nbml0bzpyb2xlcyI6WyJhcm46YXdzOmlhbTo6NTI2ODIxNTM3NDY2OnJvbGVcL0NvZ25pdG9fVGVuYW50SURQb29sVW5hdXRoX1JvbGUiXSwiYXVkIjoiN2Nha2gxYW9odXVwaTd1OTRqMzI5ZzRzZ3QiLCJldmVudF9pZCI6IjAxZjQ5OTllLTUwNGEtMTFlOS04NTZlLTYxN2ZmYzMxNzM4OSIsImN1c3RvbTpmaXJzdE5hbWUiOiJTaGl2YXJhaiIsInRva2VuX3VzZSI6ImlkIiwiYXV0aF90aW1lIjoxNTUzNjYxNDQwLCJjdXN0b206Y29tcGFueU5hbWUiOiJUZXN0VGVuYW50IiwiZXhwIjoxNTUzNjY1MDQwLCJpYXQiOjE1NTM2NjE0NDAsImVtYWlsIjoic2hpdmFyYWoubUBwb2dleWFuLmNvbSJ9.tneBLCpM4S3dKCKLAqbXiuzL2ssg92ISPp2xHPlThAECB1s6abTPs6X_APpCJntpK954vYqEFZ5S58jY1PnniIlZcEbWY6S73uushS58yhC2JYyQGvSGbsPyh8hvXUdibuDP5XS6QWJ8hM_HXdVvYCCFrpjE6nntReDnDDrGp8kVLoHwbI51Qfb0MDHCSPWGlAbAPUVXBRCmoloSAiup7-hptULLnVuXMTeVd5yw4tf1lVsjMfVnW3_EjnD3vBmnHff8Kt_oFR3T6uCI55x0LBNiJpYhvjNO0bjNXfUk6qrrfrocceH0DKC6lANmcRuHmhvL_PoMghCNxqLqlA7EwQ";
				Session session_User = createsession(username, password);
				CmisObject object_user5 = session_User.getObject(templateId);
				if (object_user5 == null) {
					addResult(createResult(INFO, "child cannot access the object created by parent"));
				}

			} catch (Exception e) {
				addResult(createResult(FAILURE, "template object is not assigned to particular project successfully"));
			}

			try {
				String objectId12 = "8989890990007879";
				assignToHierarchy(session.getRepositoryInfo().getId(), item1, templateId, objectId12);
			} catch (Exception e) {
				addResult(createResult(INFO, "hierarchy_path or hierarchyName is null"));
			}

			try {
				String user3 = "d9094d03-2272-4d5a-a9dc-2b92c92fd6af";
				String password3 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJjdXN0b206emlwQ29kZSI6IjAwMDEyMyIsImN1c3RvbTpjb3VudHJ5IjoiSW5kaWEiLCJzdWIiOiJkOTA5NGQwMy0yMjcyLTRkNWEtYTlkYy0yYjkyYzkyZmQ2YWYiLCJjb2duaXRvOmdyb3VwcyI6WyJraGFkaGkyOV9nbWFpbF9jb21fQ29tcGFueSIsImtoYWRoaTI5X2dtYWlsX2NvbV9Db21wYW55X0FETUlOIiwiVGVuYW50R3JvdXAiXSwiZW1haWxfdmVyaWZpZWQiOnRydWUsImN1c3RvbTphZGRyZXNzIjoiTGF2ZWxsZSBNYW5zaW9uIiwiY3VzdG9tOmxhc3ROYW1lIjoiSmFpbiIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6ImQ5MDk0ZDAzLTIyNzItNGQ1YS1hOWRjLTJiOTJjOTJmZDZhZiIsImN1c3RvbTpzdGF0ZSI6Imthcm5hdGthIiwiY3VzdG9tOmNvbnRhY3RQZXJzb24iOiJGYWhhZCIsImN1c3RvbTp0ZW5hbnRJZCI6ImQ3YTBlODIwLTQ2NTItMTFlOS1hMDZkLWE1NTY1MjljNTRhMSIsImNvZ25pdG86cm9sZXMiOlsiYXJuOmF3czppYW06OjUyNjgyMTUzNzQ2Njpyb2xlXC9Db2duaXRvX1RlbmFudElEUG9vbFVuYXV0aF9Sb2xlIl0sImF1ZCI6IjdjYWtoMWFvaHV1cGk3dTk0ajMyOWc0c2d0IiwiZXZlbnRfaWQiOiI2NmFiNDgzZS00OWZmLTExZTktYjRmNS1kZDI5ZWQxZWUxMDMiLCJjdXN0b206Zmlyc3ROYW1lIjoiUmlzaGFiaCIsInRva2VuX3VzZSI6ImlkIiwiY3VzdG9tOnBob25lTnVtYmVyIjoiKzkxODE0NzE5MDA2MCIsImF1dGhfdGltZSI6MTU1Mjk2OTY5MCwiY3VzdG9tOmNvbXBhbnlOYW1lIjoia2hhZGhpMjlfZ21haWxfY29tIiwiZXhwIjoxNTUyOTczMjkwLCJpYXQiOjE1NTI5Njk2OTAsImVtYWlsIjoia2hhZGhpMjlAZ21haWwuY29tIn0.MukWPBG1CBVhnu9B8YdDi6qGd9eWnI-qqaqpw5XdiLfvqTKXneNeK_AH7sVcy9CR-jq0oz6dDQzbCPJDnDrE_ik3EO_kqIWc6s9rYYXtJEGOIlUKZc_HGN0Fnh0yRL2KigdHgJwa2ZRihEd-y5rlMlUf9p16wOtbKh9M-AEMTyjDBYASC9LgsDpnxI9gG3CYlIk_SSwM7bsU1HBZ07Vu4bNtDZrgX4YsrJDYqWYYs5QeowJO58IHHw67377z9fOMj8mHBfL4X9Oyh6HZTFxy6-hE4WzMeL9rEnsmEIaAihPcOqIB_DDVQEXnc6F6w1za-uCclaCAETiGnuvqsMJeVw";
				Session session_user3 = createsession(user3, password3);
				CmisObject object_user3 = session_user3.getObject(templateId);
				if (object_user3 != null) {
					addResult(createResult(FAILURE, "templateObject is assigned successfully"));
				}
			} catch (Exception e) {
				addResult(createResult(INFO, "does not have valid acces control permission to access this object"));
			}
			//
			// // addUserToHierarchyObject
			try {
				addUserToHierarchyObject(session.getRepositoryInfo().getId(), item1, objectId);

				String user3 = "8ad52939-97bf-49d5-b9ff-898424d95802";
				String password3 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiI4YWQ1MjkzOS05N2JmLTQ5ZDUtYjlmZi04OTg0MjRkOTU4MDIiLCJjb2duaXRvOmdyb3VwcyI6WyJUZXN0VGVuYW50X0NvbXBhbnkiLCJUZXN0VGVuYW50X0NvbXBhbnlfQURNSU4iXSwiZW1haWxfdmVyaWZpZWQiOnRydWUsImN1c3RvbTphZGRyZXNzIjoiTGF2ZWxsZSBtYW5zaW9uIiwiY3VzdG9tOmxhc3ROYW1lIjoiUyIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6IjhhZDUyOTM5LTk3YmYtNDlkNS1iOWZmLTg5ODQyNGQ5NTgwMiIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiRmFoYWQiLCJjdXN0b206cHJlZmVycmVkTGFuZyI6ImZyIiwiY3VzdG9tOnRlbmFudElkIjoiMTRjOTFkNzAtMzliMS0xMWU5LWE4NTktOWY3ZWNiYzMyOTRjIiwiY29nbml0bzpyb2xlcyI6WyJhcm46YXdzOmlhbTo6NTI2ODIxNTM3NDY2OnJvbGVcL0NvZ25pdG9fVGVuYW50SURQb29sVW5hdXRoX1JvbGUiXSwiYXVkIjoiN2Nha2gxYW9odXVwaTd1OTRqMzI5ZzRzZ3QiLCJldmVudF9pZCI6IjM5YzAzMzhjLTVhYWQtMTFlOS1hYmEwLTFiOGViOWUxZmUyYiIsImN1c3RvbTpmaXJzdE5hbWUiOiJBcmNoYW5hIiwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE1NTQ4MDM1NjUsImN1c3RvbTpjb21wYW55TmFtZSI6IlRlc3RUZW5hbnQiLCJleHAiOjE1NTQ4MDcxNjUsImlhdCI6MTU1NDgwMzU2NSwiZW1haWwiOiJhcmNoYW5hLnNAcG9nZXlhbi5jb20ifQ.Wupv8a4KOUJCZpOvjZfRRsU5FvE3tG7rMSHC2Uu8XN95wFNizG5uspn-zhMTahasgJI2V5bjF4suIW1XhJ22ZixJPWA4jEuLnEUSL4YQ-3-D4IAQRKLM3wZ3yxFexLuONdreoB1rpFQk2QQiSVXTDBHYA9o8rlIhVM1MDmmeTrqohT_qevh1KgSrCkcqLRM0flzPt6-MWnFu6J_3gFSAJPe0HWAGlYeIng51LvgBurv6H-zelKRF9OCL2FSCteNOc9btUZSgHqRYdR8dUGVKrxNWbjjYpMoV_bj-Jh7BzqJhRP2a7sM8QanDDYJHG7tVHmcno8OwzE2RZ6GhaO5yqw";
				Session session_user3 = createsession(user3, password3);
				CmisObject object_user3 = session_user3.getObject(objectId);
				if (object_user3 != null) {
					addResult(createResult(INFO, "the user is added successfully"));
				}
			} catch (Exception e) {
				addResult(createResult(FAILURE, "the user is not added"));
			}
			//
			// // removeUserFromHierarchyObject
			try {
				removeUserFromHierarchyObject(session.getRepositoryInfo().getId(), item1, objectId);
				resetcache(session.getRepositoryInfo().getId());
				String user4 = "8ad52939-97bf-49d5-b9ff-898424d95802";
				String password4 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiI4YWQ1MjkzOS05N2JmLTQ5ZDUtYjlmZi04OTg0MjRkOTU4MDIiLCJjb2duaXRvOmdyb3VwcyI6WyJUZXN0VGVuYW50X0NvbXBhbnkiLCJUZXN0VGVuYW50X0NvbXBhbnlfQURNSU4iXSwiZW1haWxfdmVyaWZpZWQiOnRydWUsImN1c3RvbTphZGRyZXNzIjoiTGF2ZWxsZSBtYW5zaW9uIiwiY3VzdG9tOmxhc3ROYW1lIjoiUyIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6IjhhZDUyOTM5LTk3YmYtNDlkNS1iOWZmLTg5ODQyNGQ5NTgwMiIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiRmFoYWQiLCJjdXN0b206cHJlZmVycmVkTGFuZyI6ImZyIiwiY3VzdG9tOnRlbmFudElkIjoiMTRjOTFkNzAtMzliMS0xMWU5LWE4NTktOWY3ZWNiYzMyOTRjIiwiY29nbml0bzpyb2xlcyI6WyJhcm46YXdzOmlhbTo6NTI2ODIxNTM3NDY2OnJvbGVcL0NvZ25pdG9fVGVuYW50SURQb29sVW5hdXRoX1JvbGUiXSwiYXVkIjoiN2Nha2gxYW9odXVwaTd1OTRqMzI5ZzRzZ3QiLCJldmVudF9pZCI6IjM5YzAzMzhjLTVhYWQtMTFlOS1hYmEwLTFiOGViOWUxZmUyYiIsImN1c3RvbTpmaXJzdE5hbWUiOiJBcmNoYW5hIiwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE1NTQ4MDM1NjUsImN1c3RvbTpjb21wYW55TmFtZSI6IlRlc3RUZW5hbnQiLCJleHAiOjE1NTQ4MDcxNjUsImlhdCI6MTU1NDgwMzU2NSwiZW1haWwiOiJhcmNoYW5hLnNAcG9nZXlhbi5jb20ifQ.Wupv8a4KOUJCZpOvjZfRRsU5FvE3tG7rMSHC2Uu8XN95wFNizG5uspn-zhMTahasgJI2V5bjF4suIW1XhJ22ZixJPWA4jEuLnEUSL4YQ-3-D4IAQRKLM3wZ3yxFexLuONdreoB1rpFQk2QQiSVXTDBHYA9o8rlIhVM1MDmmeTrqohT_qevh1KgSrCkcqLRM0flzPt6-MWnFu6J_3gFSAJPe0HWAGlYeIng51LvgBurv6H-zelKRF9OCL2FSCteNOc9btUZSgHqRYdR8dUGVKrxNWbjjYpMoV_bj-Jh7BzqJhRP2a7sM8QanDDYJHG7tVHmcno8OwzE2RZ6GhaO5yqw";
				Session session_user4 = createsession(user4, password4);
				CmisObject object_user4 = session_user4.getObject(objectId);
				if (object_user4 != null) {
					addResult(createResult(FAILURE, "the user is not removed successfully"));
				}
			} catch (Exception e) {
				addResult(createResult(INFO, "the user is removed successfully"));
			}
			//
			// // updateHierarchyPath
			try {
				updateHierarchyPath(session.getRepositoryInfo().getId(), item2, objectId, objectId2);
				String user5 = "10ebd8fe-0e58-40ff-8b88-39fc21654dd5";
				String password5 = "eyJraWQiOiI5cklBaFdLWXJUS1l0dGQ1R3FtVGl4V2J4WUJ0R0U1XC85SlUzSGFkZ0FqST0iLCJhbGciOiJSUzI1NiJ9.eyJjdXN0b206emlwQ29kZSI6IjYzMiIsImN1c3RvbTpjb3VudHJ5IjoiaW5kaWEiLCJzdWIiOiIxMGViZDhmZS0wZTU4LTQwZmYtOGI4OC0zOWZjMjE2NTRkZDUiLCJjb2duaXRvOmdyb3VwcyI6WyJpYnJhaGltX2tfcG9nZXlhbl9jb21fQ29tcGFueSIsIlRlbmFudEdyb3VwIl0sImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJjdXN0b206YWRkcmVzcyI6IjMsc3RyZWV0LHl5IiwiY3VzdG9tOmxhc3ROYW1lIjoiUG9ucyIsImlzcyI6Imh0dHBzOlwvXC9jb2duaXRvLWlkcC5hcC1zb3V0aC0xLmFtYXpvbmF3cy5jb21cL2FwLXNvdXRoLTFfd1g5bUwzOVFVIiwiY29nbml0bzp1c2VybmFtZSI6IjEwZWJkOGZlLTBlNTgtNDBmZi04Yjg4LTM5ZmMyMTY1NGRkNSIsImN1c3RvbTpzdGF0ZSI6InRhbWlsbmFkdSIsImN1c3RvbTpjb250YWN0UGVyc29uIjoiZmFpc2FsIiwiY3VzdG9tOnRlbmFudElkIjoiMTJhZmM2NDAtM2MxNS0xMWU5LWJlZGQtYmI0M2IzYzQ5NDcyIiwiY29nbml0bzpyb2xlcyI6WyJhcm46YXdzOmlhbTo6NTI2ODIxNTM3NDY2OnJvbGVcL0NvZ25pdG9fVGVuYW50SURQb29sVW5hdXRoX1JvbGUiXSwiYXVkIjoiN2Nha2gxYW9odXVwaTd1OTRqMzI5ZzRzZ3QiLCJldmVudF9pZCI6IjU2YzNjN2NmLTQ5ZmYtMTFlOS04NjVmLWY5MTQxNjViNGM0YyIsImN1c3RvbTpmaXJzdE5hbWUiOiJtZCIsInRva2VuX3VzZSI6ImlkIiwiY3VzdG9tOnBob25lTnVtYmVyIjoiNzM1ODIxNjczMyIsImF1dGhfdGltZSI6MTU1Mjk2OTY2MywiY3VzdG9tOmNvbXBhbnlOYW1lIjoiaWJyYWhpbV9rX3BvZ2V5YW5fY29tIiwiZXhwIjoxNTUyOTczMjYzLCJpYXQiOjE1NTI5Njk2NjMsImVtYWlsIjoiaWJyYWhpbS5rQHBvZ2V5YW4uY29tIn0.nY3_grss7G2JIeJ0Ro3mcBhbikY9SEGuhWeMSaLoEbYidPW93X4AYuE28aSAm8usdoo0HBUJUTtK-JfsWuaKjLT3m3gtvUskbU3sPqOS-TmH-iNKRGD1y6RYXFExKOB6ABCcqDP8Z2PpePzyQNwBt-qmljn4IRFTNjrM_Ui4cEmr7F6KP-sCqAqVueGFz_nbTTDWyzPYiouVaCJOdExnBOUU0jzt19u3RjGWgAAOXTvBazoU8CTAjRNijYjQrLNSJ7JRI07tTXkcEhXRevJYpW8PElXISgCBYZXejsKXfMhSxKo2IS3WKixztvpKvyg28GRnLEjh52TJC5FZxtOklA";
				Session session_user5 = createsession(user5, password5);
				CmisObject object_user5 = session_user5.getObject(objectId);
				if (object_user5 != null) {
					addResult(createResult(FAILURE, "hierarchy_path is not updated"));
				}
			} catch (Exception e) {
				addResult(createResult(INFO, "the hierarchy_path is updated successfully"));
			}

			//// deleteHierarchyObject
			try {
				session.delete(session.getObjectByPath("/cmis_ext:relationship/relation_" + newItem.getId()));
				session.delete(newItem);
				deleteHierarchyObject(session.getRepositoryInfo().getId(), item1, objectIdc);
				session.deleteType(fv_item.getId());
				session.deleteType(fv_party.getId());
				session.deleteType(fv_topdown.getId());
				session.deleteType(template1.getId());
				session.deleteType(project1.getId());
				session.deleteType(revision.getId());
				
				CmisObject object_user6 = session.getObject(objectIdc);
				if (object_user6 != null) {
					addResult(createResult(FAILURE, "deleteHierarchyObject method failed to delete the objects"));
				}
			} catch (Exception e) {
				addResult(createResult(INFO, "objects are deleted successfully"));
			}
		} catch (FileNotFoundException e) {
			addResult(createResult(FAILURE, "FileNotFoundException while parsing the json"));
		} catch (IOException e) {
			addResult(createResult(FAILURE, "IOException while parsing the json"));
		} catch (JSONParseException e) {
			addResult(createResult(FAILURE, "JSONParseException while parsing the json"));
		}
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

	public static JSONObject resetcache(String repoId) throws Exception {

		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString + "?cmisselector=resetcache";
		HttpGet getRequest = new HttpGet(reqUrl.trim());
		getRequest.addHeader("Content-Type", "application/json");
		getRequest.addHeader("Authorization", getB64Auth(AbstractRunner.USER_NAME, AbstractRunner.PASSWORD));
		httpResponse = httpclient.execute(getRequest);
		HttpEntity resEntity = httpResponse.getEntity();
		if (resEntity != null) {
			String resBody = EntityUtils.toString(resEntity);
			JSONObject json = new ObjectMapper().readValue(resBody, JSONObject.class);
			return json;
		}
		return null;
	}

	public static JSONObject createHierarchyObject(String repoId, String objectTypeId, String name, String parentId,
			String users) throws Exception {
		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString
				+ "?cmisaction=createHierarchyObject&propertyId[0]=cmis:objectTypeId&propertyValue[0]=" + objectTypeId
				+ "&propertyId[1]=cmis:name&propertyValue[1]=" + name
				+ "&propertyId[2]=fv:hierarchy_parent_id&propertyValue[2]=" + parentId
				+ "&propertyId[3]=fv:hierarchy_users&propertyValue[3]=" + users;
		HttpPost postRequest = new HttpPost(reqUrl.trim());
		postRequest.addHeader("Content-Type", "application/json");
		postRequest.addHeader("Authorization", getB64Auth(AbstractRunner.USER_NAME, AbstractRunner.PASSWORD));
		JSONObject jbody = new JSONObject();
		StringEntity body = new StringEntity(jbody.toString());
		postRequest.setEntity(body);
		httpResponse = httpclient.execute(postRequest);
		HttpEntity resEntity = httpResponse.getEntity();
		if (resEntity != null) {
			String resBody = EntityUtils.toString(resEntity);
			JSONObject json = new ObjectMapper().readValue(resBody, JSONObject.class);
			return json;
		}
		return null;
	}

	public static JSONObject assignToHierarchy(String repoId, ArrayList<Object> docIds, String templateId, String id)
			throws Exception {

		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString + "?cmisselector=assignToHierarchy&objectId=" + templateId + "&cmis:objectId="
				+ id + "&cmis:name=checky:project_checky:template";
		HttpGet getRequest = new HttpGet(reqUrl.trim());
		getRequest.addHeader("Content-Type", "application/json");
		getRequest.addHeader("Authorization", getB64Auth(AbstractRunner.USER_NAME, AbstractRunner.PASSWORD));
		JSONObject jbody = new JSONObject();
		jbody.put("ids", docIds);
		httpResponse = httpclient.execute(getRequest);
		HttpEntity resEntity = httpResponse.getEntity();
		if (resEntity != null) {
			String resBody = EntityUtils.toString(resEntity);
			JSONObject json = new ObjectMapper().readValue(resBody, JSONObject.class);
			return json;
		}
		return null;
	}

	public static JSONObject addUserToHierarchyObject(String repoId, ArrayList<Object> docIds, String objectId)
			throws Exception {

		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString + "?cmisaction=addUserToHierarchyObject&objectId=" + objectId
				+ "&propertyId[0]=fv:hierarchy_users&propertyValue[0]=8ad52939-97bf-49d5-b9ff-898424d95802";
		HttpPost postRequest = new HttpPost(reqUrl.trim());
		postRequest.addHeader("Content-Type", "application/json");
		postRequest.addHeader("Authorization", getB64Auth(AbstractRunner.USER_NAME, AbstractRunner.PASSWORD));
		JSONObject jbody = new JSONObject();
		jbody.put("ids", docIds);
		StringEntity body = new StringEntity(jbody.toString());
		postRequest.setEntity(body);
		httpResponse = httpclient.execute(postRequest);
		HttpEntity resEntity = httpResponse.getEntity();
		if (resEntity != null) {
			String resBody = EntityUtils.toString(resEntity);
			JSONObject json = new ObjectMapper().readValue(resBody, JSONObject.class);
			return json;
		}
		return null;
	}

	public static JSONObject removeUserFromHierarchyObject(String repoId, ArrayList<Object> docIds, String objectId)
			throws Exception {

		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString + "?cmisaction=removeUserFromHierarchyObject&objectId=" + objectId
				+ "&propertyId[0]=fv:hierarchy_users&propertyValue[0]=8ad52939-97bf-49d5-b9ff-898424d95802";
		HttpPost postRequest = new HttpPost(reqUrl.trim());
		postRequest.addHeader("Content-Type", "application/json");
		postRequest.addHeader("Authorization", getB64Auth(AbstractRunner.USER_NAME, AbstractRunner.PASSWORD));
		JSONObject jbody = new JSONObject();
		jbody.put("ids", docIds);
		StringEntity body = new StringEntity(jbody.toString());
		postRequest.setEntity(body);
		httpResponse = httpclient.execute(postRequest);
		HttpEntity resEntity = httpResponse.getEntity();
		if (resEntity != null) {
			String resBody = EntityUtils.toString(resEntity);
			JSONObject json = new ObjectMapper().readValue(resBody, JSONObject.class);
			return json;
		}
		return null;
	}

	public static JSONObject updateHierarchyPath(String repoId, ArrayList<Object> docIds, String sourceId,
			String targetId) throws Exception {

		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString + "?cmisaction=updateHierarchyPath"
				+ "&propertyId[0]=sourceId&propertyValue[0]=" + sourceId + "&propertyId[1]=targetId&propertyValue[1]="
				+ targetId;
		HttpPost postRequest = new HttpPost(reqUrl.trim());
		postRequest.addHeader("Content-Type", "application/json");
		postRequest.addHeader("Authorization", getB64Auth(AbstractRunner.USER_NAME, AbstractRunner.PASSWORD));
		JSONObject jbody = new JSONObject();
		jbody.put("ids", docIds);
		httpResponse = httpclient.execute(postRequest);
		HttpEntity resEntity = httpResponse.getEntity();
		if (resEntity != null) {
			String resBody = EntityUtils.toString(resEntity);
			JSONObject json = new ObjectMapper().readValue(resBody, JSONObject.class);
			return json;
		}
		return null;
	}

	public static JSONObject deleteHierarchyObject(String repoId, ArrayList<Object> docIds, String objectId)
			throws Exception {
		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString + "?cmisselector=deleteHierarchyObject&objectId=" + objectId;
		HttpGet getRequest = new HttpGet(reqUrl.trim());
		getRequest.addHeader("Content-Type", "application/json");
		getRequest.addHeader("Authorization", getB64Auth(AbstractRunner.USER_NAME, AbstractRunner.PASSWORD));
		JSONObject jbody = new JSONObject();
		jbody.put("ids", docIds);
		httpResponse = httpclient.execute(getRequest);
		HttpEntity resEntity = httpResponse.getEntity();
		if (resEntity != null) {
			String resBody = EntityUtils.toString(resEntity);
			JSONObject json = new ObjectMapper().readValue(resBody, JSONObject.class);
			return json;
		}
		return null;
	}

	private static String getB64Auth(String username, String password) {
		String source = username + ":" + password;
		String encoding = "Basic " + Base64.getEncoder().encodeToString(source.getBytes());
		return encoding;
	}
}
