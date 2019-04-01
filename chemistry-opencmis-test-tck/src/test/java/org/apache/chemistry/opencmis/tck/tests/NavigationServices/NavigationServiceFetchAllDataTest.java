package org.apache.chemistry.opencmis.tck.tests.NavigationServices;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.UNEXPECTED_EXCEPTION;

import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.apache.chemistry.opencmis.tck.CmisTestResult;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;
import org.apache.chemistry.opencmis.tck.runner.AbstractRunner;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

public class NavigationServiceFetchAllDataTest extends AbstractSessionTest {
	private static final String CONTENT = "TCK test content.";

	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("Navigation Service fetch AllData Test");
		setDescription("Creates a Document type with NavigationService fetch AllData");
	}

	@SuppressWarnings("unchecked")
	@Override
	public void run(Session session) {
		CmisTestResult f = null;

		int numOfDocuments = 50;

		// create a test folder
		Folder testFolder = createTestFolder(session);

		try {
			Map<String, Document> documents = new HashMap<String, Document>();
			ArrayList<Object> docIds = new ArrayList<Object>();
			// create documents
			for (int i = 0; i < numOfDocuments; i++) {
				Document newDocument = createDocument(session, testFolder, "doc" + i, CONTENT);
				documents.put(newDocument.getId(), newDocument);
				docIds.add(newDocument.getId());
			}
			try {
				String repoId = session.getRepositoryInfo().getId();
				JSONObject data = fetchAllObjectsData(repoId, docIds);
				Map<String, ArrayList<Object>> relMap = formRawData(session, (ArrayList<Object>) data.get("objects"));
				boolean isAllPresent = false;
				for (Object id : docIds) {
					if (relMap.containsKey(id)) {
						isAllPresent = true;
					} else {
						isAllPresent = false;
					}
				}
				f = createResult(FAILURE, "All ids not present!");
				addResult(assertEquals(true, isAllPresent, null, f));
				f = createResult(FAILURE,
						"Number of created documents does not match the number of existing documents!");
				addResult(assertEquals(data.get("numItems"), numOfDocuments, null, f));
			} catch (Exception e) {
				addResult(createResult(UNEXPECTED_EXCEPTION,
						"Document content could not been fetched! Exception: " + e.getMessage(), e, true));
			}

		} finally {
			// delete the test folder
			deleteTestFolder();

		}

		addResult(createInfoResult("Tested the parallel creation and deletion of " + numOfDocuments + " documents."));
	}

	@SuppressWarnings("unchecked")
	public static Map<String, ArrayList<Object>> formRawData(Session session, ArrayList<Object> relationData) {

		Map<String, ArrayList<Object>> relMap = new LinkedHashMap<String, ArrayList<Object>>();
		if (relationData != null) {
			for (Object relObj : relationData) {
				LinkedHashMap<Object, Object> relObjMap = (LinkedHashMap<Object, Object>) relObj;
				LinkedHashMap<Object, Object> object1 = (LinkedHashMap<Object, Object>) relObjMap.get("object");
				LinkedHashMap<Object, Object> succintProps = (LinkedHashMap<Object, Object>) object1
						.get("succinctProperties");
				String id = succintProps.get(PropertyIds.OBJECT_ID).toString();
				ArrayList<Object> value = new ArrayList<Object>();
				value.add(succintProps);
				relMap.put(id, value);
			}
		}
		return relMap;

	}

	public static JSONObject fetchAllObjectsData(String repoId, ArrayList<Object> docIds) throws Exception {

		CloseableHttpClient httpclient = HttpClients.createDefault();
		CloseableHttpResponse httpResponse = null;
		String connectionString = AbstractRunner.BROWSER_URL;
		String reqUrl = connectionString + "?cmisselector=getAllObjects&succinct=true&cmisaction=getAllObjects";
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

	private static String getB64Auth(String username, String password) {
		String source = username + ":" + password;
		String encoding = "Basic " + Base64.getEncoder().encodeToString(source.getBytes());

		return encoding;
	}
}