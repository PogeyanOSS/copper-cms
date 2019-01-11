/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.chemistry.opencmis.tck.tests.crud;

import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.FAILURE;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.SKIPPED;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.UNEXPECTED_EXCEPTION;
import static org.apache.chemistry.opencmis.tck.CmisTestResultStatus.WARNING;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.CmisObject;
import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.ItemIterable;
import org.apache.chemistry.opencmis.client.api.ObjectType;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;
import org.apache.chemistry.opencmis.commons.enums.VersioningState;
import org.apache.chemistry.opencmis.commons.exceptions.CmisBaseException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.chemistry.opencmis.tck.CmisTestResult;
import org.apache.chemistry.opencmis.tck.impl.AbstractSessionTest;

/**
 * Simple document test.
 */
public class CreateDocWithFileName extends AbstractSessionTest {

	private static final String CONTENT = "TCK test content.";

	@Override
	public void init(Map<String, String> parameters) {
		super.init(parameters);
		setName("Create and Delete Document Test with FileName");
		setDescription(
				"Creates a document, checks the newly created documents and their parent and finally deletes the created document.");
	}

	@Override
	public void run(Session session) {
		CmisTestResult f;

		// create a test folder
		Folder testFolder = createTestFolder(session);
		try {

			Document newDocument = createDocument(session, testFolder, "doc", getDocumentTestTypeId(), null, CONTENT,
					"hello");
			// simple children test
			addResult(checkChildren(session, testFolder, "Test folder children check"));

			// check if all documents are there
			ItemIterable<CmisObject> children = testFolder.getChildren(SELECT_ALL_NO_CACHE_OC);
			for (CmisObject child : children) {
				if (child != null) {
					Document document = newDocument;

					f = createResult(FAILURE, "Document and test folder child don't match! Id: " + child.getId());
					addResult(assertShallowEquals(document, child, null, f));

					// Document with content should have cmis:name set as path
					String path = document.getPropertyValue(PropertyIds.PATH);
					addResult(
							assertEquals(path, testFolder.getPropertyValue(PropertyIds.PATH) + "/" + document.getName(),
									null, createResult(FAILURE, "Document's path should be of cmis:name")));
				}
			}

			f = createResult(FAILURE, "Number of created documents does not match the number of existing documents!");
			addResult(assertEquals(1, ((Long) children.getTotalNumItems()).intValue(), null, f));

			ContentStream contentStream = newDocument.getContentStream();
			if (contentStream == null || contentStream.getStream() == null) {
				addResult(createResult(FAILURE, "Document has no content! Id: " + newDocument.getId()));
			} else {
				IOUtils.closeQuietly(contentStream);
			}

			// delete all documents
			newDocument.delete(true);
			f = createResult(FAILURE,
					"Document should not exist anymore but it is still there! Id: " + newDocument.getId());
			addResult(assertIsFalse(exists(newDocument), null, f));

		} finally {
			// delete the test folder
			deleteTestFolder();
		}

		addResult(createInfoResult("Tested the creation and deletion of one document with setting fileName."));
	}

	/**
	 * Creates a document.
	 */
	protected Document createDocument(Session session, Folder parent, String name, String objectTypeId,
			String[] secondaryTypeIds, String content, String filename) {
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

		byte[] contentBytes = null;
		Document result = null;
		try {
			contentBytes = IOUtils.toUTF8Bytes(content);
			ContentStream contentStream = new ContentStreamImpl(filename, BigInteger.valueOf(contentBytes.length),
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
				addResult(assertEquals(filename, contentStream.getFileName(), null, f));

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
					"The folder the document has been created in is not in the list of the document parents!"));
		}

		return result;
	}
}
