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
package com.pogeyan.cmis.api.storage;

import java.io.IOException;
import java.math.BigInteger;

import org.apache.chemistry.opencmis.commons.data.ContentStream;

public interface IStorageService {

	public void setStoreSettings(IRepositoryStorageSettings storeSettings);

	/**
	 * Write a content.
	 * 
	 * @param dataId
	 *            the content identifier.
	 * @param contentStream
	 *            the stream of the content
	 * @return
	 */
	public void writeContent(String objectId, String objectName, String path, ContentStream contentStream)
			throws IOException, IllegalArgumentException;

	/**
	 * Append a content.
	 * 
	 * @param objectName
	 *            the content identifier.
	 * @param contentStream
	 *            the stream of the content
	 * @return
	 */
	public ContentStream appendContent(String objectId, String objectName, String path, ContentStream contentStream,
			Boolean isLastChunk);

	/**
	 * Delete a content. Since folder are not materialized, only document are
	 * deleted from the storage system.
	 * 
	 * @param objectName
	 *            the content identifier.
	 * @return
	 */
	public boolean deleteContent(String objectName, String path, String mimeType);

	public org.apache.chemistry.opencmis.commons.data.ContentStream getContent(String objectName, String path,
			String mimeType, BigInteger length);

	/**
	 * Tests if a content is already stored.
	 * 
	 * @param objectName
	 *            the content identifier
	 * @return true if the content exists
	 */
	public boolean exists(String objectName, String path);

	/**
	 * Set a content.
	 * 
	 * @param objectName
	 *            the content identifier.
	 * @param inputStream
	 *            the stream of the input
	 * @return
	 */
	public void setContent(String objectId, String objectName, String oldFileName, String path,
			ContentStream inputStream);

	public void createFolder(String objectId, String objectName, String path)
			throws IOException, IllegalArgumentException;

	public void moveFolder(String objectId, String sourcePath, String targetPath)
			throws IOException, IllegalArgumentException;

	public void moveDocument(String objectId, String objectName, String sourcePath, String targetPath, String mimeType,
			BigInteger contentStreamLength) throws IOException, IllegalArgumentException;

	public void rename(String oldPath, String newPath) throws IOException, IllegalArgumentException;

	public void deleteFolder(String folderName);
}
