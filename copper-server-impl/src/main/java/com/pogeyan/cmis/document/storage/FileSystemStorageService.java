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
package com.pogeyan.cmis.document.storage;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.SequenceInputStream;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConstraintException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.IOUtils;
import org.apache.chemistry.opencmis.commons.impl.MimeTypes;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.io.ByteStreams;
import com.pogeyan.cmis.api.storage.IRepositoryStoreSettings;
import com.pogeyan.cmis.api.storage.IStorageService;
import com.pogeyan.cmis.api.utils.*;

public class FileSystemStorageService implements IStorageService {
	private static final Logger LOG = LoggerFactory.getLogger(FileSystemStorageService.class);

	private static final int BUFFER_SIZE = 64 * 1024;
	private FileSystemStoreSettings storeSettings;

	FileSystemStorageService() {
	}

	@Override
	public void writeContent(String objectId, String objectName, String path, ContentStream contentStream)
			throws IOException, IllegalArgumentException {
		LOG.info("writeContent dataId:{}" + objectId);
		if (this.storeSettings.getFileLocation() == null) {
			LOG.error("Root folder does not exist");
			throw new IOException("Root folder does not exist");
		}
		if (!getFileExtensionExists(objectName)) {
			objectName = objectName + MimeUtils.guessExtensionFromMimeType(contentStream.getMimeType());
		}

		File newFile = new File(gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(path)),
				objectName);

		if (!newFile.exists()) {
			try {
				newFile.createNewFile();
			} catch (Exception e) {
				LOG.error("writeContent exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
				throw new IllegalArgumentException("Could not create file: " + e.getMessage());
			}
		}

		// write content, if available
		if (contentStream != null && contentStream.getStream() != null) {
			writeContent(newFile, contentStream.getStream());
		}
	}

	@Override
	public void createFolder(String objectId, String objectName, String path)
			throws IOException, IllegalArgumentException {
		LOG.info("create Folder dataId:{}," + objectName);
		try {
			String folderPath = gettingFolderPath(this.storeSettings.getFileLocation(), path);
			File folderDirectory = new File(folderPath);
			if (!folderDirectory.exists()) {
				folderDirectory.mkdirs();
			}
		} catch (Exception e) {
			LOG.error("Create Folder exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new IllegalArgumentException("Could not create Folder: " + e.getMessage());
		}

	}

	@Override
	public void moveFolder(String objectId, String sourcePath, String targetPath)
			throws IOException, IllegalArgumentException {
		LOG.info("Move Folder for Sourcepath and Targetpath{},{}", sourcePath, targetPath);
		try {
			String rootpath = this.storeSettings.getFileLocation();
			if (rootpath == null) {
				LOG.error("Local storage path is undefined");
				throw new CmisConstraintException("Local storage path is undefined");
			}
			File sourceDir = new File(gettingFolderPath(this.storeSettings.getFileLocation(), sourcePath));
			targetPath = createSourceInTarget(gettingFolderPath(this.storeSettings.getFileLocation(), targetPath),
					sourcePath);
			File targetDir = new File(targetPath);
			FileUtils.copyDirectory(sourceDir, targetDir);
			File deleteSourceDir = new File(gettingFolderPath(this.storeSettings.getFileLocation(), sourcePath));
			FileUtils.deleteDirectory(deleteSourceDir);
		} catch (IOException e) {
			LOG.error("createLocalStorageService exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new IllegalArgumentException("Could not create Folder: " + e.getMessage());
		}

	}

	@Override
	public void moveDocument(String objectId, String objectName, String sourcePath, String targetPath, String mimeType,
			BigInteger contentStreamLength) throws IOException, IllegalArgumentException {
		LOG.info("Move Document for Sourcepath and Targetpath,{},{}", sourcePath, targetPath);

		if (!getFileExtensionExists(objectName)) {
			objectName = objectName + MimeUtils.guessExtensionFromMimeType(mimeType);
		}

		String targetId = gettingFolderPath(this.storeSettings.getFileLocation(), targetPath) + File.separator
				+ objectName;

		File sourceFile = new File(
				gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(sourcePath)) + File.separator
						+ objectName);

		sourceFile.renameTo(new File(targetId));

		// delete the original file
		sourceFile.delete();
	}

	@Override
	public void rename(String oldPath, String newPath) throws IOException, IllegalArgumentException {
		LOG.debug("Rename of oldname into newname:{},{}", oldPath, newPath);
		String rootpath = this.storeSettings.getFileLocation();
		if (rootpath == null) {
			LOG.error("Local storage path is undefined");
			throw new CmisConstraintException("Local storage path is undefined");
		}

		File oldFile = new File(gettingFolderPath(this.storeSettings.getFileLocation(), oldPath));

		File newFile = new File(gettingFolderPath(this.storeSettings.getFileLocation(), newPath));

		oldFile.renameTo(newFile);

	}

	/**
	 * Append a content.
	 * 
	 * @param dataId,
	 *            contentStream, isLastChunk the content identifier.
	 * @return ContentStream the old ContentStream for the objectId
	 */
	@Override
	public ContentStream appendContent(String objectId, String objectName, String path, ContentStream contentStream,
			Boolean isLastChunk) {
		ContentStream oldContent = null;
		LOG.info("appendContent dataId:{}" + objectName);
		try {
			LOG.debug("appendContent dataId:{}" + objectName);

			if (!getFileExtensionExists(objectName)) {
				objectName = objectName + MimeUtils.guessExtensionFromMimeType(contentStream.getMimeType());
			}

			File appendFileName = new File(
					gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(path)) + File.separator
							+ objectName);

			if (!appendFileName.exists()) {
				LOG.error("appendContent exception: {}", "File not found");
			}
			// write content, if available

			SequenceInputStream id = null;
			if (contentStream != null && contentStream.getStream() != null) {
				// if (isLastChunk.equals(true)) {
				// id = new SequenceInputStream(new
				// FileInputStream(appendFileName), contentStream.getStream());
				// } else {
				// id = new SequenceInputStream(contentStream.getStream(), new
				// FileInputStream(appendFileName));
				// }
				id = new SequenceInputStream(new FileInputStream(appendFileName), contentStream.getStream());
				Files.write(Paths.get(gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(path))
						+ File.separator + objectName), ByteStreams.toByteArray(id), StandardOpenOption.WRITE);
				id.close();
				oldContent = getContent(objectName, path, contentStream.getMimeType(),
						BigInteger.valueOf(appendFileName.length()));
			}
		} catch (Exception e) {
			LOG.error("appendContent exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new CmisObjectNotFoundException(e.getMessage(), e);
		}
		return oldContent;
	}

	/**
	 * Delete a content. Since folder are not materialized, only document are
	 * deleted from the storage system.
	 * 
	 * @param dataId
	 *            the content identifier.
	 * @return
	 */
	public boolean deleteContent(String objectName, String path, String mimeType) {
		LOG.info("Delete file name:{}" + objectName);
		try {
			if (!getFileExtensionExists(objectName)) {
				objectName = objectName + MimeUtils.guessExtensionFromMimeType(mimeType);
			}

			File file = new File(gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(path))
					+ File.separator + objectName);
			if (file == null || !file.isFile()) {
				return true;
			} else {
				return file.delete();
			}

		} catch (Exception e) {
			LOG.error("deleteContent exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new IllegalArgumentException("The object cannot be deleted");
		}
	}

	/**
	 * Writes the content to disc.
	 */
	private void writeContent(File newFile, InputStream stream) throws IllegalArgumentException {
		OutputStream out = null;
		try {
			out = new FileOutputStream(newFile);
			IOUtils.copy(stream, out, BUFFER_SIZE);
		} catch (Exception e) {
			LOG.error("writeContent exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new IllegalArgumentException("Could not write content: " + e.getMessage(), e);
		} finally {
			IOUtils.closeQuietly(out);
			IOUtils.closeQuietly(stream);
		}
	}

	@Override
	public org.apache.chemistry.opencmis.commons.data.ContentStream getContent(String objectName, String path,
			String mimeType, BigInteger length) {
		LOG.info("getConetent file name:{}" + objectName);
		try {
			String objectNameWithExtension = objectName;
			if (LOG.isDebugEnabled()) {
				LOG.debug("getConetent file name:{}" + objectName);
			}
			if (!getFileExtensionExists(objectName)) {
				objectNameWithExtension = objectName + MimeUtils.guessExtensionFromMimeType(mimeType);
			}

			ContentStream contentStream = null;

			File file = new File(gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(path))
					+ File.separator + objectNameWithExtension);

			if (!file.isFile()) {
				return contentStream;
			}

			if (file.length() == 0) {
				return new ContentStreamImpl(objectName, length, MimeTypes.getMIMEType(file),
						new FileInputStream(file));
			}

			contentStream = new ContentStreamImpl(objectName, length, MimeTypes.getMIMEType(file),
					new FileInputStream(file));
			return contentStream;
		} catch (Exception e) {
			LOG.error("getContent exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new CmisObjectNotFoundException(e.getMessage(), e);
		}
	}

	@SuppressWarnings("unused")
	public boolean exists(String objectName, String path) {
		try {
			File file = new File(gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(path))
					+ File.separator + objectName);
			if (file == null) {
				return false;
			}

			return file.exists();
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

	@Override
	public void setContent(String objectId, String objectName, String oldFileName, String path,
			ContentStream inputStream) {
		LOG.info("setContent dataId:{}" + objectName, objectId);
		try {
			// LOG.debug("setContent dataId:{}" + objectName,objectId);
			if (!getFileExtensionExists(objectName)) {
				objectName = objectName + MimeUtils.guessExtensionFromMimeType(inputStream.getMimeType());
			}

			File fileName = new File(gettingFolderPath(this.storeSettings.getFileLocation(), gettingDocNamePath(path))
					+ File.separator + objectName);

			if (!fileName.exists()) {
				LOG.error("setContent exception: {}", "File not found");
			}
			if (oldFileName != null) {
				File oldFile = new File(oldFileName);
				oldFile.delete();
			}
			writeContent(fileName, inputStream.getStream());

		} catch (Exception e) {
			LOG.error("setContent exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new CmisObjectNotFoundException(e.getMessage(), e);
		}
	}

	@Override
	public void deleteFolder(String folderName) {
		LOG.info("Delete Folder:{}" + folderName);
		String respositoryRoot = null;
		try {
			LOG.debug("Delete Folder:{}" + folderName);
			String rootpath = this.storeSettings.getFileLocation();
			if (rootpath == null) {
				LOG.error("Local storage path is undefined");
				throw new CmisConstraintException("Local storage path is undefined");
			}
			respositoryRoot = rootpath + "\\" + folderName;
			File deleteSourceDir = new File(respositoryRoot);
			FileUtils.deleteDirectory(deleteSourceDir);
		} catch (Exception e) {
			LOG.error("createLocalStorageService exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
		}

	}

	@Override
	public void setStoreSettings(IRepositoryStoreSettings storeSettings) {
		if (storeSettings instanceof FileSystemStoreSettings) {
			FileSystemStoreSettings fileStore = (FileSystemStoreSettings) storeSettings;
			String rootpath = fileStore.getFileLocation();
			if (rootpath == null) {
				LOG.error("Local storage path is undefined");
				throw new CmisConstraintException("Local storage path is undefined");
			}
			File directory = new File(rootpath);
			if (!directory.exists()) {
				directory.mkdirs();
			}
			this.storeSettings = (FileSystemStoreSettings) storeSettings;
		} else {
			LOG.error("writeContent exception: {}", "Respository Setting details not valid");
			throw new IllegalArgumentException("Respository Setting details not valid");
		}
	}

	private static String createSourceInTarget(String targetPath, String source)
			throws IOException, IllegalArgumentException {
		try {
			String[] folderNames = source.split("/");
			String lastToken = folderNames[folderNames.length - 1];
			targetPath = targetPath + "\\" + lastToken;
			File folderDirectory = new File(targetPath);
			folderDirectory.mkdirs();
			return targetPath;
		} catch (Exception e) {
			LOG.error("Create Folder exception: {}, {}", e.getMessage(), ExceptionUtils.getStackTrace(e));
			throw new IllegalArgumentException("Could not create Folder: " + e.getMessage());
		}

	}

	private static String gettingDocNamePath(String path) {
		String[] folderNames = path.split("/");
		String root = null;
		String lastToken = folderNames[folderNames.length - 1];
		for (String folderName : folderNames) {
			if (!folderName.isEmpty()) {
				if (lastToken != folderName) {
					if (root == null) {
						root = "/" + folderName;
					} else {
						root = root + "/" + folderName;
					}

				}
			}
		}
		if (root == null) {
			root = "/";
		}
		return root;

	}

	private static String gettingFolderPath(String rootPath, String path) {
		String folderPath = rootPath + path.replace("/", File.separator);
		return folderPath;
	}

	private static boolean getFileExtensionExists(String objectName) {
		if (objectName.contains(".")) {
			return true;
		}
		return false;
	}

}
