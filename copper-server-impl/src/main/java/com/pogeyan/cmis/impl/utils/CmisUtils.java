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
package com.pogeyan.cmis.impl.utils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.RenditionData;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.api.storage.IStorageService;
import com.pogeyan.cmis.impl.factory.StorageServiceFactory;

public class CmisUtils {

	private static final Logger LOG = LoggerFactory.getLogger(CmisUtils.class.getName());
	// private static final int BUFFER_SIZE = 65536;
	public static final String RENDITION_MIME_TYPE_JPEG = "image/jpeg";
	public static final String RENDITION_MIME_TYPE_PNG = "image/png";
	public static final String RENDITION_SUFFIX = "-rendition";
	public static final int THUMBNAIL_SIZE = 100;
	public static final int ICON_SIZE = 32;

	public static class Object {
		public static Acl getAclFor(String principalId, String permission) {
			List<String> permissions = new ArrayList<String>();
			permissions.add(permission);
			return getAclFor(principalId, permissions);
		}

		public static Acl getAclFor(String principalId, List<String> permissions) {
			List<Ace> aceList = new ArrayList<Ace>();
			AccessControlEntryImpl ace = new AccessControlEntryImpl(new AccessControlPrincipalDataImpl(principalId),
					permissions);
			aceList.add(ace);
			AccessControlListImplExt aclImp = new AccessControlListImplExt(aceList);
			return aclImp;
		}

		public static Acl getAcl(Acl acl, String principalId, String permission) {
			if (acl != null) {
				List<Ace> aceList = acl.getAces();
				AccessControlEntryImpl ace = new AccessControlEntryImpl(new AccessControlPrincipalDataImpl(principalId),
						Arrays.asList(permission));
				aceList.add(ace);
				return acl;
			} else {
				return getAclFor(principalId, permission);
			}

		}
	}

	public static class Rendition {

		public static boolean hasRendition(IBaseObject so, String user, String repositoryId) {
			IDocumentObject documentData = null;
			if (so.getBaseId() == BaseTypeId.CMIS_FOLDER) {
				return true;
			}
			if (so.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
				documentData = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, so.getId(), null);
				if (documentData.getContentStreamLength() == null) {
					return false;
				}

				String mimeType = documentData.getContentStreamMimeType();

				return isImage(mimeType) || isAudio(mimeType) || isVideo(mimeType) || isPDF(mimeType)
						|| isPowerpoint(mimeType) || isExcel(mimeType) || isWord(mimeType) || isHtml(mimeType)
						|| isPlainText(mimeType);
			} else

			{
				return false;
			}
		}

		private static boolean isImage(String mimeType) {
			return mimeType.startsWith("image/");
		}

		private static boolean isWord(String mimeType) {
			return mimeType.equals("application/vnd.openxmlformats-officedocument.wordprocessingml.document")
					|| mimeType.equals("application/ms-word");
		}

		private static boolean isExcel(String mimeType) {
			return mimeType.equals("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
					|| mimeType.equals("application/vnd.ms-excel");
		}

		private static boolean isPowerpoint(String mimeType) {
			return mimeType.equals("application/vnd.openxmlformats-officedocument.presentationml.slideshow")
					|| mimeType.equals("application/vnd.openxmlformats-officedocument.presentationml.presentation")
					|| mimeType.equals("application/vnd.ms-powerpoint");
		}

		private static boolean isPDF(String mimeType) {
			return mimeType.equals("application/pdf");
		}

		private static boolean isHtml(String mimeType) {
			return mimeType.equals("text/html");
		}

		private static boolean isAudio(String mimeType) {
			return mimeType.startsWith("audio/");
		}

		private static boolean isVideo(String mimeType) {
			return mimeType.startsWith("video/");
		}

		private static boolean isPlainText(String mimeType) {
			return mimeType.equals("text/plain");
		}

		public static boolean testRenditionFilterForImage(String[] formats) {
			if (formats.length == 1 && null != formats[0] && formats[0].equals("cmis:none")) {
				return false;
			} else {
				return arrayContainsString(formats, "*") || arrayContainsString(formats, "image/*")
						|| arrayContainsString(formats, "image/jpeg");
			}
		}

		private static boolean arrayContainsString(String[] formats, String val) {
			for (String s : formats) {
				if (val.equals(s)) {
					return true;
				}
			}
			return false;
		}

		public static List<RenditionData> getRenditions(String repositoryId, IBaseObject so, String renditionFilter,
				long maxItems, long skipCount, String user) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("getRenditions data using this id:{}", so.getId());
			}
			String tokenizer = "[\\s;]";
			if (null == renditionFilter) {
				return null;
			}
			String[] formats = renditionFilter.split(tokenizer);
			boolean isImageRendition = CmisUtils.Rendition.testRenditionFilterForImage(formats);

			if (isImageRendition && hasRendition(so, user, repositoryId)) {
				String mimeType = null;
				if (so.getBaseId() == BaseTypeId.CMIS_FOLDER) {
					mimeType = "image/png";
				} else {
					try {
						IDocumentObject documentData = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId,
								so.getId(), null);
						if (documentData == null) {
							LOG.error("getRenditions Object is null in {} repository!", repositoryId);
							throw new CmisObjectNotFoundException("Object must not be null!");
						}
						Map<String, String> parameters = RepositoryManagerFactory.getFileDetails(repositoryId);
						IStorageService localService = StorageServiceFactory.createStorageService(parameters);
						if (documentData.getContentStreamFileName() != null) {
							ContentStream contentStream = localService.getContent(
									documentData.getContentStreamFileName(), so.getPath(),
									documentData.getContentStreamMimeType(),
									BigInteger.valueOf(documentData.getContentStreamLength()));
							if (contentStream.equals(null)) {
								LOG.error("ContentStream should not be :{}", contentStream);
								throw new CmisObjectNotFoundException("Unkonwn ObjectId");
							}
							mimeType = contentStream.getMimeType();
						}
					} catch (Exception e) {
						LOG.error("getRenditions Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
						throw new CmisObjectNotFoundException(e.toString());
					}
				}

				List<RenditionData> renditions = new ArrayList<RenditionData>(1);
				org.apache.chemistry.opencmis.commons.impl.dataobjects.RenditionDataImpl rendition = new org.apache.chemistry.opencmis.commons.impl.dataobjects.RenditionDataImpl();

				if (mimeType != null) {
					if (mimeType.equals("image/jpeg")) {
						rendition.setBigHeight(BigInteger.valueOf(THUMBNAIL_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(THUMBNAIL_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_JPEG);
					} else {
						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_PNG);
					}

				}
				rendition.setKind("cmis:thumbnail");
				rendition.setRenditionDocumentId(so.getId().toString());
				rendition.setStreamId(so.getId() + RENDITION_SUFFIX);
				rendition.setBigLength(BigInteger.valueOf(-1L));
				rendition.setTitle(so.getName());
				renditions.add(rendition);
				return renditions;
			} else {
				return null;
			}
		}
	}
}
