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
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.client.api.OperationContext;
import org.apache.chemistry.opencmis.commons.data.Ace;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.RenditionData;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlEntryImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.AccessControlPrincipalDataImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.IDocumentObject;
import com.pogeyan.cmis.api.data.common.AccessControlListImplExt;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RenditionDataImpl;

public class CmisUtils {

	private static final Logger LOG = LoggerFactory.getLogger(CmisUtils.class.getName());
	public static final String RENDITION_MIME_TYPE_IMAGE = "image/";
	public static final String RENDITION_MIME_TYPE_PDF = "application/pdf";
	public static final String RENDITION_MIME_TYPE_POWERPOINT = "powerpoint";
	public static final String RENDITION_MIME_TYPE_AUDIO = "audio/";
	public static final String RENDITION_MIME_TYPE_VIDEO = "video/";
	public static final String RENDITION_MIME_TYPE_PLAINTEXT = "text/plain";
	public static final String RENDITION_MIME_TYPE_EXCEL = "excel";
	public static final String RENDITION_MIME_TYPE_HTML = "text/html";
	public static final String RENDITION_MIME_TYPE_WORD = "wordoc";
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
				Set<Ace> aces = aceList.stream().collect(
						Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(Ace::getPrincipalId))));
				aceList = new ArrayList<Ace>(aces);
				AccessControlListImplExt aclImp = new AccessControlListImplExt(aceList);
				return aclImp;
			} else {
				return getAclFor(principalId, permission);
			}

		}

		public OperationContext createOperationContext() {
			// TODO Auto-generated method stub
			return null;
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
				return mimeType.startsWith("image/")
						|| mimeType.equals("application/vnd.openxmlformats-officedocument.wordprocessingml.document")
						|| mimeType.equals("application/msword")
						|| mimeType.equals("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
						|| mimeType.equals("application/vnd.ms-excel")
						|| mimeType.equals("application/vnd.openxmlformats-officedocument.presentationml.slideshow")
						|| mimeType.equals("application/vnd.openxmlformats-officedocument.presentationml.presentation")
						|| mimeType.equals("application/vnd.ms-powerpoint") || mimeType.equals("application/pdf")
						|| mimeType.equals("text/html") || mimeType.startsWith("audio/")
						|| mimeType.startsWith("video/") || mimeType.equals("text/plain");
			} else

			{
				return false;
			}
		}

		public static boolean testRenditionFilterForImage(String[] formats) {
			if (formats.length == 1 && null != formats[0] && formats[0].equals("cmis:none")) {
				return false;
			} else {
				return arrayContainsString(formats, "*") || arrayContainsString(formats, "image/*")
						|| arrayContainsString(formats, "image/") || arrayContainsString(formats, "application/pdf")
						|| arrayContainsString(formats, "application/vnd.ms-powerpoint")
						|| arrayContainsString(formats,
								"application/vnd.openxmlformats-officedocument.presentationml.presentation")
						|| arrayContainsString(formats,
								"application/vnd.openxmlformats-officedocument.presentationml.slideshow")
						|| arrayContainsString(formats, "audio/") || arrayContainsString(formats, "text/plain")
						|| arrayContainsString(formats, "text/plain") || arrayContainsString(formats, "text/html")
						|| arrayContainsString(formats,
								"application/vnd.openxmlformats-officedocument.wordprocessingml.document")
						|| arrayContainsString(formats, "application/msword") || arrayContainsString(formats, "video/");
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
				if (so.getBaseId() == BaseTypeId.CMIS_DOCUMENT) {
					IDocumentObject documentData = null;
					documentData = DBUtils.DocumentDAO.getDocumentByObjectId(repositoryId, so.getId(), null);
					mimeType = documentData.getContentStreamMimeType();
					LOG.info("the rendition can be seen through this id:{}, and mimeType is set to:{}", so.getId(),
							mimeType);
				}
				List<RenditionData> renditions = new ArrayList<RenditionData>(1);
				RenditionDataImpl rendition = new RenditionDataImpl();

				if (mimeType == null) {
					LOG.error("mimeType is not set");
				}
				if (mimeType != null) {

					if (mimeType.startsWith("image/")) {
						rendition.setBigHeight(BigInteger.valueOf(THUMBNAIL_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(THUMBNAIL_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_IMAGE);
					} else if (mimeType.equals("application/vnd.ms-powerpoints")
							|| mimeType.equals("application/vnd.openxmlformats-officedocument.presentationml.slideshow")
							|| mimeType
									.equals("application/vnd.openxmlformats-officedocument.presentationml.presentation")
							|| mimeType.equals("application/vnd.ms-powerpoint")) {

						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_POWERPOINT);
					} else if (mimeType.equals("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
							|| mimeType.equals("application/vnd.ms-excel")) {

						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_EXCEL);
					} else if (mimeType.startsWith("audio/")) {
						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_AUDIO);
					} else if (mimeType.startsWith("video/")) {
						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_VIDEO);
					} else if (mimeType.equals("text/plain")) {
						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_PLAINTEXT);
					} else if (mimeType.equals("text/html")) {
						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_HTML);
					} else if (mimeType
							.equals("application/vnd.openxmlformats-officedocument.wordprocessingml.document")
							|| mimeType.equals("application/msword")) {
						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_WORD);
					} else {
						rendition.setBigHeight(BigInteger.valueOf(ICON_SIZE));
						rendition.setBigWidth(BigInteger.valueOf(ICON_SIZE));
						rendition.setMimeType(RENDITION_MIME_TYPE_PDF);
					}
				}
				rendition.setKind("cmis:*");
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
