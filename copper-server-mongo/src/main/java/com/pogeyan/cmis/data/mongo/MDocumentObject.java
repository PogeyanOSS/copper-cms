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
package com.pogeyan.cmis.data.mongo;

import java.io.Serializable;

import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Field;
import org.mongodb.morphia.annotations.Index;
import org.mongodb.morphia.annotations.IndexOptions;
import org.mongodb.morphia.annotations.Indexes;

import com.pogeyan.cmis.api.data.IDocumentObject;

@Entity(value = "objectData", noClassnameStored = true)
@Indexes(@Index(fields = { @Field("name") }, options = @IndexOptions(unique = true)))
public class MDocumentObject extends MBaseObject implements IDocumentObject, Serializable {
	private static final long serialVersionUID = 4325001035854649673L;
	private Boolean isImmutable;
	private Boolean isLatestVersion;
	private Boolean isMajorVersion;
	private Boolean isLatestMajorVersion;
	private Boolean isPrivateWorkingCopy;
	private String versionLabel;
	private String versionSeriesId;
	private String versionReferenceId;
	private Boolean isVersionSeriesCheckedOut;
	private String versionSeriesCheckedOutBy;
	private String versionSeriesCheckedOutId;
	private String checkinComment;
	private Long contentStreamLength;
	private String contentStreamMimeType;
	private String contentStreamFileName;
	private String contentStreamId;
	private String previousVersionObjectId;

	public MDocumentObject() {
		super();
	}

	public MDocumentObject(MBaseObject baseObject, Boolean isImmutable, Boolean isLatestVersion, Boolean isMajorVersion,
			Boolean isLatestMajorVersion, Boolean isPrivateWorkingCopy, String versionLabel, String versionSeriesId,
			String versionReferenceId, Boolean isVersionSeriesCheckedOut, String versionSeriesCheckedOutBy,
			String versionSeriesCheckedOutId, String checkinComment, Long contentStreamLength,
			String contentStreamMimeType, String contentStreamFileName, String contentStreamId,
			String previousVersionObjectId) {
		super(baseObject.getName(), baseObject.getBaseId(), baseObject.getTypeId(),
				baseObject.getRepositoryId(), baseObject.getSecondaryTypeIds(), baseObject.getDescription(),
				baseObject.getCreatedBy(), baseObject.getModifiedBy(), baseObject.getChangeToken(),
				baseObject.getInternalPath(), baseObject.getProperties(), baseObject.getPolicies(), baseObject.getAcl(),
				baseObject.getPath(), baseObject.getParentId());
		this.setId(baseObject.getId());
		this.isImmutable = isImmutable;
		this.isLatestVersion = isLatestVersion;
		this.isMajorVersion = isMajorVersion;
		this.isLatestMajorVersion = isLatestMajorVersion;
		this.isPrivateWorkingCopy = isPrivateWorkingCopy;
		this.versionLabel = versionLabel;
		this.versionSeriesId = versionSeriesId;
		this.versionReferenceId = versionReferenceId;
		this.isVersionSeriesCheckedOut = isVersionSeriesCheckedOut;
		this.versionSeriesCheckedOutBy = versionSeriesCheckedOutBy;
		this.versionSeriesCheckedOutId = versionSeriesCheckedOutId;
		this.checkinComment = checkinComment;
		this.contentStreamLength = contentStreamLength;
		this.contentStreamMimeType = contentStreamMimeType;
		this.contentStreamFileName = contentStreamFileName;
		this.contentStreamId = contentStreamId;
		this.previousVersionObjectId = previousVersionObjectId;
	}

	public Boolean getIsImmutable() {
		return isImmutable;
	}

	public void setIsImmutable(Boolean isImmutable) {
		this.isImmutable = isImmutable;
	}

	public Boolean getIsLatestVersion() {
		return isLatestVersion;
	}

	public void setIsLatestVersion(Boolean isLatestVersion) {
		this.isLatestVersion = isLatestVersion;
	}

	public Boolean getIsMajorVersion() {
		return isMajorVersion;
	}

	public void setIsMajorVersion(Boolean isMajorVersion) {
		this.isMajorVersion = isMajorVersion;
	}

	public Boolean getIsLatestMajorVersion() {
		return isLatestMajorVersion;
	}

	public void setIsLatestMajorVersion(Boolean isLatestMajorVersion) {
		this.isLatestMajorVersion = isLatestMajorVersion;
	}

	public Boolean getIsPrivateWorkingCopy() {
		return isPrivateWorkingCopy;
	}

	public void setIsPrivateWorkingCopy(Boolean isPrivateWorkingCopy) {
		this.isPrivateWorkingCopy = isPrivateWorkingCopy;
	}

	public String getVersionLabel() {
		return versionLabel;
	}

	public void setVersionLabel(String versionLabel) {
		this.versionLabel = versionLabel;
	}

	public String getVersionSeriesId() {
		return versionSeriesId;
	}

	public void setVersionSeriesId(String versionSeriesId) {
		this.versionSeriesId = versionSeriesId;
	}

	public Boolean getIsVersionSeriesCheckedOut() {
		return isVersionSeriesCheckedOut;
	}

	public void setIsVersionSeriesCheckedOut(Boolean isVersionSeriesCheckedOut) {
		this.isVersionSeriesCheckedOut = isVersionSeriesCheckedOut;
	}

	public String getVersionSeriesCheckedOutBy() {
		return versionSeriesCheckedOutBy;
	}

	public void setVersionSeriesCheckedOutBy(String versionSeriesCheckedOutBy) {
		this.versionSeriesCheckedOutBy = versionSeriesCheckedOutBy;
	}

	public String getVersionSeriesCheckedOutId() {
		return versionSeriesCheckedOutId;
	}

	public void setVersionSeriesCheckedOutId(String versionSeriesCheckedOutId) {
		this.versionSeriesCheckedOutId = versionSeriesCheckedOutId;
	}

	public String getCheckinComment() {
		return checkinComment;
	}

	public void setCheckinComment(String checkinComment) {
		this.checkinComment = checkinComment;
	}

	public Long getContentStreamLength() {
		return contentStreamLength;
	}

	public void setContentStreamLength(Long contentStreamLength) {
		this.contentStreamLength = contentStreamLength;
	}

	public String getContentStreamMimeType() {
		return contentStreamMimeType;
	}

	public void setContentStreamMimeType(String contentStreamMimeType) {
		this.contentStreamMimeType = contentStreamMimeType;
	}

	public String getContentStreamFileName() {
		return contentStreamFileName;
	}

	public void setContentStreamFileName(String contentStreamFileName) {
		this.contentStreamFileName = contentStreamFileName;
	}

	public String getContentStreamId() {
		return contentStreamId;
	}

	public void setContentStreamId(String contentStreamId) {
		this.contentStreamId = contentStreamId;
	}

	public String getPreviousVersionObjectId() {
		return previousVersionObjectId;
	}

	public void setPreviousVersionObjectId(String previousVersionObjectId) {
		this.previousVersionObjectId = previousVersionObjectId;
	}

	public String getVersionReferenceId() {
		return versionReferenceId;
	}

	public void setVersionReferenceId(String versionReferenceId) {
		this.versionReferenceId = versionReferenceId;
	}
}