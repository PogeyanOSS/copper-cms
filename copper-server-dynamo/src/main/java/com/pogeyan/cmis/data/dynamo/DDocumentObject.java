package com.pogeyan.cmis.data.dynamo;

import com.pogeyan.cmis.api.data.IDocumentObject;

public class DDocumentObject extends DBaseObject implements IDocumentObject {

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
	

	public DDocumentObject() {
		super();
	}
	
	public DDocumentObject(DBaseObject baseObject, Boolean isImmutable, Boolean isLatestVersion, Boolean isMajorVersion,
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
