package com.pogeyan.cmis.api.data;

public interface IDocumentObject extends IBaseObject {
	public Boolean getIsImmutable();

	public Boolean getIsLatestVersion();

	public Boolean getIsMajorVersion();

	public Boolean getIsLatestMajorVersion();

	public Boolean getIsPrivateWorkingCopy();

	public String getVersionLabel();

	public String getVersionSeriesId();

	public Boolean getIsVersionSeriesCheckedOut();

	public String getVersionSeriesCheckedOutBy();

	public String getVersionSeriesCheckedOutId();

	public String getCheckinComment();

	public Long getContentStreamLength();

	public String getContentStreamMimeType();

	public String getContentStreamFileName();

	public String getContentStreamId();

	public String getPreviousVersionObjectId();

	public String getVersionReferenceId();
}
