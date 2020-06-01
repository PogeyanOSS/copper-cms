package com.pogeyan.cmis.api;

import java.io.InputStream;
import java.math.BigInteger;
import java.util.List;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.data.ContentStream;

public class CustomContentStream implements ContentStream {

	private String fieldName;
	private String filename;
	private BigInteger length;
	private String mimetype;
	private transient InputStream stream;

	/**
	 * Constructor.
	 */
	public CustomContentStream() {
	}

	/**
	 * Constructor.
	 */
	public CustomContentStream(String fieldName, String filename, BigInteger length, String mimetype,
			InputStream stream) {
		this.fieldName = fieldName;
		this.filename = filename;
		this.length = length;
		this.mimetype = mimetype;
		this.stream = stream;
	}

	public CustomContentStream(String filename, BigInteger length, String mimetype, InputStream stream) {
		super();
		this.filename = filename;
		this.length = length;
		this.mimetype = mimetype;
		this.stream = stream;
	}

	@Override
	public String getFileName() {
		return filename;
	}

	public void setFileName(String filename) {
		this.filename = filename;
	}

	@Override
	public long getLength() {
		return length == null ? -1 : length.longValue();
	}

	@Override
	public BigInteger getBigLength() {
		return length;
	}

	public void setLength(BigInteger length) {
		this.length = length;
	}

	@Override
	public String getMimeType() {
		return mimetype;
	}

	public void setMimeType(String mimeType) {
		this.mimetype = mimeType;
	}

	@Override
	public InputStream getStream() {
		return stream;
	}

	public void setStream(InputStream stream) {
		this.stream = stream;
	}

	public String getFieldName() {
		return fieldName;
	}

	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}

	@Override
	public String toString() {
		return "ContentStream [filename=" + filename + ", length=" + length + ", MIME type=" + mimetype
				+ ", has stream=" + (stream != null) + "]" + super.toString();
	}

	@Override
	public List<CmisExtensionElement> getExtensions() {
		return null;
	}

	@Override
	public void setExtensions(List<CmisExtensionElement> extensions) {

	}
}
