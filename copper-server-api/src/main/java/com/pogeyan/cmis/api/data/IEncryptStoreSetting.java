package com.pogeyan.cmis.api.data;

public interface IEncryptStoreSetting {

	public String getEncryptType();

	public void setEncryptStoreSetting(String encryption, String kms_id, String kms_arn, String kmsSettings);
}
