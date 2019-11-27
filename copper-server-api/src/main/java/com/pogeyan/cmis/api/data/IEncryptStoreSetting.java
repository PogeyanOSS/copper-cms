package com.pogeyan.cmis.api.data;

public interface IEncryptStoreSetting {

	public String getEncryptType();

	 public void setEncryptStoreSetting(String encryption, String kms_id, String kms_arn, String kms_region,
	            String kms_accessKeyId, String kms_secretAccessKey, String kmsSettings);
}
