package com.pogeyan.cmis.api.data;

public interface IObjectFlowStoreSetting {
	public String getType();

	public void setStoreSetting(String fileSetting, String accessKeyId, String secretAccessKey, String region,
			String encryption, String kms_id, String kms_arn, String kmsSettings);
}
