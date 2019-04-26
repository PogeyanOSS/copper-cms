package com.pogeyan.cmis.api.data;

import java.util.List;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectEncryptService {

	public void setEncryptStoreSettings(IEncryptStoreSetting encryptSettings) throws InvalidTargetObjectTypeException;

	public boolean encrypt(String repositoryId, String typeId, String propId, List<String> properties);

	public Object decrypt(String repositoryId, String typeId, String propId, Object propValue);

	public boolean shouldEncrypt(String repositoryId, String typeId, String propId);
}
