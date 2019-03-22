package com.pogeyan.cmis.api.data;

import java.util.List;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectEncryptService {

	public void setEncryptStoreSettings(IEncryptStoreSetting encryptSettings) throws InvalidTargetObjectTypeException;

	public boolean beforeEncrypt(String repositoryId, String typeId, String propId, List<String> properties);

	public Object afterEncrypt(String repositoryId, String typeId, String propId, Object propValue);
}
