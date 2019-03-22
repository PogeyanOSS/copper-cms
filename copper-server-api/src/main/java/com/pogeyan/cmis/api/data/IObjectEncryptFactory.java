package com.pogeyan.cmis.api.data;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectEncryptFactory {

	public IEncryptStoreSetting getEncryptStoreSetting();

	public IObjectEncryptService getEncryptionService(String repositoryId) throws InvalidTargetObjectTypeException;
}
