package com.pogeyan.cmis.api.data;

import java.util.List;
import java.util.Map;

import javax.management.modelmbean.InvalidTargetObjectTypeException;

public interface IObjectEncryptService {

	public void setEncryptStoreSettings(IEncryptStoreSetting encryptSettings) throws InvalidTargetObjectTypeException;

	public boolean encrypt(String repositoryId, String typeId, String propId, List<String> propertyValues,
			List<String> secondaryObjectTypeIdsValues, Map<String, List<String>> propertyDataMap);

	public Object decrypt(String repositoryId, String typeId, String propId, Object propValue,
			List<String> secondaryObjectTypeIdsValues, List<String> secondaryEncryptProps);

	public boolean shouldEncrypt(String repositoryId, String typeId, String propId,
			List<String> secondaryObjectTypeIdsValues, List<String> encryptProp);

}
