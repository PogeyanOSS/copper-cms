package com.pogeyan.cmis.data.services;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;

import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.data.jdo.JDocumentTypeObject;
import com.pogeyan.cmis.data.jdo.JTypeObject;

public class JDocumentTypeManagerDAOImpl implements MDocumentTypeManagerDAO {

	@SuppressWarnings("unchecked")
	@Override
	public DocumentTypeDefinition getByTypeId(String repositoryId, String typeId) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Query query = pm.newQuery(JTypeObject.class);
		query.declareParameters("String docId");
		query.setFilter("id == docId");
		Map<String, String> paramValues = new HashMap<>();
		paramValues.put("docId", typeId);
		List<JTypeObject> typeObject = (List<JTypeObject>) query.executeWithMap(paramValues);
		JDocumentTypeObject object = typeObject.get(0).getDocTypeDefinition();
		object.setPropertyDefinition(typeObject.get(0).getPropertyDefinition());
		return object;

	}

}
