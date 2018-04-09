package com.pogeyan.cmis.data.services;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import org.apache.chemistry.opencmis.commons.definitions.DocumentTypeDefinition;

import com.pogeyan.cmis.api.data.services.MDocumentTypeManagerDAO;
import com.pogeyan.cmis.data.jdo.JDocumentTypeObject;

public class JDocumentTypeManagerDAOImpl implements MDocumentTypeManagerDAO {

	@SuppressWarnings("unchecked")
	@Override
	public DocumentTypeDefinition getByTypeId(String repositoryId, String typeId) {
		PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
		Query<?> query = pm.newQuery(JDocumentTypeObject.class);
		query.declareParameters("String docId");
		query.setFilter("this.id == docId");
		Map<String, String> paramValues = new HashMap<>();
		paramValues.put("docId", typeId);
		List<JDocumentTypeObject> typeObject = (List<JDocumentTypeObject>) query.executeWithMap(paramValues);
		if (typeObject.size() > 0) {
			JDocumentTypeObject type = typeObject.get(0);
			type.setPropertyDefinition(type.getListPropertyDefinition());
			return type;
		}
		return null;

	}

}
