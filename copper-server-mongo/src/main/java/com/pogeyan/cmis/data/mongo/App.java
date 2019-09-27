package com.pogeyan.cmis.data.mongo;

import java.util.HashMap;
import java.util.Map;

import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.mongodb.morphia.Morphia;
import org.mongodb.morphia.mapping.MappedClass;
import org.mongodb.morphia.mapping.Mapper;
import org.mongodb.morphia.query.Query;

import com.mongodb.MongoClient;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.api.uri.expression.ExceptionVisitExpression;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.data.mongo.services.MongoClientFactory;
import com.pogeyan.cmis.impl.uri.expression.ExpressionParserInternalError;
import com.pogeyan.cmis.repo.local.LocalRepoImpl;

public class App  {
	
		
	
	public static void main(String[] args)
			throws Exception, ExpressionParserException, ExpressionParserInternalError, ExceptionVisitExpression {
		Morphia morphia = new Morphia();
		RepositoryManagerFactory.getInstance().init(new LocalRepoImpl());
		MongoClientFactory dbFactory = (MongoClientFactory) MongoClientFactory.createDatabaseService();
		MongoClient mongoClient = dbFactory.getMongoClient("Tenant", "127.0.0.1", 27017, false);
		Datastore clientDatastore =  morphia.createDatastore(mongoClient, "ContentDB");
		
		Map<String, String> crunchifyComapnies = new HashMap<>();
        crunchifyComapnies.put("Google", "Mountain View, CA");
		crunchifyComapnies.put("Yahoo", "Santa Clara, CA");
		crunchifyComapnies.put("Microsoft", "Redmond, WA");
		crunchifyComapnies.put("Google", "Seattle, WA");
		crunchifyComapnies.put("Amazon", "Seattle, WA");
		if (crunchifyComapnies.containsKey("Google")){
		    System.out.println("HashMap: "
                           + crunchifyComapnies.get("Google"));
		}
//		Query<MDestObject> mDestObjectQuery = clientDatastore.find(MDestObject.class).disableValidation();
//		
//		
//		MDestObject resultObject = mDestObjectQuery.iterator().next();
		
//		Map<String, Object> mp = new HashMap<String, Object>(); 
//		Mapper mapper = morphia.getMapper();
//		CloneMapper helper = new CloneMapper(mapper);
//		String typeId = "checky:audtAnalytics";
//		if (typeId.contains(":")) {
//			typeId = typeId.replaceAll(":", "_");
//		}
//		helper.map(MapProxy.class, typeId);
//		MappedClass mc = mapper.getMappedClass(MapProxy.class);
//		mc.update();
//		Query<MapProxy> mapProxyQuery = clientDatastore.find(MapProxy.class).disableValidation();
//		MapProxy resultProxy = mapProxyQuery.iterator().next();

		
//		mp.put("Abc", "21e"); 
//		mp.put("Cde","dwq"); 
//		mp.put("Geeks", "qe3"); 
//		mp.put("Welcomes", "dw"); 
//		mp.put("You", "dwqdwq"); 
//		MapProxy mapProxy = new MapProxy();
//		mapProxy.setProperties(mp);
//		
//		MDestObject mDestObject = new MDestObject(); 
//		mDestObject.setProperties(mapProxy);
//		clientDatastore.save(mapProxy);
//        clientDatastore.save((MDestObject) mDestObject);
//		MDocumentObjectDAOImpl dao = (MDocumentObjectDAOImpl) dbFactory.getObjectService("TestRepo",
//				MDocumentObjectDAO.class);
//		Query<MDocumentObject> query = dao.createQuery();
//		// String filterExpressionQuery = "name eq 'fd' and name ne 'sd'";
//		String filterExpressionQuery = "contains(name, 'fd')";
//		FilterExpression expression = UriParser.parseFilter(filterExpressionQuery);
//		Object filterExp = expression.accept(new MongoExpressionVisitor<MDocumentObject>(query, null));
//		System.out.print("output: " + filterExp != null ? filterExp.toString() : "error");
//
//		String orderByExpressionQuery = "name asc, repositoryId desc";
//		OrderByExpression orderByExpression = UriParser.parseOrderBy(orderByExpressionQuery);
//		Object orderByExp = orderByExpression.accept(new MongoExpressionVisitor<MDocumentObject>(query, null));
//		System.out.print("output: " + orderByExp != null ? orderByExp.toString() : "error");
	}
}
