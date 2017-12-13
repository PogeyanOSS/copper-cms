package com.pogeyan.cmis.data.mongo;

import org.apache.olingo.odata2.api.uri.UriParser;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.apache.olingo.odata2.api.uri.expression.OrderByExpression;
import org.mongodb.morphia.query.Query;

import com.pogeyan.cmis.api.data.services.MDocumentObjectDAO;
import com.pogeyan.cmis.api.repo.RepositoryManagerFactory;
import com.pogeyan.cmis.data.mongo.services.MDocumentObjectDAOImpl;
import com.pogeyan.cmis.data.mongo.services.MongoClientFactory;
import com.pogeyan.cmis.repo.local.LocalRepoImpl;

public class App {
	public static void main(String[] args) throws Exception {
		RepositoryManagerFactory.getInstance().init(new LocalRepoImpl());
		MongoClientFactory dbFactory = (MongoClientFactory) MongoClientFactory.createDatabaseService();
		MDocumentObjectDAOImpl dao = (MDocumentObjectDAOImpl) dbFactory.getObjectService("TestRepo",
				MDocumentObjectDAO.class);
		Query<MDocumentObject> query = dao.createQuery();
		//String filterExpressionQuery = "name eq 'fd' and name ne 'sd'";
		String filterExpressionQuery = "startswith(name, 'fd')";
		FilterExpression expression = UriParser.parseFilter(null, null, filterExpressionQuery);
		Object filterExp = expression.accept(new MongoExpressionVisitor<MDocumentObject>(query));
		System.out.print("output: " + filterExp != null ? filterExp.toString() : "error");

		String orderByExpressionQuery = "name asc, repositoryId";
		OrderByExpression orderByExpression = UriParser.parseOrderBy(null, null, orderByExpressionQuery);
		Object orderByExp = orderByExpression.accept(new MongoExpressionVisitor<MDocumentObject>(query));
		System.out.print("output: " + orderByExp != null ? orderByExp.toString() : "error");
	}
}
