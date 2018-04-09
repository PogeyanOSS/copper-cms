package com.pogeyan.cmis.data.services;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.jdo.Extent;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.data.IBaseObject;
import com.pogeyan.cmis.api.data.common.TokenChangeType;
import com.pogeyan.cmis.api.data.services.MNavigationServiceDAO;
import com.pogeyan.cmis.api.uri.UriParser;
import com.pogeyan.cmis.api.uri.expression.ExceptionVisitExpression;
import com.pogeyan.cmis.api.uri.expression.ExpressionParserException;
import com.pogeyan.cmis.api.uri.expression.FilterExpression;

public class JNavigationServiceDAOImpl implements MNavigationServiceDAO {
	private static final Logger LOG = LoggerFactory.getLogger(JNavigationServiceDAOImpl.class);

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends IBaseObject> getChildren(String repositoryId, String typeId, String path,
			String[] principalIds, boolean aclPropagation, int maxItems, int skipCount, String orderBy,
			String[] mappedColumns, String filterExpression) {
		try {
			Object filterString = null;
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, true);
				if (objectClass != null) {
					Extent<?> QueryExtent = pm.getExtent(objectClass, true);
					Query<?> query = pm.newQuery(QueryExtent);
					query.declareVariables(
							"com.pogeyan.cmis.data.jdo.JAclImpl jacl; com.pogeyan.cmis.data.jdo.JAceImpl jace;");
					String declareParameters = null;
					String filterParameter = null;
					Map<String, Object> fieldNames = null;
					if (!StringUtils.isEmpty(filterExpression)) {
						try {
							FilterExpression expression = UriParser.parseFilter(filterExpression);
							filterString = expression.accept(new JDOExpressionVisitor(repositoryId, typeId));
							LOG.info("filterString for folder: {} , folderId: {}", filterString, path);
						} catch (ExpressionParserException | ExceptionVisitExpression e) {
							System.out.println(e);
						}
					}
					if (aclPropagation) {
						List<String> principalId = Stream.of(principalIds).distinct()
								.collect(Collectors.<String>toList());
						String[] principalIdDistinct = principalId.toArray(new String[principalId.size()]);
						declareParameters = JDOHelper.Impl.getACLDeclareParameter(principalIdDistinct)
								+ ",String path,int tokenType";
						filterParameter = "this.id == jacl.baseId && this.id == jace.baseId && this.internalPath == path && token.changeType != tokenType && ("
								+ JDOHelper.Impl.getACLFilterParameter(principalIdDistinct) + ")";
						if (filterString != null) {
							filterParameter = "(" + filterParameter + " && " + filterString + ")";
						}
						fieldNames = JDOHelper.Impl.getACLMap(principalIdDistinct);
					} else {
						declareParameters = "String path,int tokenType";
						filterParameter = "this.internalPath == path && token.changeType != tokenType";
						if (filterString != null) {
							filterParameter = "(" + filterParameter + " && " + filterString + ")";
						}
						fieldNames = new HashMap<>();
					}
					query.declareParameters(declareParameters);
					query.setFilter(filterParameter);
					fieldNames.put("path", path);
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					if (maxItems > 0) {
						query.setRange(skipCount, skipCount + maxItems);
					} else {
						query.setRange(skipCount, 0);
					}
					if (!StringUtils.isEmpty(orderBy)) {
						query.setOrdering(getOrderString(orderBy));
					}
					if (mappedColumns != null && mappedColumns.length > 0) {
					}
					List<? extends IBaseObject> result = (List<? extends IBaseObject>) query.executeWithMap(fieldNames);
					return getResultWithProp(result);
				}
			}

		} catch (Exception e) {
			LOG.error("getChildren Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	@Override
	public long getChildrenSize(String repositoryId, String typeId, String path, String[] principalIds,
			boolean aclPropagation) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, true);
				if (objectClass != null) {
					Extent<?> QueryExtent = pm.getExtent(objectClass, true);
					Query<?> query = pm.newQuery(QueryExtent);
					query.declareVariables(
							"com.pogeyan.cmis.data.jdo.JAclImpl jacl; com.pogeyan.cmis.data.jdo.JAceImpl jace;");
					String declareParameters = null;
					String filterParameter = null;
					Map<String, Object> fieldNames = null;
					if (aclPropagation) {
						List<String> principalId = Stream.of(principalIds).distinct()
								.collect(Collectors.<String>toList());
						String[] principalIdDistinct = principalId.toArray(new String[principalId.size()]);

						declareParameters = JDOHelper.Impl.getACLDeclareParameter(principalIdDistinct)
								+ ",String path,int tokenType";
						filterParameter = "this.id == jacl.baseId && this.id == jace.baseId && this.internalPath == path && token.changeType != tokenType && ("
								+ JDOHelper.Impl.getACLFilterParameter(principalIdDistinct) + ")";

						fieldNames = JDOHelper.Impl.getACLMap(principalIdDistinct);
					} else {
						declareParameters = "String path,int tokenType";
						filterParameter = "this.internalPath == path && token.changeType != tokenType";
						fieldNames = new HashMap<>();
					}
					query.declareParameters(declareParameters);
					query.setFilter(filterParameter);
					fieldNames.put("path", path);
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					List<IBaseObject> result = (List<IBaseObject>) query.executeWithMap(fieldNames);
					return result.size();
				}
			}

		} catch (Exception e) {
			LOG.error("getChildrenSize Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return 0;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<? extends IBaseObject> getDescendants(String repositoryId, String typeId, String path,
			String[] principalIds, boolean aclPropagation, String[] mappedColumns, String filterExpression) {
		try {
			Object filterString = null;
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, true);
				if (objectClass != null) {
					Extent<?> QueryExtent = pm.getExtent(objectClass, true);
					Query<?> query = pm.newQuery(QueryExtent);
					String declareParameters = null;
					String filterParameter = null;
					Map<String, Object> fieldNames = null;
					if (!StringUtils.isEmpty(filterExpression)) {
						try {
							FilterExpression expression = UriParser.parseFilter(filterExpression);
							filterString = expression.accept(new JDOExpressionVisitor(repositoryId, typeId));
							LOG.info("filterString for folder: {} , folderId: {}", filterString, path);
						} catch (ExpressionParserException | ExceptionVisitExpression e) {
							System.out.println(e);
						}
					}
					if (aclPropagation) {
						query.declareVariables(
								"com.pogeyan.cmis.data.jdo.JAclImpl jacl; com.pogeyan.cmis.data.jdo.JAceImpl jace;");
						List<String> principalId = Stream.of(principalIds).distinct()
								.collect(Collectors.<String>toList());
						String[] principalIdDistinct = principalId.toArray(new String[principalId.size()]);

						declareParameters = JDOHelper.Impl.getACLDeclareParameter(principalIdDistinct)
								+ ",int tokenType";
						filterParameter = "this.id == jacl.baseId && this.id == jace.baseId && this.internalPath.matches('.*"
								+ path.trim() + ".*') && token.changeType != tokenType && ("
								+ JDOHelper.Impl.getACLFilterParameter(principalIdDistinct) + ")";
						if (filterString != null) {
							filterParameter = "(" + filterParameter + " && " + filterString + ")";
						}
						fieldNames = JDOHelper.Impl.getACLMap(principalIdDistinct);
					} else {
						declareParameters = "int tokenType";
						filterParameter = "this.internalPath.matches('.*" + path.trim()
								+ ".*') && token.changeType != tokenType";
						if (filterString != null) {
							filterParameter = "(" + filterParameter + " && " + filterString + ")";
						}
						fieldNames = new HashMap<>();
					}
					query.declareParameters(declareParameters);
					query.setFilter(filterParameter);
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					return (List<? extends IBaseObject>) query.executeWithMap(fieldNames);
				}
			}

		} catch (Exception e) {
			LOG.error("getDescendants Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<? extends IBaseObject> getFolderTreeIds(String repositoryId, String typeId, String path,
			String[] principalIds, boolean aclPropagation) {
		try {
			PersistenceManager pm = JDOServiceImpl.getInstance().initializePersistenceManager(repositoryId);
			if (pm != null) {
				Class<?> objectClass = JDOHelper.Impl.load(repositoryId, typeId, true);
				if (objectClass != null) {
					Extent<?> QueryExtent = pm.getExtent(objectClass, true);
					Query<?> query = pm.newQuery(QueryExtent);
					String declareParameters = null;
					String filterParameter = null;
					Map<String, Object> fieldNames = null;
					if (aclPropagation) {
						query.declareVariables(
								"com.pogeyan.cmis.data.jdo.JAclImpl jacl; com.pogeyan.cmis.data.jdo.JAceImpl jace;");
						List<String> principalId = Stream.of(principalIds).distinct()
								.collect(Collectors.<String>toList());
						String[] principalIdDistinct = principalId.toArray(new String[principalId.size()]);

						declareParameters = JDOHelper.Impl.getACLDeclareParameter(principalIdDistinct)
								+ ",String baseId,int tokenType";
						filterParameter = "this.id == jacl.baseId && this.id == jace.baseId && this.internalPath.matches('.*"
								+ path.trim() + ".*') && this.baseId == baseId && token.changeType != tokenType && ("
								+ JDOHelper.Impl.getACLFilterParameter(principalIdDistinct) + ")";
						fieldNames = JDOHelper.Impl.getACLMap(principalIdDistinct);
					} else {
						declareParameters = "int tokenType";
						filterParameter = "this.internalPath.matches('.*" + path.trim()
								+ "*') && this.baseId == baseId && token.changeType != tokenType";
						fieldNames = new HashMap<>();
					}
					query.declareParameters(declareParameters);
					query.setFilter(filterParameter);
					fieldNames.put("baseId", "CMIS_FOLDER");
					fieldNames.put("tokenType", TokenChangeType.DELETED.value());
					return (List<? extends IBaseObject>) query.executeWithMap(fieldNames);
				}
			}
		} catch (Exception e) {
			LOG.error("getFolderTreeIds Exception: {}, {}", e.toString(), ExceptionUtils.getStackTrace(e));
			e.printStackTrace();
		}
		return new ArrayList<>();
	}

	private List<? extends IBaseObject> getResultWithProp(List<? extends IBaseObject> result) {
		List<IBaseObject> results = new ArrayList<>();
		if (result != null && result.size() > 0) {
			for (IBaseObject resultObject : result) {
				if (resultObject != null) {
					String propString = resultObject.getPropString();
					if (propString != null) {
						String[] propFields = propString.split(",");
						Map<String, Object> props = Stream.of(propFields).filter(elem -> elem.split("=").length > 1)
								.map(elem -> elem.split("=")).collect(Collectors.toMap(e -> e[0], e -> e[1]));
						resultObject.setProperties(props);
						results.add(resultObject);
					}
				}
			}
		}
		return results;
	}

	private String getOrderString(String orderBy) {
		String[] orderbyArray = orderBy.split(",");
		String jdoOrderBy = Stream.of(orderbyArray).map(t -> "this." + t).collect(Collectors.joining(","));
		return jdoOrderBy;

	}

}
