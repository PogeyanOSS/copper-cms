package com.pogeyan.cmis.ldap;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;

import org.apache.commons.lang3.StringUtils;

import com.pogeyan.cmis.ldap.model.LDAPLogin;
import com.pogeyan.cmis.ldap.model.LoginProperties;

public class LDAPUtils {
	private final static String contextFactory = "com.sun.jndi.ldap.LdapCtxFactory";
	private static final String LDAPHOST = "ldap://";
	private static final String DOMAIN = "dc";
	private static final String COMMON_NAME = "cn";
	private static final String SurName = "sn";
	private static final String OrganizationalUnit = "ou";
	private static final String Password = "userPassword";
	private static final String MAIL = "mail";
	private static final String People = "People";
	private static final String UID = "uid";
	private static final String LMCACL = "lmcACL";
	private static final String FacsimileTelephoneNumber = "facsimileTelephoneNumber";
	private static final String GidNumber = "gidNumber";
	private static final String UidNumber = "uidNumber";
	private static final String MemberOf = "memberOf";
	private static final String ACI = "aci";
	private static final String Permission = "permission";
	private static final String EntryUUID = "entryUUID";
	private static final String ObjectGUID = "objectGUID";
	private static final String CreateTimestamp = "createTimestamp";
	private static final String TelephoneNumber = "telephoneNumber";
	private static final String[] PERSON_ATTRIBUTES = { EntryUUID, ObjectGUID, UID, COMMON_NAME, SurName,
			CreateTimestamp, TelephoneNumber, MAIL, Password, MemberOf, OrganizationalUnit, GidNumber, UidNumber,
			Permission, LMCACL, ACI, FacsimileTelephoneNumber };

	public static LDAPLogin login(LoginProperties loginProperties) throws Exception {
		String ldapURI = LDAPHOST + loginProperties.getServerName() + ":" + loginProperties.getPort();
		String[] adminData = loginProperties.getAdminUser().split(":");
		String adminUser = adminData[0];
		String adminPassword = adminData[1];
		String companyName = loginProperties.getCompanyName();
		if (StringUtils.isBlank(companyName)) {
			companyName = loginProperties.getMasterCompany();
		} else {
			companyName = DOMAIN + "=" + companyName + "," + loginProperties.getMasterCompany();
		}
		String adminUserDN = COMMON_NAME + "=" + adminUser + "," + companyName;
		String filter = "(|(" + COMMON_NAME + "=" + loginProperties.getUserName() + ")(" + UID + "="
				+ loginProperties.getUserName() + "))";
		DirContext adminContext = loginUser(ldapURI, adminUserDN, adminPassword);
		if (!adminUser.equals(loginProperties.getUserName())) {
			String userDN = loginProperties.getUserIdAttribute() + "=" + loginProperties.getUserName() + ","
					+ OrganizationalUnit + "=" + People + "," + loginProperties.getMasterCompany();
			DirContext userContext = loginUser(ldapURI, userDN, loginProperties.getPassword());
			if (userContext != null) {
				LDAPLogin userObject = search(adminContext, companyName, filter);
				if (userObject != null) {
					return userObject;
				}
			}
		} else {
			LDAPLogin userObject = search(adminContext, companyName, filter);
			if (userObject != null) {
				return userObject;
			}
		}

		return null;

	}

	private static DirContext loginUser(String ldapURI, String dn, String password) throws Exception {
		Hashtable<String, String> env = new Hashtable<String, String>();
		env.put(Context.INITIAL_CONTEXT_FACTORY, contextFactory);
		env.put(Context.PROVIDER_URL, ldapURI);
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, dn);
		env.put(Context.SECURITY_CREDENTIALS, password);
		DirContext ctx = null;
		try {
			ctx = bind(env);
		} catch (javax.naming.AuthenticationException e) {
			return null;
		}
		return ctx;
	}

	private static DirContext bind(Hashtable<String, String> env) throws Exception {
		DirContext ctx = new InitialDirContext(env);
		return ctx;
	}

	private static LDAPLogin search(DirContext ctx, String companyName, String filter) throws Exception {
		LDAPLogin userObject = new LDAPLogin();
		SearchControls ctrl = new SearchControls();
		ctrl.setSearchScope(SearchControls.SUBTREE_SCOPE);
		ctrl.setReturningAttributes(PERSON_ATTRIBUTES);
		NamingEnumeration<SearchResult> answer = ctx.search(companyName, filter, ctrl);
		if (answer.hasMore()) {
			SearchResult result = (SearchResult) answer.next();
			userObject.setPermission(result.getAttributes().get(LMCACL).get().toString());
			userObject.setUserDN(result.getNameInNamespace());
		} else {
			userObject = null;
		}
		answer.close();
		return userObject;

	}

}
