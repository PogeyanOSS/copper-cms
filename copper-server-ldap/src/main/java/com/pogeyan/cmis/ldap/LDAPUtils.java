/**
 * Copyright 2017 Pogeyan Technologies
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.pogeyan.cmis.ldap;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.api.messages.GroupPermissions;
import com.pogeyan.cmis.ldap.model.LDAPLogin;
import com.pogeyan.cmis.ldap.model.LdapProperties;
import com.pogeyan.cmis.ldap.model.LoginProperties;
import com.unboundid.ldap.sdk.BindResult;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPException;
import com.unboundid.ldap.sdk.ResultCode;
import com.unboundid.ldap.sdk.SearchRequest;
import com.unboundid.ldap.sdk.SearchResult;
import com.unboundid.ldap.sdk.SearchResultEntry;
import com.unboundid.ldap.sdk.SearchScope;

public class LDAPUtils {
	public static final String DOMAIN = "dc";
	public static final String DOMAIN_NAME = "dn";
	public static final String COMMON_NAME = "cn";
	public static final String SurName = "sn";
	public static final String OrganizationalUnit = "ou";
	public static final String Password = "userPassword";
	public static final String CHANGETYPE = "changetype";
	public static final String PHONENUMBER = "telephoneNumber";
	public static final String MAIL = "mail";
	public static final String OBJECT_CLASS = "ObjectClass";
	public static final String MEMBER = "uniqueMember";
	public static final String ADD = "add";
	public static final String MODIFY = "modify";
	public static final String People = "People";
	public static final String GROUPS = "Groups";
	public static final String OLCAccess = "olcAccess";
	public static final String UID = "uid";
	public static final String LMCACL = "lmcACL";
	public static final String Location = "l";
	public static final String FacsimileTelephoneNumber = "facsimileTelephoneNumber";
	public static final String GivenName = "givenName";
	public static final String GidNumber = "gidNumber";
	public static final String UidNumber = "uidNumber";
	public static final String Description = "description";
	public static final String HomeDirectory = "homeDirectory";
	public static final String RoomNumber = "roomNumber";
	public static final String MemberOf = "memberOf";
	public static final String ACI = "aci";
	public static final String Permission = "permission";
	public static final String LDAP_CONFIG_USER = "cn=admin,cn=config";
	public static final String LDAP_CONFIG_PASSWORD = "c0nfig";
	public static final String EntryUUID = "entryUUID";
	public static final String ObjectGUID = "objectGUID";
	public static final String CreateTimestamp = "createTimestamp";
	public static final String TelephoneNumber = "telephoneNumber";
	public static final String Member = "uniqueMember";
	public static final String ShadowExpire = "shadowExpire";
	/** The Constant OBJECT_SCOPE. */
	public static final SearchScope OBJECT_SCOPE = SearchScope.SUB;
	/** The Constant PERSON_ATTRIBUTES. */
	public static final String[] PERSON_ATTRIBUTES = { EntryUUID, ObjectGUID, UID, COMMON_NAME, SurName,
			CreateTimestamp, TelephoneNumber, MAIL, Password, MemberOf, OrganizationalUnit, GidNumber, UidNumber,
			Permission, LMCACL, ACI, FacsimileTelephoneNumber };

	private static Logger logger = LoggerFactory.getLogger(LDAPUtils.class);

	/**
	 * Search for LDAP user based on the @link LdapProperties ldapProperties.
	 * 
	 * @param ldapConnection
	 *            the LdapConnection class
	 * @param ldapProperties
	 *            the LdapProperties class, can be null if not null values for
	 *            companyName and filter
	 * @param companyName
	 *            the baseDN from LdapProperties, if not passing
	 * @param filter
	 *            the String filter
	 * @return List<SearchResultEntry>, if successful
	 */
	public static List<SearchResultEntry> search(LDAPConnection ldapConnection, LdapProperties ldapProperties,
			String companyName, String filter) {
		if (ldapConnection != null && ldapConnection.isConnected()) {
			try {
				if (ldapProperties != null) {
					filter = filter != null ? filter : ldapProperties.getSearchFilter();
					companyName = StringUtils.isNoneBlank(companyName)
							? DOMAIN + "=" + companyName + "," + ldapProperties.getBaseDN()
							: ldapProperties.getBaseDN();
				}
				SearchRequest searchRequest = new SearchRequest(companyName, OBJECT_SCOPE, filter, PERSON_ATTRIBUTES);
				SearchResult searchResult = ldapConnection.search(searchRequest);
				List<SearchResultEntry> list = searchResult.getSearchEntries();
				list.stream().collect(Collectors.toList()).forEach(element -> logger.info(element.getDN()));
				return list;
			} catch (LDAPException e) {
				logger.error("LDAP search failed, {} ", e.getMessage());
			}
		}

		return null;
	}

	/**
	 * Login for LDAP user based on the @link LoginProperties LoginProperties.
	 * 
	 * @param loginProperties
	 *            the LoginProperties
	 * @return LDAPLogin, if successful ldap connection
	 */
	@SuppressWarnings("resource")
	public static LDAPLogin login(LoginProperties loginProperties) throws LDAPException {
		try {
			LDAPLogin login = new LDAPLogin();
			LDAPConnection ldapConnection = new LDAPConnection(loginProperties.getServerName(),
					loginProperties.getPort());
			if (ldapConnection != null && ldapConnection.isConnected()) {
				String companyName = loginProperties.getCompanyName();
				String[] adminData = loginProperties.getAdminUser().split(":");
				String adminUser = adminData[0];
				String adminPassword = adminData[1];
				if (StringUtils.isBlank(companyName)) {
					companyName = loginProperties.getMasterCompany();
				} else {
					companyName = DOMAIN + "=" + companyName + "," + loginProperties.getMasterCompany();
				}

				String filter = "(|(" + COMMON_NAME + "=" + loginProperties.getUserName() + ")(" + UID + "="
						+ loginProperties.getUserName() + "))";
				String userName = COMMON_NAME + "=" + adminUser + "," + companyName;
				BindResult bindResult = ldapConnection.bind(userName, adminPassword);
				if (bindResult.getResultCode().intValue() == 0) {
					List<SearchResultEntry> list = search(ldapConnection, null, companyName, filter);
					if (list != null && list.size() > 0) {
						Optional<SearchResultEntry> search = list.stream().findFirst();
						if (search.isPresent())
							userName = search.get().getDN();
					} else
						userName = COMMON_NAME + "=" + loginProperties.getUserName() + ","
								+ loginProperties.getMasterCompany();
					bindResult = ldapConnection.bind(userName, loginProperties.getPassword());
					if (bindResult.getResultCode().intValue() == 0) {
						String[] attributes = { MemberOf, Permission, LMCACL, ACI, ShadowExpire };
						SearchResultEntry entry = ldapConnection.getEntry(userName, attributes);
						login.setUserDN(entry.getDN());
						if (entry.getAttribute(ShadowExpire) != null) {
							String expired = entry.getAttribute(ShadowExpire).getValue();
							if (Integer.parseInt(expired) == 1) {
								String error = "Login Failed: " + loginProperties.getUserName()
										+ " Got deleted already";
								logger.error(error);
								throw new LDAPException(ResultCode.INAPPROPRIATE_AUTHENTICATION);
							}
						}
						if (entry.getAttribute(Permission) != null)
							login.setPermission(entry.getAttribute(Permission).getValue());
						else if (entry.getAttribute(LMCACL) != null)
							login.setPermission(entry.getAttribute(LMCACL).getValue());
						else if (entry.getAttribute(ACI) != null)
							login.setPermission(entry.getAttribute(ACI).getValue());

						if (entry.getAttribute(MemberOf) != null) {
							List<GroupPermissions> groupPermissions = new ArrayList<GroupPermissions>();
							List<String> groups = Arrays.asList(entry.getAttribute(MemberOf).getValues());
							for (String groupDN : groups) {
								GroupPermissions groupPermission = new GroupPermissions();
								groupPermission.setGroupDN(groupDN);
								String[] attributeACL = { LMCACL };

								entry = ldapConnection.getEntry(groupDN, attributeACL);
								if (entry != null && entry.getAttribute(LMCACL) != null) {
									String gPermission = entry.getAttribute(LMCACL).getValue();
									groupPermission.setPermission(gPermission);
								}
								groupPermissions.add(groupPermission);
							}
							login.setGroups(groupPermissions);
						}
						ldapConnection.close();
						return login;
					} else {
						ldapConnection.close();
						return null;
					}
				} else {
					ldapConnection.close();
					logger.error("Login Failed for {}", loginProperties.getUserName());
					return null;
				}
			}
		} catch (LDAPException e) {
			logger.error("Login Failed: {}", e.getResultString());
			throw new LDAPException(e);
		}
		return null;
	}
}