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
package com.pogeyan.cmis.ldap.model;

import java.io.File;

import org.apache.chemistry.opencmis.commons.BasicPermissions;

public class UsersProperties {
	/* This will be the user name (cn) */
	private String userName;
	/* This will be the family name (sn) */
	private String surName;
	/* This will be the given name (givenName) */
	private String givenName;
	/* This will be the email address (email) */
	private String email;
	/* This will be the telephone number (telephoneNumber) */
	private String phone;
	/* This will be the company name user belongs to */
	private String companyName;
	/*
	 * This will be the organization unit name user belongs to other than People
	 * (ou)
	 */
	private String orgUnit;
	/* This will be the photo of the user (jpegPhoto) */
	private File photo;
	/* This will be the user id (uid) */
	private String userId;
	/* This will be the fax number (facsimileTelephoneNumber) */
	private String facsimileTelephoneNumber;
	/* This will be the room number (roomNumber) */
	private String roomNumber;
	/* This will be the locality of user (locality) */
	private String address;
	/* This will be the unique number (gidNumber) */
	private long gidNumber;
	/*
	 * This will be the the unique number for the group user belongs to
	 * (uidNumber)
	 */
	private long uidNumber;
	/* This will provide repository level permission for the user*/
	private BasicPermissions permission;

	public String getCompanyName() {
		return companyName;
	}

	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String i) {
		this.phone = i;
	}

	public File getPhoto() {
		return photo;
	}

	public void setPhoto(File photo) {
		this.photo = photo;
	}

	public String getOrgUnit() {
		return orgUnit;
	}

	public void setOrgUnit(String orgUnit) {
		this.orgUnit = orgUnit;
	}

	/**
	 * @return the surName
	 */
	public String getSurName() {
		return surName;
	}

	/**
	 * @param surName
	 *            the surName to set
	 */
	public void setSurName(String surName) {
		this.surName = surName;
	}

	/**
	 * @return the givenName
	 */
	public String getGivenName() {
		return givenName;
	}

	/**
	 * @param givenName
	 *            the givenName to set
	 */
	public void setGivenName(String givenName) {
		this.givenName = givenName;
	}

	/**
	 * @return the userId
	 */
	public String getUserId() {
		return userId;
	}

	/**
	 * @param userId
	 *            the userId to set
	 */
	public void setUserId(String userId) {
		this.userId = userId;
	}

	/**
	 * @return the facsimileTelephoneNumber
	 */
	public String getFacsimileTelephoneNumber() {
		return facsimileTelephoneNumber;
	}

	/**
	 * @param facsimileTelephoneNumber
	 *            the facsimileTelephoneNumber to set
	 */
	public void setFacsimileTelephoneNumber(String facsimileTelephoneNumber) {
		this.facsimileTelephoneNumber = facsimileTelephoneNumber;
	}

	/**
	 * @return the roomNumber
	 */
	public String getRoomNumber() {
		return roomNumber;
	}

	/**
	 * @param roomNumber
	 *            the roomNumber to set
	 */
	public void setRoomNumber(String roomNumber) {
		this.roomNumber = roomNumber;
	}

	/**
	 * @return the address
	 */
	public String getAddress() {
		return address;
	}

	/**
	 * @param address
	 *            the address to set
	 */
	public void setAddress(String address) {
		this.address = address;
	}

	/**
	 * @return the gidNumber
	 */
	public long getGidNumber() {
		return gidNumber;
	}

	/**
	 * @param gidNumber
	 *            the gidNumber to set
	 */
	public void setGidNumber(long gidNumber) {
		this.gidNumber = gidNumber;
	}

	/**
	 * @return the uidNumber
	 */
	public long getUidNumber() {
		return uidNumber;
	}

	/**
	 * @param uidNumber
	 *            the uidNumber to set
	 */
	public void setUidNumber(long uidNumber) {
		this.uidNumber = uidNumber;
	}

	/**
	 * @return the permission
	 */
	public BasicPermissions getPermission() {
		return permission;
	}

	/**
	 * @param permission the permission to set
	 */
	public void setPermission(BasicPermissions permission) {
		this.permission = permission;
	}
}
