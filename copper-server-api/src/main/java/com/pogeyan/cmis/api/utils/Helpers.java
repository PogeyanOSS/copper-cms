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
package com.pogeyan.cmis.api.utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Stream;
import java.util.stream.Collectors;

import org.apache.chemistry.opencmis.commons.BasicPermissions;
import org.apache.commons.lang3.StringUtils;

import com.pogeyan.cmis.api.auth.IUserGroupObject;
import com.pogeyan.cmis.api.auth.IUserObject;

import akka.actor.ActorRef;
import akka.pattern.Patterns;
import akka.util.Timeout;
import scala.concurrent.Await;
import scala.concurrent.Future;

/**
 * Helper utility functions.
 */
public class Helpers {
	static long LOWER_RANGE = 0; // assign lower range value
	static long UPPER_RANGE = 1000000; // assign upper range value

	static Random random = new Random();

	/**
	 * Gets the utc time.
	 *
	 * @return the utc time
	 */
	public static Date getUtcTime() {
		Calendar c2 = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		return c2.getTime();
	}

	/**
	 * Gets the DB utc time.
	 *
	 * @return the DB utc time
	 */
	public static java.sql.Timestamp getDBUtcTime() {
		return new java.sql.Timestamp(Helpers.getUtcTime().getTime());
	}

	/**
	 * Gets the random value.
	 *
	 * @return the long
	 */
	public static long rand() {
		long randomValue = LOWER_RANGE + (long) (random.nextDouble() * (UPPER_RANGE - LOWER_RANGE));
		return randomValue;
	}

	/**
	 * Try parse double.
	 *
	 * @param number
	 *            the number
	 * @return the double
	 */
	public static double tryParseDouble(final String number) {
		double result;
		try {
			result = Double.parseDouble(number);
		} catch (NumberFormatException e) {
			result = 0.0;
		}
		return result;
	}

	/**
	 * Await actor for the prescribed time and return the value.
	 *
	 * @param <T>
	 *            the generic type
	 * @param actorRef
	 *            the actor ref
	 * @param message
	 *            the message
	 * @param timeout
	 *            the timeout
	 * @return the object result from the actor response
	 */
	@SuppressWarnings("unchecked")
	public static <T> T awaitActor(final ActorRef actorRef, final Object message, final Timeout timeout) {
		Future<Object> future = Patterns.ask(actorRef, message, timeout);
		try {
			T bResp = (T) Await.result(future, timeout.duration());
			return bResp;
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Converts byte array to properties.
	 * 
	 * @param bytes
	 *            The byte array which shall be converted.
	 * 
	 * @return The converted properties.
	 * 
	 * @throws IOException
	 */
	public static Properties bytes2Properties(byte[] bytes) throws IOException {
		Properties properties = new Properties();
		ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(bytes);
		properties.load(byteArrayInputStream);
		return properties;
	}

	/**
	 * Converts properties to byte array.
	 * 
	 * @param properties
	 *            The properties which shall be converted.
	 * 
	 * @return The converted byte array.
	 * 
	 * @throws IOException
	 */
	public static byte[] properties2Bytes(Properties properties) throws IOException {
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		properties.store(byteArrayOutputStream, "");
		return byteArrayOutputStream.toByteArray();
	}

	/*public static boolean isInDevMode() {
		final String value = System.getenv("MODE");
		if (value != null) {
			return value.equalsIgnoreCase("1");
		}
	
		return false;
	}*/

	public static boolean isPerfMode() {
		final String value = System.getenv("PERF");
		if (value != null) {
			return value.equalsIgnoreCase("1");
		}

		return false;
	}

	public static boolean isInfluxDBReport() {
		final String value = System.getenv("INFLUXDB");
		if (value != null) {
			return value.equalsIgnoreCase("1");
		}

		return false;
	}

	public static boolean isTestMode() {
		final String value = System.getenv("TEST_MODE");
		if (value != null) {
			return value.equalsIgnoreCase("1");
		}

		return false;
	}

	public static boolean isPrometheusMode() {
		final String value = System.getenv("PROMETHEUS_MODE");
		if (value != null) {
			return value.equalsIgnoreCase("1");
		}

		return false;
	}

	public static String getPushgatewayAddress() {
		return System.getenv("PROM_ADDR");
	}

	public static boolean getGroupPermission(String permission, IUserGroupObject[] groups) {
		boolean allowed = false;
		if (groups != null) {
			allowed = Arrays.stream(groups).filter(t -> t.getPermission().equals(BasicPermissions.WRITE)
					|| t.getPermission().equals(BasicPermissions.ALL)).count() > 0;
		}
		if (!allowed) {
			allowed = checkingUserPremission(permission, "post");
		}
		return allowed;
	}

	public static boolean checkingUserPremission(String permission, String request) {
		if (request.equals("get")) {
			if (StringUtils.isNotBlank(permission) && (permission.equals(BasicPermissions.READ)
					|| permission.equals(BasicPermissions.WRITE) || permission.equals(BasicPermissions.ALL))) {
				return true;
			}
			return false;
		} else {
			if (StringUtils.isNotBlank(permission)
					&& (permission.equals(BasicPermissions.WRITE) || permission.equals(BasicPermissions.ALL))) {
				return true;
			}
			return false;
		}

	}

	public static String[] getPrincipalIds(IUserObject userObject) {
		List<String> principalIds = Stream.of(userObject.getGroups()).map(t -> t.getGroupDN())
				.collect(Collectors.<String>toList());
		principalIds.add(userObject.getUserDN());
		return principalIds.toArray(new String[principalIds.size()]);
	}

	public static String getQueryName(String name) {
		if (name.equalsIgnoreCase("cmis:path") || name.equalsIgnoreCase("cmis:description")
				|| name.equalsIgnoreCase("cmis:parentId") || name.equalsIgnoreCase("cmis:contentStreamLength")
				|| name.equalsIgnoreCase("cmis:contentStreamFileName")
				|| name.equalsIgnoreCase("cmis:contentStreamMimeType") || name.equalsIgnoreCase("cmis:checkinComment")
				|| name.equalsIgnoreCase("cmis:versionLabel") || name.equalsIgnoreCase("cmis:isMajorVersion")
				|| name.equalsIgnoreCase("cmis:isLatestVersion") || name.equalsIgnoreCase("cmis:isLatestMajorVersion")
				|| name.equalsIgnoreCase("cmis:name") || name.equalsIgnoreCase("cmis:isPrivateWorkingCopy")
				|| name.equalsIgnoreCase("cmis:createdBy") || name.equalsIgnoreCase("cmis:contentStreamId")
				|| name.equalsIgnoreCase("cmis:versionSeriesCheckedOutId")
				|| name.equalsIgnoreCase("cmis:versionSeriesId")
				|| name.equalsIgnoreCase("cmis:isVersionSeriesCheckedOut") || name.equalsIgnoreCase("cmis:isImmutable")
				|| name.equalsIgnoreCase("cmis:modifiedBy")
				|| name.equalsIgnoreCase("cmis:versionSeriesCheckedOutBy")) {
			return getFieldName(name);
		} else if (name.equalsIgnoreCase("cmis:objectId")) {
			return "id";
		} else if (name.equalsIgnoreCase("cmis:secondaryObjectTypeIds")) {
			return "secondaryTypeIds";
		} else if (name.equalsIgnoreCase("cmis:objectTypeId")) {
			return "typeId";
		} else if (name.equalsIgnoreCase("cmis:lastModifiedBy")) {
			return "modifiedBy";
		} else if (name.equalsIgnoreCase("cmis:creationDate")) {
			return "createdAt";
		} else if (name.equalsIgnoreCase("cmis:changeToken")) {
			return "token";
		} else if (name.equalsIgnoreCase("cmis:lastModificationDate")) {
			return "modifiedAt";
		} else if (name.equalsIgnoreCase("cmis:baseTypeId")) {
			return "baseId";
		} else if (name.equalsIgnoreCase("id")) {
			return "id";
		} else if (name.equalsIgnoreCase("operator")) {
			return "operator";
		} else {
			return "properties." + name;
		}
	}

	private static String getFieldName(Object value) {
		String valueString = value.toString();
		String[] values = valueString.split(":");
		String stringValue = values[1].replaceAll("[-+.^:',{}]", "");
		return stringValue;
	}

	public static List<String> getDocumentProperties() {
		List<String> documentProperty = new ArrayList<String>();
		documentProperty.add("contentStreamLength");
		documentProperty.add("isImmutable");
		documentProperty.add("isLatestVersion");
		documentProperty.add("isMajorVersion");
		documentProperty.add("isLatestMajorVersion");
		documentProperty.add("isPrivateWorkingCopy");
		documentProperty.add("versionLabel");
		documentProperty.add("versionSeriesId");
		documentProperty.add("versionReferenceId");
		documentProperty.add("isVersionSeriesCheckedOut");
		documentProperty.add("versionSeriesCheckedOutBy");
		documentProperty.add("versionSeriesCheckedOutId");
		documentProperty.add("checkinComment");
		documentProperty.add("contentStreamLength");
		documentProperty.add("contentStreamMimeType");
		documentProperty.add("contentStreamFileName");
		documentProperty.add("contentStreamId");
		documentProperty.add("previousVersionObjectId");
		return documentProperty;
	}

	public static String[] getFilterArray(Set<String> filterCollection, boolean check) {
		List<String> filterArray = filterCollection.stream().map(t -> getQueryName(t))
				.collect(Collectors.<String>toList());
		filterArray.add("acl");
		if (check) {
			List<String> docProps = getDocumentProperties();
			List<String> matchProps = new ArrayList<String>();
			for (String props : filterArray) {
				if (docProps.contains(props)) {
					matchProps.add(props);
				}
			}
			if (matchProps != null && matchProps.size() > 0) {
				filterArray.removeAll(matchProps);
			}
		}
		return filterArray.toArray(new String[filterArray.size()]);
	}
	
	public static String getObjectId() {
		return java.util.UUID.randomUUID().toString().replace("-", "");
	}
}
