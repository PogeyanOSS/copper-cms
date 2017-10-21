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
package com.pogeyan.cmis.ldap.factory;

import java.io.*;
import java.nio.file.*;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.function.Function;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pogeyan.cmis.ldap.model.LdapProperties;
import com.unboundid.ldap.sdk.LDAPConnection;
import com.unboundid.ldap.sdk.LDAPException;

public class Ldap {
	private static Logger logger = LoggerFactory.getLogger(Ldap.class);

	/**
	 * get TLS certificate.
	 * 
	 * @param host
	 *            the String
	 * @param port
	 *            the integer
	 * @param folderPath
	 *            the String
	 * @param javaKeystorePassword
	 *            the String
	 * @return TlsInfo, if successful
	 */
	public static TlsInfo getCerts(String host, int port, String folderPath,
			String javaKeystorePassword) throws Exception {
		char[] passphrase = "changeit".toCharArray();
		if (javaKeystorePassword != null && !javaKeystorePassword.isEmpty()) {
			passphrase = javaKeystorePassword.toCharArray();
		}

		TlsInfo tlsInfo = new TlsInfo();
		String filename = "jssecacerts";
		Path filePath = Paths.get(folderPath, filename);
		File file = filePath.toFile();
		if (file.isFile() == false) {
			char SEP = File.separatorChar;
			File dir = new File(System.getProperty("java.home") + SEP + "lib" + SEP + "security");
			file = new File(dir, "jssecacerts");
			if (file.isFile() == false) {
				file = new File(dir, "cacerts");
			}
		}
		logger.info("Loading KeyStore " + file + "...");
		InputStream in = new FileInputStream(file);
		KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
		ks.load(in, passphrase);
		in.close();

		SSLContext context = SSLContext.getInstance("TLS");
		TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory
				.getDefaultAlgorithm());
		tmf.init(ks);
		X509TrustManager defaultTrustManager = (X509TrustManager) tmf.getTrustManagers()[0];
		SavingTrustManager tm = new SavingTrustManager(defaultTrustManager);
		context.init(null, new TrustManager[] { tm }, null);
		SSLSocketFactory factory = context.getSocketFactory();

		logger.info("Opening connection to " + host + ":" + port + "...");
		SSLSocket socket = (SSLSocket) factory.createSocket(host, port);
		socket.setSoTimeout(10000);
		try {
			logger.info("Starting SSL handshake...");
			socket.startHandshake();
			socket.close();
			logger.info("No errors, certificate is already trusted");
			tlsInfo.alreadyCertified = true;
		} catch (SSLException e) {
			e.printStackTrace(System.out);
		}

		X509Certificate[] chain = tm.chain;
		if (chain == null) {
			logger.info("Could not obtain server certificate chain");
			tlsInfo.errorCode = "NO_SERVER_CERT_CHAIN";
			return tlsInfo;
		}

		logger.info("Server sent " + chain.length + " certificate(s):");
		MessageDigest sha1 = MessageDigest.getInstance("SHA1");
		MessageDigest md5 = MessageDigest.getInstance("MD5");
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < chain.length; i++) {
			X509Certificate cert = chain[i];
			sb.append((i + 1) + "&nbsp;&nbsp;Subject&nbsp;&nbsp;" + cert.getSubjectDN());
			sb.append(System.getProperty("line.separator"));
			sb.append("</br>");
			sb.append("&nbsp;&nbsp;&nbsp;&nbsp;Issuer&nbsp;&nbsp;" + cert.getIssuerDN());
			sb.append(System.getProperty("line.separator"));
			sb.append("</br>");
			sha1.update(cert.getEncoded());
			sb.append("&nbsp;&nbsp;&nbsp;&nbsp;sha1&nbsp;&nbsp;" + toHexString(sha1.digest()));
			sb.append(System.getProperty("line.separator"));
			sb.append("</br>");
			md5.update(cert.getEncoded());
			sb.append("&nbsp;&nbsp;&nbsp;&nbsp;md5&nbsp;&nbsp;" + toHexString(md5.digest()));
			sb.append(System.getProperty("line.separator"));
			sb.append("</br>");
		}

		tlsInfo.certDetails = sb.toString();
		logger.info("Cert Info: {}", tlsInfo.certDetails);

		if (!tlsInfo.alreadyCertified) {
			/**
			 * Last certificate is required to save for SSL. First certificate
			 * for TLS
			 */
			int k = 0;
			X509Certificate cert = chain[k];
			String alias = host + "-" + (k + 1);
			ks.setCertificateEntry(alias, cert);

			File outFile = filePath.toFile();
			OutputStream out = new FileOutputStream(outFile);
			ks.store(out, passphrase);
			out.close();
		}

		return tlsInfo;
	}

	private static final char[] HEXDIGITS = "0123456789abcdef".toCharArray();

	private static String toHexString(byte[] bytes) {
		StringBuilder sb = new StringBuilder(bytes.length * 3);
		for (int b : bytes) {
			b &= 0xff;
			sb.append(HEXDIGITS[b >> 4]);
			sb.append(HEXDIGITS[b & 15]);
			sb.append(' ');
		}
		return sb.toString();
	}

	/**
	 * Create and close connection
	 * 
	 * @param props
	 *            the LdapProperties
	 * @param fun
	 */
	public static <T> T query(LdapProperties props, Function<LDAPConnection, T> fn) {
		LDAPConnection conn = null;
		T tObject = null;
		try {
			conn = connectLDAP(props);
			tObject = fn.apply(conn);
		} catch (Exception e) {
			logger.error("LDAP transaction query error, {}", e.getMessage());
		} finally {
			if (conn != null) {
				conn.close();
			}
		}

		return tObject;
	}

	public static class TlsInfo {
		String certDetails = "";
		boolean alreadyCertified = false;
		String errorCode = "";

		public String getErrorCode() {
			return this.errorCode;
		}

		public String getCertDetails() {
			return this.certDetails;
		}

		public boolean isAlreadyCertified() {
			return this.alreadyCertified;
		}
	}

	private static class SavingTrustManager implements X509TrustManager {

		private final X509TrustManager tm;
		private X509Certificate[] chain;

		SavingTrustManager(X509TrustManager tm) {
			this.tm = tm;
		}

		public X509Certificate[] getAcceptedIssuers() {

			/**
			 * This change has been done due to the following resolution advised
			 * for Java 1.7+
			 * http://infposs.blogspot.kr/2013/06/installcert-and-java-7.html
			 **/

			return new X509Certificate[0];
			// throw new UnsupportedOperationException();
		}

		public void checkClientTrusted(X509Certificate[] chain, String authType)
				throws CertificateException {
			throw new UnsupportedOperationException();
		}

		public void checkServerTrusted(X509Certificate[] chain, String authType)
				throws CertificateException {
			this.chain = chain;
			tm.checkServerTrusted(chain, authType);
		}
	}
	


	/**
	 * Connect Open LDAP server.
	 * 
	 * @param ldapProperties
	 *            the LdapProperties class
	 * @return LDAPConnection, if successful
	 * @throws LDAPException 
	 */
	public static LDAPConnection connectLDAP(LdapProperties ldapProperties) throws LDAPException {
		LDAPConnection ldapConnection = null;
		try {
			LdapConnectionFactory connectionFactory = new LdapConnectionFactory(
					ldapProperties.getServerName(), ldapProperties.getPort(),
					ldapProperties.getUserName(), ldapProperties.getPassword());
			if (ldapProperties.isUseTLS()) {
				ldapConnection = connectionFactory.getLDAPConnection(Integer
						.parseInt(ldapProperties.getPortTLS()));
				logger.info("LDAP connected using TLS with status, {}",
						ldapConnection.isConnected());
			} else {
				ldapConnection = connectionFactory.getLDAPConnection();
				logger.info("LDAP connected with status, {}", ldapConnection.isConnected());
			}
		} catch (LDAPException e) {
			logger.error("LDAP connection failed, {} ", e.getMessage());
			throw new LDAPException(e);
		}
		return ldapConnection;
	}
}
