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
package com.pogeyan.cmis.repo;

import org.mongodb.morphia.annotations.Embedded;

@Embedded
public class MRepositoryFile {
	private String type;
	private String location;
	private String bucket;
	private String accessKeyId;
	private String secretAccessKey;
	private String region;
	private String kms_region;
	private String kms_id;
	private String encryption;

	public MRepositoryFile() {

	}

	public MRepositoryFile(String type, String location, String bucket, String accessKeyId, String secretAccessKey,
			String region, String kms_region, String kms_id, String encryption) {
		super();
		this.type = type;
		this.location = location;
		this.bucket = bucket;
		this.accessKeyId = accessKeyId;
		this.secretAccessKey = secretAccessKey;
		this.region = region;
		this.kms_region = kms_region;
		this.kms_id = kms_id;
		this.encryption = encryption;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public String getBucket() {
		return bucket;
	}

	public void setBucket(String bucket) {
		this.bucket = bucket;
	}

	public String getAccessKeyId() {
		return accessKeyId;
	}

	public void setAccessKeyId(String accessKeyId) {
		this.accessKeyId = accessKeyId;
	}

	public String getSecretAccessKey() {
		return secretAccessKey;
	}

	public void setSecretAccessKey(String secretAccessKey) {
		this.secretAccessKey = secretAccessKey;
	}

	public String getRegion() {
		return region;
	}

	public void setRegion(String region) {
		this.region = region;
	}

	public String getKms_region() {
		return kms_region;
	}

	public void setKms_region(String kms_region) {
		this.kms_region = kms_region;
	}

	public String getEncryption() {
		return encryption;
	}

	public void setEncryption(String encryption) {
		this.encryption = encryption;
	}

	public String getKms_id() {
		return kms_id;
	}

	public void setKms_id(String kms_id) {
		this.kms_id = kms_id;
	}

}
