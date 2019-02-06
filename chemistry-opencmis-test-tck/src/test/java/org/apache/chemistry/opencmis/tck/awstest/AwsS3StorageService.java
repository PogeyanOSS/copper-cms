/**
 * Copyright (C) Pogeyan Technologies Private Limited - All Rights Reserved
 * Unauthorized copying of this file, via any medium is strictly prohibited
 * Proprietary and confidential.
 */
package org.apache.chemistry.opencmis.tck.awstest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.DeleteObjectRequest;
import com.amazonaws.services.s3.model.GetObjectMetadataRequest;

public class AwsS3StorageService {
	private static final Logger LOG = LoggerFactory.getLogger(AwsS3StorageService.class);
	private AmazonS3 s3client;

	public AwsS3StorageService() {
	}

	public AmazonS3 setClient(String accessKey, String secretAccessKey, String region) {

		AWSCredentials credentials = new BasicAWSCredentials(accessKey, secretAccessKey);
		this.s3client = AmazonS3ClientBuilder.standard().withCredentials(new AWSStaticCredentialsProvider(credentials))
				.withRegion(region).build();
		return s3client;

	}

	public boolean folderExists(String folderName, String bucketName) {
		LOG.info("checking folder present: {}", folderName);
		try {
			this.s3client.getObjectMetadata(new GetObjectMetadataRequest(bucketName, folderName));
			return true;
		} catch (Exception e) {
			LOG.warn("folderExists folder not persent for name: {} in bucket: {}", folderName, bucketName);
		}
		return false;
	}

	public void deleteFolder(String folderName, String bucketName) {
		LOG.info("deleteFolder for folderName: {}", folderName);
		DeleteObjectRequest deleteObject = new DeleteObjectRequest(bucketName, folderName);
		this.s3client.deleteObject(deleteObject);
	}
}
