package com.pogeyan.cmis.data.jdo;

import java.util.List;

import javax.jdo.annotations.Cacheable;
import javax.jdo.annotations.DatastoreIdentity;
import javax.jdo.annotations.IdGeneratorStrategy;
import javax.jdo.annotations.PersistenceCapable;
import javax.jdo.annotations.Serialized;

@Cacheable("false")
@PersistenceCapable(detachable = "true")
@DatastoreIdentity(strategy = IdGeneratorStrategy.IDENTITY)
public class JChoiceImpl {

	private String dispalyName;
	@Serialized
	private List<Object> value;

	public JChoiceImpl() {

	}

	public JChoiceImpl(String dispalyName, List<Object> value) {
		this.dispalyName = dispalyName;
		this.setValue(value);
	}

	public String getDispalyName() {
		return dispalyName;
	}

	public void setDispalyName(String dispalyName) {
		this.dispalyName = dispalyName;
	}

	public List<Object> getValue() {
		return value;
	}

	public void setValue(List<Object> value) {
		this.value = value;
	}

}
