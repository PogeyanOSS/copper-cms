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
package com.pogeyan.cmis.data.objects;

import java.util.List;

import org.apache.chemistry.opencmis.commons.data.CmisExtensionElement;
import org.apache.chemistry.opencmis.commons.definitions.TypeMutability;
import org.mongodb.morphia.annotations.Embedded;

@Embedded
public class MTypeMutability implements TypeMutability {

	private Boolean canCreate;
	private Boolean canUpdate;
	private Boolean canDelete;

	public MTypeMutability() {

	}

	public MTypeMutability(Boolean canCreate, Boolean canUpdate, Boolean canDelete) {
		super();
		this.canCreate = canCreate;
		this.canUpdate = canUpdate;
		this.canDelete = canDelete;
	}

	@Override
	public List<CmisExtensionElement> getExtensions() {
		return null;
	}

	@Override
	public void setExtensions(List<CmisExtensionElement> extensions) {
	}

	@Override
	public Boolean canCreate() {
		return this.canCreate;
	}

	@Override
	public Boolean canUpdate() {
		return this.canUpdate;
	}

	@Override
	public Boolean canDelete() {
		return this.canDelete;
	}

	public void setCanCreate(Boolean canCreate) {
		this.canCreate = canCreate;
	}

	public void setCanUpdate(Boolean canUpdate) {
		this.canUpdate = canUpdate;
	}

	public void setCanDelete(Boolean canDelete) {
		this.canDelete = canDelete;
	}
}
