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
package com.pogeyan.cmis.api.data.services;

import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.definitions.PropertyDefinition;
import com.pogeyan.cmis.data.objects.MTypeObject;

public interface MTypeManagerDAO {
	/**
	 * Returns MTypeObject values depending on TypeId
	 */
	public List<MTypeObject> getById(List<?> typeId);

	/**
	 * Remove MTypeObject values depending on TypeId
	 */
	public void delete(String typeId);

	/**
	 * Returns list of MTypeObject children depending on TypeId
	 */
	public List<MTypeObject> getChildrenIds(String parentId, int maxItems, int skipCount);

	/**
	 * Returns propertyDef depending on propId
	 */
	public Map<String, PropertyDefinition<?>> getAllPropertyById(String propId);

	public void commit(MTypeObject entity);

}
