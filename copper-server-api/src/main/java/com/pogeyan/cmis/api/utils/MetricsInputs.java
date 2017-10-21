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

import com.codahale.metrics.Counter;
import com.codahale.metrics.Meter;
import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.Timer;

import io.prometheus.client.CollectorRegistry;

public class MetricsInputs {

	private static MetricRegistry metrics = new MetricRegistry();
	final static MetricsInputs instance = new MetricsInputs();

	public static MetricsInputs get() {
		return instance;
	}

	public static CollectorRegistry collectorRegistry() {
		return CollectorRegistry.defaultRegistry;
	}

	public MetricRegistry getMetrics() {
		return metrics;
	}

	public Meter getMarker(String name) {
		Meter requests = getMetrics().meter(name);
		return requests;
	}

	public Timer getTimer(String name) {
		Timer time = getMetrics().timer(name);
		return time;
	}

	public Counter getCounter(String name) {
		Counter count = getMetrics().counter(name);
		return count;
	}

	public static void markUploadErrorMeter() {
		if (Helpers.isPerfMode()) {
			MetricsInputs.get().getMarker("meter_upload_error").mark();
		}
	}

	public static void markDownloadErrorMeter() {
		if (Helpers.isPerfMode()) {
			MetricsInputs.get().getMarker("meter_download_error").mark();
		}
	}

	public static void markBindingServletErrorMeter() {
		if (Helpers.isPerfMode()) {
			MetricsInputs.get().getMarker("meter_bindingServlet_error").mark();
		}
	}
}
