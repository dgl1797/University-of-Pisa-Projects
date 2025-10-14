package com.unipi.bikeassistant.services.osm

data class Address(val town: String?, val suburb: String?, val village: String?, val city: String?)

data class NominatimResponse(val lat: Double, val lon: Double, val address: Address)