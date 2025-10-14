package com.unipi.bikeassistant.services.geomaps

import com.unipi.bikeassistant.FirebaseController.GeoPointWithDCI

data class CityDoc(val center: GeoCoordinates, val avgDCI: Double, val nSamples: Int, val zones: List<ZoneDoc>)
data class ZoneDoc(val center: GeoCoordinates, val avgDCI: Double, val name:String, val nSamples: Int, val cells: List<CellDoc>)
data class CellDoc(val center: GeoCoordinates, val avgDCI: Double, val nSamples: Int, val cellID: String, val bbox: List<GeoCoordinates>)
data class GetCityDataResponse(val data: CityDoc)
data class PostRouteBody(val data: List<GeoPointWithDCI>)
data class GeoCoordinates(val latitude: Double, val longitude: Double)