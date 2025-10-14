package com.unipi.bikeassistant.Sensors

import com.unipi.bikeassistant.FirebaseController.GeoPointWithDCI
import org.osmdroid.util.GeoPoint

class StreetSegment {
    private val geoPointsWithDCI: MutableList<GeoPointWithDCI> = mutableListOf()

    fun addGeoPointWithDCI(point: GeoPoint, dci: Double) {
        val geoPointWithDCI = GeoPointWithDCI(point, dci)
        geoPointsWithDCI.add(geoPointWithDCI)
    }

    fun getAllGeoPointsWithDCI(): List<GeoPointWithDCI> {
        return geoPointsWithDCI
    }

    fun getPointWithDCI(i: Int): GeoPointWithDCI {
        return geoPointsWithDCI[i]
    }

    fun getDimension(): Int {
        return geoPointsWithDCI.size
    }
}
