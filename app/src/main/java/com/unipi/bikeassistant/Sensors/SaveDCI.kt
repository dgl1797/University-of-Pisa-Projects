package com.unipi.bikeassistant.Sensors
import org.osmdroid.util.GeoPoint

class SaveDCI {
    private val geoPoints: MutableList<MutableList<GeoPoint>> = mutableListOf()

    fun addGeoPoints(points: Array<GeoPoint>) {
        geoPoints.add(points.toMutableList())
    }

    fun removeGeoPoints(index: Int) {
        if (index in 0 until geoPoints.size) {
            geoPoints.removeAt(index)
        } else {
            throw IndexOutOfBoundsException("Invalid index")
        }
    }

    fun getGeoPoints(index: Int): Array<GeoPoint> {
        if (index in 0 until geoPoints.size) {
            return geoPoints[index].toTypedArray()
        } else {
            throw IndexOutOfBoundsException("Invalid index")
        }
    }

    fun getAllGeoPoints(): List<Array<GeoPoint>> {
        return geoPoints.map { it.toTypedArray() }
    }
}
