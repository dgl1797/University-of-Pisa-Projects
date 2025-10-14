package com.unipi.bikeassistant.Sensors

import android.graphics.Color
import android.location.Location
import org.osmdroid.util.GeoPoint
import org.osmdroid.views.MapView
import org.osmdroid.views.overlay.Polyline

class DistanceCalculator(val mapView: MapView) {

    private var currentLocation: Location? = null
    private var previousLocation: Location? = null
    private var distance = 0.0
    private var previousPoint: GeoPoint? = null
    private var polyline: Polyline? = null
    private var polylineList: MutableList<Polyline?> = mutableListOf()
    private var previousSegmentColor: Int = Color.RED

    fun updateLocation(location: Location, DCIValue: Float) {
        currentLocation = location
        if (previousLocation == null) {
            previousLocation = location
            previousPoint = GeoPoint(location.latitude, location.longitude)

            // aggiungo il punto alla polyline
            polyline = Polyline(mapView)
            polyline?.addPoint(previousPoint)

            // aggiorno la polyline col percorso effettuato
            mapView.overlays.add(polyline)
        } else {
            // calcolo la distanza percorsa sulla strada dall'ultima posizione nota
            val distanceOnRoad = previousLocation?.distanceTo(currentLocation!!)

            // se l'utente si è mosso
            if (distanceOnRoad != null && distanceOnRoad > 0) {
                distance += distanceOnRoad.toDouble()
                val currentPoint = GeoPoint(location.latitude, location.longitude)

                val currentSegmentColor = when {
                    DCIValue < 0.25 -> Color.RED
                    DCIValue > 0.50 -> Color.GREEN
                    else -> Color.YELLOW
                }

                // aggiorno la polyline
                polyline?.addPoint(currentPoint)

                previousPoint = currentPoint
                previousLocation = location

                if (currentSegmentColor != previousSegmentColor) {
                    // Creo un nuovo segmento con il colore corrente
                    polyline = Polyline(mapView)
                    polyline?.addPoint(previousPoint)
                    polyline?.color = currentSegmentColor

                    // Aggiungo il nuovo segmento alla mappa
                    mapView.overlays.add(polyline)
                }
                previousSegmentColor = currentSegmentColor
                polylineList.add(polyline)
            }
        }
    }

    fun getCurrentDistance(): Double {
        return distance
    }

    fun getCurrentDistanceInString(): String{
        val distanceKm = distance/1000
        val formattedString = String.format("%.2f", distanceKm)
        return formattedString
    }

    fun removePolyline() {
        for(polyline in polylineList)
            mapView.overlays.remove(polyline)
    }
}
