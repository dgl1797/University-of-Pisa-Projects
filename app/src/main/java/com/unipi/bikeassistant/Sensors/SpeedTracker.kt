package com.unipi.bikeassistant.Sensors

import android.graphics.Color
import android.graphics.Paint
import android.location.Location
import org.osmdroid.util.GeoPoint
import org.osmdroid.views.MapView
import org.osmdroid.views.overlay.Polyline
import kotlin.math.roundToInt

class SpeedTracker(openStreetMap: MapView) {
        private var startTime: Long? = null
        private var previousTime: Long? = null
        private var previousDistance: Double = 0.0
        private var currentSpeed: Double = 0.0
        private var distanza: Float = 0f
        private var currentAverageSpeed: Double = 0.0

        fun getCurrentSpeed(): Double {
            return currentSpeed
        }

        fun getCurrentSpeedInString(): String{
            var currentSpeedKmH = currentSpeed*3.6
            var formattedString = String.format("%.2f", currentSpeedKmH)
            return formattedString
        }

        fun getCurrentAverageSpeed(): Double {
            return currentAverageSpeed
        }

        fun getCurrentAverageSpeedInString(): String{
            var currentAverageSpeedKmH = currentAverageSpeed*3.6
            var formattedString = String.format("%.2f", currentAverageSpeedKmH)
            return formattedString
        }
        fun updateLocation(location: Location, distanza: Float) {

            if (startTime == null) {
                startTime = System.currentTimeMillis()
                previousTime = startTime
            } else {
                // calcolo il tempo trascorso
                val currentTime = System.currentTimeMillis()
                val timeDelta = (currentTime - previousTime!!) / 1000.0 // converti in secondi

                // calcolo la distanza percorsa
                val distanceDelta = distanza - previousDistance

                // La velocità è data da spazio/tempo
                currentSpeed = distanceDelta / timeDelta

                // per la velocità media calcolo: distanza totale percorsa dall'inizio dell'attività fino al momento attuale diviso
                // quanto tempo è passato da quando ho iniziato il percorso
                currentAverageSpeed = distanza / ((currentTime - startTime!!) / 1000.0)

                previousTime = currentTime
                previousDistance = distanza.toDouble()
            }
        }
}