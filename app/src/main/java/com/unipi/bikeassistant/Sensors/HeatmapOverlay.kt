package com.unipi.bikeassistant.Sensors

import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.graphics.Point
import android.graphics.PointF
import android.graphics.Rect
import android.view.MotionEvent
import org.osmdroid.util.GeoPoint
import org.osmdroid.views.MapView
import org.osmdroid.views.Projection
import org.osmdroid.views.overlay.Overlay

class HeatmapOverlay(
    private val mapView: MapView,
    private val geoPoint: GeoPoint, // la coordinata in cui si trova attualmente l'utente
    private val radius: Double,
    private val dci: Double
) : Overlay() {

    private val paint = Paint(Paint.ANTI_ALIAS_FLAG)

    val heatmapColors = listOf(
        Color.rgb(255, 0, 0),    // Rosso
        Color.rgb(255, 255, 0),  // Giallo
        Color.rgb(0, 255, 0)     // Verde
    )

    init {
        paint.style = Paint.Style.FILL
        paint.strokeWidth = 8f
    }

    override fun draw(canvas: Canvas, projection: Projection) {
        super.draw(canvas, mapView.projection)

        val centerPoint = Point()
        projection.toPixels(geoPoint, centerPoint)

        val color = getColorForDCI(dci)
        paint.color = color

        val radiusPixels = projection.metersToPixels(radius.toFloat())

        canvas.drawCircle(centerPoint.x.toFloat(), centerPoint.y.toFloat(), radiusPixels, paint)
    }

    private fun getColorForDCI(dci:Double): Int {
        val index = when {
            (dci < 0.25) -> 0
            (dci > 0.25 && dci < 0.5) -> 1
            else -> 2
        }

        return heatmapColors[index]
    }
}

/*
class HeatmapOverlay(
    private val segment: StreetSegment,
    private val Dci: Double
) : Overlay() {

    // creo una lista coi colori possibili da assegnare alla polyline
    private val heatmapColors: List<Int> = listOf(
        Color.rgb(255, 0, 0),    // Rosso
        Color.rgb(255, 255, 0),  // Giallo
        Color.rgb(0, 255, 0)     // Verde
    )

    // Il Canvas servirà per disegnare elementi grafici come linee, forme, testo, etc.
    override fun draw(canvas: Canvas, mapView: MapView, shadow: Boolean) {
        super.draw(canvas, mapView, shadow)

        // creo un oggetto Path, che rappresenta un percorso disegnato su un Canvas
        val path = Path()

        // L'oggetto Projection converte le coordinate geografiche dei GeoPoint in
        // coordinate del pixel sul Canvas
        val projection = mapView.projection

        // per ogni coppia di punti consecutivi, calcolo le coordinate dei pixel di inizio e fine
        for (i in 0 until segment.getDimension() - 1) {
            val startPoint = projection.toPixels(segment.getPoint(i), null)
            val endPoint = projection.toPixels(segment.getPoint(i + 1), null)

            // percorso che connette i punti del percorso sulla mappa
            path.moveTo(startPoint.x.toFloat(), startPoint.y.toFloat())
            path.lineTo(endPoint.x.toFloat(), endPoint.y.toFloat())
        }

        val color = getColorForDCI(Dci)

        // dopo aver preparato il percorso, adesso preparo il colore
        val paint = Paint(Paint.ANTI_ALIAS_FLAG)
        paint.color = color
        paint.style = Paint.Style.STROKE
        paint.strokeWidth = 8f

        // passo al canvas sia il colore che il percorso
        canvas.drawPath(path, paint)
    }

    private fun getColorForDCI(dci: Double): Int {
        val index = when {
            (dci < 0.25) -> 0
            (dci > 0.25 && dci < 0.5) -> 1
            else -> 2
        }

        return heatmapColors[index]
    }
}
*/
