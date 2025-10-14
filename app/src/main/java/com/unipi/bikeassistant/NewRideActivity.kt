package com.unipi.bikeassistant

import android.Manifest
import android.annotation.SuppressLint
import android.content.Context
import android.content.pm.PackageManager
import android.graphics.Color
import android.location.Address
import android.location.Geocoder
import android.os.Bundle
import android.os.Looper
import android.widget.Button
import android.widget.ImageButton
import android.widget.TextView
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import com.google.android.gms.location.*
import com.google.firebase.firestore.FirebaseFirestore
import com.google.firebase.firestore.ktx.firestore
import com.google.firebase.ktx.Firebase
import com.unipi.bikeassistant.DAOs.AppDB
import com.unipi.bikeassistant.DAOs.ProfileRecord
import com.unipi.bikeassistant.FirebaseController.GeoPointWithDCI
import com.unipi.bikeassistant.Sensors.ComputeDCI
import com.unipi.bikeassistant.Sensors.DistanceCalculator
import com.unipi.bikeassistant.Sensors.SpeedTracker
import com.unipi.bikeassistant.constants.Codes
import com.unipi.bikeassistant.services.geomaps.CityDoc
import com.unipi.bikeassistant.services.geomaps.PostRouteBody
import com.unipi.bikeassistant.services.geomaps.ZoneDoc
import com.unipi.bikeassistant.services.osm.CityBoundaries
import com.unipi.bikeassistant.services.osm.NominatimResponse
import getCityBoundaries
import kotlinx.coroutines.MainScope
import kotlinx.coroutines.launch
import org.osmdroid.api.IMapController
import org.osmdroid.config.Configuration
import org.osmdroid.tileprovider.tilesource.TileSourceFactory
import org.osmdroid.util.GeoPoint
import org.osmdroid.views.MapView
import org.osmdroid.views.overlay.Polygon
import org.osmdroid.views.overlay.mylocation.MyLocationNewOverlay
import java.lang.Math.atan2
import java.lang.Math.cos
import java.lang.Math.sin
import java.lang.Math.sqrt
import java.util.Date
import kotlin.math.pow


class NewRideActivity : AppCompatActivity(){
        private lateinit var map: MapView
        private lateinit var mapController: IMapController
        private lateinit var mapOverlay: MyLocationNewOverlay
        private lateinit var actualSpeedTextView: TextView
        private lateinit var averageSpeedTextView: TextView
        private lateinit var distanceTravelledTextView: TextView
        private lateinit var locationTextView: TextView
        private lateinit var DCITextView: TextView
        private lateinit var db: AppDB
        private lateinit var recenterMapButton: ImageButton
        private lateinit var startRideButton: Button
        private lateinit var endRideButton: Button
        private lateinit var distanceCalculator: DistanceCalculator
        private lateinit var speedTracker: SpeedTracker
        private lateinit var computeDCI: ComputeDCI
        private lateinit var fusedLocationClient: FusedLocationProviderClient
        private lateinit var locationRequest: LocationRequest
        private lateinit var locationCallback: LocationCallback
        private lateinit var geoPointWithDCI: MutableList<GeoPointWithDCI>
        private lateinit var geoPointCityBoundaries: MutableList<GeoPoint>
    lateinit var backend: FirebaseFirestore
    private lateinit var actualCity: String
    private lateinit var actualZone: String
    private var lastZone: String? = null
    private var lastCity: String? = null
    private lateinit var geocoder: Geocoder
    private lateinit var actualPostRoute: PostRouteBody
    private val app by lazy { applicationContext as App }
    private lateinit var cityPolygon: Polygon
    private lateinit var cellsPolygon: MutableList<Polygon>
    private var cityDoc: CityDoc? = null
    private val mainscope = MainScope()
    private val  YOUR_THRESHOLD_ZOOM_LEVEL: Double = 15.0

    @SuppressLint("SetTextI18n", "MissingInflatedId")

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val ctx: Context = applicationContext
        Configuration.getInstance().load(ctx, ctx.getSharedPreferences("defaultSp", Context.MODE_PRIVATE))
        setContentView(R.layout.activity_new_ride)
        backend = Firebase.firestore
        geoPointWithDCI = mutableListOf()
        geoPointCityBoundaries = mutableListOf()
        geocoder = Geocoder(this)
        cellsPolygon = mutableListOf()

        map = findViewById(R.id.mapview)
        map.setTileSource(TileSourceFactory.MAPNIK)
        map.setMultiTouchControls(true)
        mapController = map.controller
        mapOverlay = MyLocationNewOverlay(map)
        map.overlays.add(mapOverlay)
        distanceCalculator = DistanceCalculator(map)
        speedTracker = SpeedTracker(map)
        computeDCI = ComputeDCI(this)
        cityPolygon = Polygon()

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED){
            // got permission to access position, hence initialize map on position and follow it
            mapController.setZoom(20.2)
            mapOverlay.enableFollowLocation()
        } else {
            ActivityCompat.requestPermissions(this, arrayOf(
                Manifest.permission.ACCESS_FINE_LOCATION,
                Manifest.permission.ACCESS_COARSE_LOCATION
            ), Codes.REQUEST_LOCATION_CODE)
        }

        fusedLocationClient = LocationServices.getFusedLocationProviderClient(this)

        // Set up location request
        locationRequest = LocationRequest.create().apply {
            interval = 2000 // Update interval in milliseconds
            fastestInterval = 2000 // Fastest update interval in milliseconds
            priority = LocationRequest.PRIORITY_HIGH_ACCURACY
        }

        //inizializzo tutti gli elementi grafici della schermata
        recenterMapButton = findViewById<ImageButton>(R.id.recenterMapButton)
        recenterMapButton.setOnClickListener { recenterMap()}

        actualSpeedTextView = findViewById<TextView>(R.id.ActualSpeedTextView)

        distanceTravelledTextView = findViewById<TextView>(R.id.distanceTravelledText)

        DCITextView = findViewById<TextView>(R.id.DCIText)

        averageSpeedTextView = findViewById<TextView>(R.id.averageSpeedText)

        startRideButton = findViewById<Button>(R.id.startRideButton)
        startRideButton.setOnClickListener{startNewRide()}

        locationTextView = findViewById<TextView>(R.id.locationText)

        endRideButton = findViewById<Button>(R.id.endRideButton)
        endRideButton.setOnClickListener {
            computeDCI.stopOperating()
            saveDataToDatabase()
            mainscope.launch { kotlin.runCatching {
                app.geomapAPIs.postRoute(actualCity, actualZone, PostRouteBody(geoPointWithDCI))
                geoPointWithDCI.clear()
            }
                .onFailure { throwable ->
                    throwable.printStackTrace()
                    val errorMessage = throwable.message ?: "Errore sconosciuto"
                    Toast.makeText(this@NewRideActivity, "Errore EndRideFunction: $errorMessage", Toast.LENGTH_SHORT).show()
                }
            }
            endRideButton.isEnabled = false
            startRideButton.isEnabled = true
            actualSpeedTextView.text = ""
            averageSpeedTextView.text = ""
            distanceTravelledTextView.text = ""
            DCITextView.text = ""
            distanceCalculator.removePolyline()
            fusedLocationClient.removeLocationUpdates(locationCallback)
        }
        endRideButton.isEnabled = false
    }

    //funzione che resetta lo zoom della mappa al valore centrale e la ricentra nella posizione attuale
    private fun recenterMap(){
        val CurrentGeoPoint = mapOverlay.myLocation
        mapController.setCenter(CurrentGeoPoint)
        mapController.setZoom(20.2)
        mapOverlay.enableFollowLocation()
    }

    //funzione che si occupa di aggiornare tutte le statistiche a schermo nel momento in cui viene rilevato un cambiamento della posizione (al più una al secondo)
    private fun startNewRide(){

        //inizio a calcolare i valori di dci partendo dai dati prelevati dall'accelerometro
        computeDCI.startOperating()
        var currentDCIValue = 1f

        //funzione che aggiorna la posizione e modifica le statistiche a schermo
        locationCallback = object : LocationCallback() {
            override fun onLocationResult(locationResult: LocationResult) {
                for (location in locationResult.locations) {
                    // Handle location updates
                    distanceCalculator.updateLocation(location, currentDCIValue)
                    speedTracker.updateLocation(
                        location,
                        distanceCalculator.getCurrentDistance().toFloat()
                    )
                    val actualGeoPoint = GeoPoint(location.latitude, location.longitude)
                    mainscope.launch {
                        kotlin.runCatching {
                            val result: NominatimResponse = app.nominatimAPIs.getAddress(
                                actualGeoPoint.latitude, actualGeoPoint.longitude
                            )
                            if (result.address.city != null) {
                                actualCity = result.address.city
                            } else {
                                if (result.address.town != null) {
                                    actualCity = result.address.town
                                }
                            }
                            if (result.address.suburb != null) {
                                actualZone = result.address.suburb
                            } else {
                                if (result.address.village != null) {
                                    actualZone = result.address.village
                                }
                            }
                            Thread {
                                if (!::actualZone.isInitialized){
                                    actualZone = actualCity
                                }
                                if (actualZone == null)
                                    actualZone = actualCity
                                // Utilizza zone come necessario
                                if (actualCity == lastCity) {
                                    if (actualZone != lastZone) {
                                        val cityInvioDati = lastCity
                                        val zonaInvioDati = lastZone
                                        val listaPunti = geoPointWithDCI
                                        //Manda i dati raccolti nella vecchia zona, crea la nuova struttura dati
                                        mainscope.launch {
                                            kotlin.runCatching {
                                                app.geomapAPIs.postRoute(cityInvioDati, zonaInvioDati, PostRouteBody(listaPunti))
                                                geoPointWithDCI.clear()
                                            }
                                                .onFailure {
                                                    // TODO: attenzione a cancellare il testo prima della consegna 🤣🤣
                                                    Toast.makeText(this@NewRideActivity, "Errore nell'invio dati nuova zona", Toast.LENGTH_SHORT).show()
                                                }
                                        }
                                        if(cityDoc != null) {
                                            for(polygon in cellsPolygon){
                                                if(map.overlays.contains(polygon))
                                                    map.overlays.remove(polygon)
                                            }
                                            cellsPolygon.clear()
                                            for (zone in cityDoc!!.zones) {
                                                if (zone.name == actualZone) {
                                                    DrawZoneCells(zone, cellsPolygon)
                                                }
                                            }
                                        }
                                    } else {
                                        //aggiunge i dati relativi al dci e aumenta il numero di sample raccolti
                                        mainscope.launch {
                                            kotlin.runCatching {
                                                var newGeoPointWithDCI = GeoPointWithDCI(actualGeoPoint, computeDCI.getCurrentDCI().toDouble())
                                                if (newGeoPointWithDCI.dci != Double.NaN && newGeoPointWithDCI.dci < 1) {
                                                    geoPointWithDCI.add(newGeoPointWithDCI)
                                                }
                                            }
                                                .onFailure {
                                                    // TODO: attenzione a cancellare il testo prima della consegna 🤣🤣
                                                    Toast.makeText(this@NewRideActivity, "Errore aggiunta nuovo dato", Toast.LENGTH_SHORT).show()
                                                }
                                        }
                                    }
                                } else {
                                    println("Sono qua, cambio città")
                                    val cityInvioDati = lastCity
                                    val zonaInvioDati = lastZone
                                    val listaPunti = geoPointWithDCI
                                    if(cityDoc != null) {
                                        println("Elimino la roba dal vecchio cityDoc")
                                        for (polygon in cellsPolygon) {
                                            if (map.overlays.contains(polygon))
                                                map.overlays.remove(polygon)
                                        }
                                    }
                                    if(cellsPolygon != null && cityPolygon != null) {
                                        cellsPolygon.clear()
                                        if (map.overlays.contains(cityPolygon))
                                            map.overlays.remove(cityPolygon)
                                    }
                                    println("Ottengo i nuovi dati sulla mappa")
                                    val cityBoundaries = getCityBoundaries(actualCity)
                                    var geoPointCityBoundaries2 = mutableListOf<GeoPoint>()
                                    if(cityBoundaries != null)
                                        geoPointCityBoundaries2 = getCityBoundaries2(cityBoundaries)
                                    //Manda i dati raccolti nella vecchia zona e città, crea la nuova struttura dati per la nuova città
                                    mainscope.launch {
                                        kotlin.runCatching {
                                            cityDoc = app.geomapAPIs.getCityData(actualCity).data
                                            cityPolygon = generateContornoCity(geoPointCityBoundaries2, cityDoc!!.avgDCI)
                                            for (zone in cityDoc!!.zones) {
                                                if (zone.name == actualZone.lowercase()) {
                                                    DrawZoneCells(zone, cellsPolygon)
                                                }
                                            }
                                        }
                                            .onFailure {
                                                // TODO: attenzione a cancellare il testo prima della consegna 🤣🤣
                                                Toast.makeText(this@NewRideActivity, "Errore cambio città e cambio confini", Toast.LENGTH_SHORT).show()
                                            }
                                    }
                                    mainscope.launch {
                                        kotlin.runCatching {
                                            app.geomapAPIs.postRoute(cityInvioDati, zonaInvioDati, PostRouteBody(listaPunti))
                                            geoPointWithDCI.clear()
                                        }.onFailure {
                                            // TODO: attenzione a cancellare il testo prima della consegna 🤣🤣
                                            Toast.makeText(this@NewRideActivity, "Errore invio dati: Caso Cambia Città", Toast.LENGTH_SHORT).show()
                                        }
                                    }
                                }
                                lastCity = actualCity
                                lastZone = actualZone
                            }.start()
                        }.onFailure {
                            println(it.stackTrace)
                            Toast.makeText(this@NewRideActivity, "Errore lettura posizione", Toast.LENGTH_SHORT).show()
                        }
                    }
                    // Update the UI with the updated distance and speed values
                    runOnUiThread {
                        if(::actualCity.isInitialized && ::actualZone.isInitialized)
                            locationTextView.text = ("Location: " + actualCity + " " + actualZone)
                        computeDCI.calculateCurrentDCI()
                        currentDCIValue = computeDCI.getCurrentDCI()
                        actualSpeedTextView.text = speedTracker.getCurrentSpeedInString()
                        averageSpeedTextView.text = speedTracker.getCurrentAverageSpeedInString()
                        distanceTravelledTextView.text = distanceCalculator.getCurrentDistanceInString()
                        DCITextView.text = computeDCI.getCurrentDCIinString()
                        val currentZoomLevel = map.zoomLevelDouble
                        if (currentZoomLevel < YOUR_THRESHOLD_ZOOM_LEVEL) {
                            if( !map.overlays.contains(cityPolygon)) {
                                map.overlays.add(cityPolygon)
                            }
                            for(polygon in cellsPolygon){
                                if(map.overlays.contains(polygon))
                                    map.overlays.remove(polygon)
                            }
                        } else {
                            if(map.overlays.contains(cityPolygon))
                                map.overlays.remove(cityPolygon)
                            for(polygon in cellsPolygon){
                                if(!map.overlays.contains(polygon))
                                    map.overlays.add(polygon)
                            }
                        }
                    }
                }
            }
        }

        // Request location updates
        if (ActivityCompat.checkSelfPermission(
                this,
                Manifest.permission.ACCESS_FINE_LOCATION
            ) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(
                this,
                Manifest.permission.ACCESS_COARSE_LOCATION
            ) != PackageManager.PERMISSION_GRANTED
        ) {
            return
        }
        fusedLocationClient.requestLocationUpdates(locationRequest, locationCallback, Looper.getMainLooper())
        endRideButton.isEnabled = true
        startRideButton.isEnabled = false
    }

    private fun saveDataToDatabase() {
        val avgSpeedText = averageSpeedTextView.text.toString().replace(",",".")
        val avgSpeedValue  = avgSpeedText.toDouble()

        val pDistanceText  = distanceTravelledTextView.text.toString().replace(",",".")
        val pDistanceValue = pDistanceText.toDouble()

        var dciValue = 0.0
        if(computeDCI.getMediaDCI() == Float.NaN)
            dciValue = 0.0
        else
            dciValue = computeDCI.getMediaDCI().toDouble()

        val date: Date = Date()
        val newRecord = ProfileRecord(date, avgSpeedValue ?: 0.0, pDistanceValue?: 0.0, dciValue ?: 0.0)

        Thread {
            app.db.precordDao().insertAll(newRecord)
        }.start()
    }

    override fun onResume(){
        super.onResume()
        map.onResume()
        mapOverlay.enableFollowLocation()
    }

    override fun onPause() {
        super.onPause()
        map.onPause()
        mapOverlay.disableFollowLocation()
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        if(requestCode == Codes.REQUEST_LOCATION_CODE){
            if(grantResults.isNotEmpty() && grantResults[0] == PackageManager.PERMISSION_GRANTED){
                mapController.setZoom(20.2)
                mapOverlay.enableFollowLocation()
            } else {
                val randomPoint = GeoPoint(341.412, 443.0)
                mapController.animateTo(randomPoint)
                mapController.setZoom(20.2)
            }
        }
    }

    fun getCityName(position: GeoPoint): List<String?>{
        var returnAddress = mutableListOf<String?>()
        mainscope.launch {
            kotlin.runCatching {
                val result: NominatimResponse = app.nominatimAPIs.getAddress(position.latitude, position.longitude)
                if(result.address.city != null) {
                    returnAddress.add(result.address.city)
                } else {
                returnAddress.add(result.address.town)
                }
                if(result.address.suburb != null){
                    returnAddress.add(result.address.suburb)
                }else{
                    returnAddress.add(result.address.village)
                }
                println(returnAddress)
            }.onFailure {
                println(it.stackTrace)
                Toast.makeText(this@NewRideActivity, "EH VOLEEVI", Toast.LENGTH_SHORT).show()
            }
        }
        println(returnAddress)
        return returnAddress
    }

    fun getCityCenter(cityName: String): GeoPoint{
        val addresses: List<Address>? = geocoder.getFromLocationName(cityName, 1)
        val center: GeoPoint
        val address = addresses!![0]
        val latitude = address.latitude
        val longitude = address.longitude
        center = GeoPoint(latitude, longitude)
        return center
    }
    fun getCityBoundaries2(cityBoundaries: CityBoundaries?): MutableList<GeoPoint>{
        val gPointCityBoundaries = mutableListOf<GeoPoint>()
        val gPointCityBoundariesList = mutableListOf<Array<GeoPoint>>()
        if (cityBoundaries != null) {
            // Utilizza l'oggetto cityBoundaries per accedere ai confini della città
            for (boundary in cityBoundaries.elements) {
                if(boundary.type == "relation" && boundary.members != null){
                    for (point in boundary.members) {
                        if(point.geometry != null && point.geometry.size > 2){
                            var ArrayGeoPoints: Array<GeoPoint> = arrayOf()
                            for(position in point.geometry){
                                ArrayGeoPoints += GeoPoint(position.lat, position.lon)
                                println(GeoPoint(position.lat, position.lon))
                            }
                            println(ArrayGeoPoints.toString())
                            gPointCityBoundariesList.add(ArrayGeoPoints)
                        }
                    }
                }
            }
        } else {
            println("Errore durante il recupero dei confini della città.")
        }
        var sortedGPointCityBoundariesList: MutableList<Array<GeoPoint>> = mutableListOf<Array<GeoPoint>>()
        sortedGPointCityBoundariesList = sortCityBoundariesArray(gPointCityBoundariesList)
        for(array in sortedGPointCityBoundariesList)
            for(geoPoint in array){
                gPointCityBoundaries.add(geoPoint)
                println(geoPoint)
            }
        return gPointCityBoundaries
    }

    fun distanceBetweenPoints(point1: GeoPoint, point2: GeoPoint): Double {
        val earthRadiusKm = 6371.0

        val lat1Rad = Math.toRadians(point1.latitude)
        val lon1Rad = Math.toRadians(point1.longitude)
        val lat2Rad = Math.toRadians(point2.latitude)
        val lon2Rad = Math.toRadians(point2.longitude)

        val deltaLat = lat2Rad - lat1Rad
        val deltaLon = lon2Rad - lon1Rad

        val a = sin(deltaLat / 2).pow(2) +
                cos(lat1Rad) * cos(lat2Rad) * sin(deltaLon / 2).pow(2)
        val c = 2 * atan2(sqrt(a), sqrt(1 - a))

        return earthRadiusKm * c
    }
    fun sortCityBoundariesArray(PointCityBoundariesArrays: List<Array<GeoPoint>>): MutableList<Array<GeoPoint>> {
        var sortedCityBoundariesArray = mutableListOf<Array<GeoPoint>>()
        sortedCityBoundariesArray.add(PointCityBoundariesArrays.first())
        var remainingBoundaries = PointCityBoundariesArrays.drop(1)
        while (remainingBoundaries.isNotEmpty()) {
            val lastPosition = sortedCityBoundariesArray.last().lastOrNull()

            var closestDistance = Double.MAX_VALUE
            var closestPosition: Array<GeoPoint>? = null

            for (position in remainingBoundaries) {
                val distanceToFirst = lastPosition?.let { distanceBetweenPoints(it, position.first()) }
                    ?: Double.MAX_VALUE
                val distanceToLast = lastPosition?.let { distanceBetweenPoints(it, position.last()) }
                    ?: Double.MAX_VALUE

                if (distanceToFirst < closestDistance || distanceToLast < closestDistance) {
                    closestDistance = minOf(distanceToFirst, distanceToLast)
                    closestPosition = position
                }
            }

            closestPosition?.let {
                val reversedArray = if (distanceBetweenPoints(lastPosition!!, closestPosition.last()) < distanceBetweenPoints(
                        lastPosition,
                        closestPosition.first()
                    )
                ) {
                    closestPosition.reversedArray()
                } else {
                    closestPosition
                }

                sortedCityBoundariesArray.add(reversedArray)
                remainingBoundaries = remainingBoundaries.toMutableList().apply { remove(closestPosition) }
            }
        }

        return sortedCityBoundariesArray
    }

    fun generateContornoCity(geoPointCityBoundaries: MutableList<GeoPoint>,dci: Double): Polygon {
        println("Carico i punti del confine della città")
        val polygon = Polygon()
        polygon.setPoints(geoPointCityBoundaries)
        val color = generateColorBasedOnDCI(dci)
        polygon.fillColor = color
        polygon.holes.clear()
        polygon.strokeWidth = 1f
        polygon.strokeColor = color
        return polygon
    }

    fun DrawZoneCells(zone: ZoneDoc, cellsPolygon: MutableList<Polygon>){
        for(cell in zone.cells){
            var polygon = Polygon()
            for(point in cell.bbox){
                polygon.addPoint(GeoPoint(point.latitude,point.longitude))
            }
            polygon.fillColor = generateColorBasedOnDCI(cell.avgDCI)
            polygon.strokeWidth = 1f
            polygon.strokeColor = generateColorBasedOnDCI(cell.avgDCI)
            cellsPolygon.add(polygon)
        }
    }
    fun generateColorBasedOnDCI(dci: Double): Int{
        var hue: Float = 120f

        if (dci <= 0.5) {
            hue = (1 - dci / 0.5).toFloat() * 120
        }

        val hsv = floatArrayOf(hue, 1f, 1f)
        val color = Color.HSVToColor(hsv)
        val fillColor = Color.argb(64, Color.red(color), Color.green(color), Color.blue(color))

        return fillColor
    }
}
