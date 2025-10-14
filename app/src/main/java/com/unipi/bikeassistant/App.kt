package com.unipi.bikeassistant

import android.app.Application
import androidx.room.Room
import com.google.firebase.firestore.CollectionReference
import com.google.firebase.firestore.FirebaseFirestore
import com.google.firebase.firestore.ktx.firestore
import com.google.firebase.ktx.Firebase
import com.unipi.bikeassistant.DAOs.AppDB
import com.unipi.bikeassistant.constants.Env
import com.unipi.bikeassistant.services.geomaps.GeomapService
import com.unipi.bikeassistant.services.osm.NominatimService
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory

class App : Application() {
    lateinit var db: AppDB
    lateinit var backend: FirebaseFirestore
    lateinit var geomapsStore: CollectionReference
    lateinit var geomapAPIs : GeomapService
    lateinit var nominatimAPIs: NominatimService

    private fun buildGeomapService(): GeomapService{
        val ctx = applicationContext
        //val hostname = Env.getProperty("HOST", ctx)
        val hostname = "154a-87-19-205-28.eu.ngrok.io"
        val port = Env.getProperty("PORT", ctx)
        val appID = Env.getProperty("APP_ID", ctx)
        val region = Env.getProperty("REGION", ctx)

        return Retrofit.Builder()
            //.baseUrl("http://$hostname:$port/$appID/$region/apis/")
            .baseUrl("https://$hostname/$appID/$region/apis/")
            .addConverterFactory(GsonConverterFactory.create())
            .build()
            .create(GeomapService::class.java)
    }
    private fun buildNominatimService(): NominatimService{
        val ctx = applicationContext
        val hostname = "https://nominatim.openstreetmap.org/"

        return Retrofit.Builder()
            .baseUrl("$hostname")
            .addConverterFactory(GsonConverterFactory.create())
            .build()
            .create(NominatimService::class.java)
    }
    override fun onCreate() {
        super.onCreate()
        db = Room.databaseBuilder(applicationContext, AppDB::class.java, "bike_assistant_db").build()
        backend = Firebase.firestore
        geomapsStore = backend.collection(Env.COLLECTION_NAME)
        geomapAPIs = buildGeomapService()
        nominatimAPIs = buildNominatimService()
    }
}