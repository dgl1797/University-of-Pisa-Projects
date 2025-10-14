package com.unipi.bikeassistant.services.osm

import retrofit2.http.GET
import retrofit2.http.Query

interface NominatimService {
    @GET(value = "reverse?format=json&addressdetails=1")
    suspend fun getAddress(@Query("lat") latitude: Double, @Query("lon") longitude: Double): NominatimResponse
}