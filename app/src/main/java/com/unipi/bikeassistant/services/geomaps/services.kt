package com.unipi.bikeassistant.services.geomaps

import com.unipi.bikeassistant.FirebaseController.GeoPointWithDCI
import retrofit2.http.Body
import retrofit2.http.GET
import retrofit2.http.POST
import retrofit2.http.Path

interface GeomapService{
    @GET(value = "geomap/{city}")
    suspend fun getCityData(@Path(value = "city") city: String): GetCityDataResponse

    @POST(value = "geomap/{city}/{zone}/")
    suspend fun postRoute(@Path(value = "city") city: String?, @Path(value = "zone") zone: String?, @Body body: PostRouteBody)
}