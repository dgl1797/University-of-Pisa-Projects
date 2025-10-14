import retrofit2.Call
import retrofit2.http.GET
import retrofit2.http.Query
import com.google.gson.GsonBuilder
import com.unipi.bikeassistant.services.osm.CityBoundaries
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import java.io.IOException

interface OverpassApiService {
    @GET("interpreter")
    fun getCityBoundaries(@Query("data") query: String): Call<CityBoundaries>
}

fun getCityBoundaries(cityName: String): CityBoundaries? {
    val overpassUrl = "https://overpass-api.de/api/"

    // Configure Retrofit
    val gson = GsonBuilder().create()
    val retrofit = Retrofit.Builder()
        .baseUrl(overpassUrl)
        .addConverterFactory(GsonConverterFactory.create(gson))
        .build()

    val service = retrofit.create(OverpassApiService::class.java)

    // Construct the query
    val query = """
        [out:json][timeout:25];
        
        //Area filter
        area[name="${cityName}"];
        (
        //try and get the specific CityName relation
        relation["type"="boundary"]["name"="${cityName}"]["admin_level"="8"]["place"!="town"](area);
        );
        (._;>;);
        out geom;
    """

    // Make the API call to Overpass using Retrofit
    val call = service.getCityBoundaries(query)

    try {
        val response = call.execute()
        if (response.isSuccessful) {
            val responseBody = response.body()
            if (responseBody != null) {
                val jsonString = gson.toJson(responseBody)

                return responseBody
            } else {
                println("Empty response body")
            }
        } else {
            println("Errore nella chiamata all'API Overpass: ${response.code()}")
            println("Response message: ${response.message()}")
            val errorBody = response.errorBody()?.string()
            println("Error body: $errorBody")
        }
    } catch (e: IOException) {
        println("Errore durante la chiamata all'API Overpass: ${e.message}")
    }

    return null
}
