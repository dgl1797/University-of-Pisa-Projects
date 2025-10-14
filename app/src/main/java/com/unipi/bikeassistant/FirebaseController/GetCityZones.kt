import com.github.kittinunf.fuel.Fuel
import com.github.kittinunf.result.Result
import org.json.JSONArray
import org.json.JSONObject

/*
// un esempio di utilizzo è

val cityName = "Pisa"
val zones = getCityZones(cityName)

println("Zones of $cityName:")
zones.forEach { zone ->
    println(zone)
}
 */

fun getCityZones(cityName: String): List<String> {
    val query = "[out:json][timeout:25];area[name=\"$cityName\"];(node[\"place\"~\"(suburb|neighbourhood|quarter|city_block|plot)\"](area);way[\"place\"~\"(suburb|neighbourhood|quarter|city_block|plot)\"](area);relation[\"place\"~\"(suburb|neighbourhood|quarter|city_block|plot)\"](area););out center;"

    val (_, response, result) = Fuel.get("https://overpass-api.de/api/interpreter", listOf("data" to query)).responseString()

    if (response.statusCode != 200) {
        throw Exception("Failed to retrieve city data.")
    }

    val json = JSONObject(result.get())
    val elements = json.getJSONArray("elements")

    val zones = mutableListOf<String>()

    for (i in 0 until elements.length()) {
        val element = elements.getJSONObject(i)
        val tags = element.getJSONObject("tags")
        val zoneName = tags.optString("name", "")

        if (zoneName.isNotEmpty()) {
            zones.add(zoneName)
        }
    }

    return zones
}
