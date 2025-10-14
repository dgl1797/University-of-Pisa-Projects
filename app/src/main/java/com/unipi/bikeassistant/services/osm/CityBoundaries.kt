package com.unipi.bikeassistant.services.osm

    data class CityBoundaries(
        val elements: List<BoundaryPoints>
    )

    data class BoundaryPoints(
        val type: String,
        val id: Double,
        val bounds: Limiti?,
        val members: Array<BoundaryPoints>?,
        val lat: Double,
        val lon: Double,
        val tags: Tags?,
        val geometry: Array<Geom>?,
    )

data class Tags(
    val capital: String?,
    val gfoss_id: String?,
    val int_name: String?,
    val name: String?,
)

data class Geom(
    val lat: Double,
    val lon: Double,
)

data class Limiti(
    val minlat: Double,
    val minlon: Double,
    val maxlat: Double,
    val maxlon: Double,
)
