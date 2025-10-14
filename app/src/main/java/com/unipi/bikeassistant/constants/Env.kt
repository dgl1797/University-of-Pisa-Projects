package com.unipi.bikeassistant.constants

import android.content.Context
import java.io.IOException
import java.io.InputStream
import java.util.Properties

class Env {
    companion object{
        val COLLECTION_NAME = "geomap"
        @Throws(IOException::class)
        fun getProperty(key: String, context: Context): String{
            val properties = Properties()
            val assetManager = context.assets
            val inStream: InputStream = assetManager.open("config.properties")
            properties.load(inStream)
            return properties.getProperty(key)
        }
    }
}