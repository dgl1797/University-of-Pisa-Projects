package com.unipi.bikeassistant

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.Button
import com.unipi.bikeassistant.R

class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        title = "Bike Assistant"
        setContentView(R.layout.activity_main)

        val newRideBtn = findViewById<Button>(R.id.main_menu_new)
        val profileBtn = findViewById<Button>(R.id.main_menu_profile)

        newRideBtn.setOnClickListener {
            val intent = Intent(this, NewRideActivity::class.java)
            startActivity(intent)
        }

        profileBtn.setOnClickListener {
            val intent = Intent(this, ProfileActivity::class.java)
            startActivity(intent)
        }
    }
}