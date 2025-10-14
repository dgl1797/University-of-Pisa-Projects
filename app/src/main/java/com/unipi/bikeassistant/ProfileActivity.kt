package com.unipi.bikeassistant

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.FrameLayout
import android.widget.LinearLayout
import android.widget.Toast
import androidx.core.view.isVisible
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout
import com.unipi.bikeassistant.adapters.ProfileRecordAdapter
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.MainScope
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

class ProfileActivity : AppCompatActivity() {

    private lateinit var recordRecyclerView: RecyclerView
    private lateinit var progressBar : FrameLayout
    private lateinit var swipeRefresher : SwipeRefreshLayout
    private lateinit var recAdapter: ProfileRecordAdapter

    private val mainscope = MainScope()
    private val app by lazy { applicationContext as App }

    override fun onDestroy(){
        super.onDestroy()
        mainscope.cancel()
    }

    private fun displayRecords(needReload: Boolean){
        progressBar.isVisible = true

        mainscope.launch {
            kotlin.runCatching {
                withContext(Dispatchers.IO) { app.db.precordDao().getAll() }
            }.onSuccess { recordList ->
                recAdapter.records = recordList
                recAdapter.notifyDataSetChanged()
            }.onFailure {
                // Toast.makeText(this@ProfileActivity, it.localizedMessage, Toast.LENGTH_SHORT).show()
                Toast.makeText(this@ProfileActivity, "Could not access the data storage", Toast.LENGTH_SHORT).show()
            }
            progressBar.isVisible = false
        }
    }
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        title = "Your Activities"
        setContentView(R.layout.activity_profile)

        val mainLayout = findViewById<LinearLayout>(R.id.profile_main_page)

        recAdapter = ProfileRecordAdapter(listOf(), mainLayout.context)

        progressBar = findViewById(R.id.progressBar)
        swipeRefresher = findViewById(R.id.swiperefresh)
        recordRecyclerView = findViewById(R.id.records_list)
        recordRecyclerView.adapter = recAdapter
        recordRecyclerView.layoutManager = LinearLayoutManager(this)

        swipeRefresher.setOnRefreshListener {
            swipeRefresher.isRefreshing = false
            displayRecords(true)
        }
        displayRecords(false)

    }
}