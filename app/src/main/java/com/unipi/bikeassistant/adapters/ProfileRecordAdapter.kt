package com.unipi.bikeassistant.adapters

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import android.widget.Toast
import androidx.recyclerview.widget.RecyclerView
import com.unipi.bikeassistant.App
import com.unipi.bikeassistant.DAOs.ProfileRecord
import com.unipi.bikeassistant.ProfileActivity
import com.unipi.bikeassistant.R
import com.unipi.bikeassistant.gestures.OnSwipeListener
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.MainScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.text.SimpleDateFormat

class ProfileRecordAdapter(
    var records : List<ProfileRecord>,
    val mainLayoutContext: Context
) : RecyclerView.Adapter<ProfileRecordAdapter.RecordViewHolder>(){

    inner class RecordViewHolder(
        recordView: View,
        adapter: ProfileRecordAdapter,
    ): RecyclerView.ViewHolder(recordView){
        private val dateTextView : TextView = recordView.findViewById(R.id.record_date)
        private val avgSpeedTextView : TextView = recordView.findViewById(R.id.record_avgspeed)
        private val distanceTextView : TextView = recordView.findViewById(R.id.record_distance)
        private val caloriesTextView : TextView= recordView.findViewById(R.id.record_dci)
        private val mainscope = MainScope()
        private val app by lazy { recordView.context.applicationContext as App }
        private val adapter: ProfileRecordAdapter

        init {
            this.adapter = adapter
            recordView.setOnTouchListener(object : OnSwipeListener(recordView.context){
                override fun onSwipe(): Boolean{
                    deleteItem()
                    return true
                }
            })
        }
        fun bindData(rec: ProfileRecord){
            dateTextView.text = "Date: " + SimpleDateFormat("dd/MM/yyyy hh:mm").format(rec.date)
            avgSpeedTextView.text = "Avg. Speed " + rec.avgSpeed.toString() + " km/h"
            distanceTextView.text = "Dist. " + rec.pDistance.toString() + " km"
            caloriesTextView.text = "Avg. DCI " + String.format("%.3f", rec.DCI)
        }

        private fun deleteItem(){
            mainscope.launch {
                kotlin.runCatching {
                    withContext(Dispatchers.IO) { app.db.precordDao().deleteOne(records[adapterPosition]) }
                }.onSuccess {
                    records = records.filter { rec -> rec.date != records[adapterPosition].date }
                    adapter.notifyDataSetChanged()

                }.onFailure {
                    Toast.makeText(mainLayoutContext, "Could not access the External Storage", Toast.LENGTH_SHORT)
                }
            }
        }
    }

    override fun onCreateViewHolder(
        parent: ViewGroup,
        viewType: Int
    ): RecordViewHolder {
        val inflatedView = LayoutInflater.from(parent.context).inflate(R.layout.record_layout, parent, false)
        return RecordViewHolder(inflatedView, this)
    }

    override fun onBindViewHolder(holder: RecordViewHolder, position: Int) {
        holder.bindData(records[position])
    }

    override fun getItemCount(): Int {
        return records.count()
    }

}