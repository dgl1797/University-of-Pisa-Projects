package com.unipi.bikeassistant.DAOs

import androidx.room.Database
import androidx.room.RoomDatabase

// for each new table append the DAO class to the list of entities and add a getter
@Database(entities = [ProfileRecord::class], version = 1)
abstract class AppDB : RoomDatabase() {
    // DAOs Getters
    abstract fun precordDao(): PRecordDAO
}