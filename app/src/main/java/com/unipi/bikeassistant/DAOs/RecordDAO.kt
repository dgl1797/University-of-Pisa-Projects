package com.unipi.bikeassistant.DAOs

import androidx.room.ColumnInfo
import androidx.room.Dao
import androidx.room.Delete
import androidx.room.Entity
import androidx.room.Insert
import androidx.room.PrimaryKey
import androidx.room.Query
import androidx.room.TypeConverter
import androidx.room.TypeConverters
import java.util.Date

class DateConverter {
    @TypeConverter
    fun fromDate(date: Date): Long{
        return date.time
    }

    @TypeConverter
    fun toDate(millis: Long): Date {
        return Date(millis)
    }
}

@Entity @TypeConverters(DateConverter::class) data class ProfileRecord(
    @PrimaryKey val date: Date,
    @ColumnInfo(name = "average_speed") val avgSpeed: Double,
    @ColumnInfo(name = "percurred_distance") val pDistance: Double,
    @ColumnInfo(name = "DCI") val DCI: Double
)

@Dao interface PRecordDAO{
    @Query(value = "SELECT * FROM profilerecord")
    fun getAll(): List<ProfileRecord>

    @Query(value = "SELECT * FROM profilerecord WHERE strftime('%W', date) = strftime('%W', 'now')")
    fun getAllWeek(): List<ProfileRecord>

    @Query(value = "SELECT * FROM profilerecord WHERE strftime('%D', date) = strftime('%D', 'now')")
    fun getToday(): List<ProfileRecord>

    @Insert
    fun insertAll(vararg precord : ProfileRecord)

    @Delete
    fun deleteOne(precord: ProfileRecord)
}