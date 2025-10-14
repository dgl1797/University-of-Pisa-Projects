package com.unipi.bikeassistant.Sensors

import android.content.Context
import android.hardware.Sensor
import android.hardware.SensorEvent
import android.hardware.SensorEventListener
import android.hardware.SensorManager
import android.hardware.SensorManager.getRotationMatrixFromVector
import android.os.Build
import androidx.annotation.RequiresApi

/*
onAccelerometerChanged che viene chiamato ogni volta che i valori dell'accelerometro vengono
 modificati. La classe implementa l'interfaccia SensorEventListener e il metodo
 onSensorChanged viene chiamato ogni volta che il valore del sensore cambia. Se il sensore
 è un accelerometro, la classe chiama il metodo onAccelerometerChanged del listener con i
 valori dell'accelerometro sull'asse x, y e z.
 */

class AccelerometerManager(private val context: Context, private val listener: AccelerometerListener) : SensorEventListener {

    private var sensorManager: SensorManager? = null
    private var accelerometer: Sensor? = null
    private var rotationVector: Sensor? = null
    private var rotationMatrix = FloatArray(9)
    private var rotationVectorData = FloatArray(3)
    private var orientation = FloatArray(3)
    private var gravity = FloatArray(3)

    private var SENSOR_DELAY = 5000

    interface AccelerometerListener {
        fun onAccelerometerChanged(z: Float, orientation: FloatArray)
    }

    init {
        sensorManager = context.getSystemService(Context.SENSOR_SERVICE) as SensorManager
        accelerometer = sensorManager?.getDefaultSensor(Sensor.TYPE_ACCELEROMETER)
        rotationVector = sensorManager?.getDefaultSensor(Sensor.TYPE_ROTATION_VECTOR)
    }

    fun startListening() {
        sensorManager?.registerListener(this, accelerometer, SENSOR_DELAY)
    }

    fun stopListening() {
        sensorManager?.unregisterListener(this)
    }

    override fun onAccuracyChanged(sensor: Sensor?, accuracy: Int) {
        // non utilizzato
    }

    fun getRorationMatrix(): FloatArray {
        return rotationMatrix
    }

    @RequiresApi(Build.VERSION_CODES.GINGERBREAD)
    override fun onSensorChanged(event: SensorEvent?) {
        if (event?.sensor?.type == Sensor.TYPE_ACCELEROMETER) {
            listener.onAccelerometerChanged(event.values[2], orientation)
        }
        if (event?.sensor?.type == Sensor.TYPE_ROTATION_VECTOR){
            rotationVectorData = event.values
            getRotationMatrixFromVector(rotationMatrix!!, rotationVectorData!!)
            SensorManager.getOrientation(rotationMatrix, orientation)
        }
    }
}