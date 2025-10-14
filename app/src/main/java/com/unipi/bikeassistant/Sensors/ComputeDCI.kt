package com.unipi.bikeassistant.Sensors

import android.content.Context

class ComputeDCI(private val context: Context): AccelerometerManager.AccelerometerListener {
    private lateinit var accelerometerManager: AccelerometerManager
    var gravityZ = 0f
    val alpha = 0.8f // Fattore di smoothing, regolabile tra 0 e 1


    // questa variabile contiene volta volta il valore del DCI istantaneo
    private var dynamicControlIndex: Float = 0.0f

    // Le due variabili seguenti servono per mantenere la media del DCI
    private var sample: Int = 0
    private var sum: Float = 0.0f

    // numero di campioni di accelerazione sull'asse z che sono stati prelevati finora
    private var accelerationSamplesCount = 0

    // somma dei quadrati di questi campioni
    private var accelerationSumOfSquares = 0f

    init{
        accelerometerManager = AccelerometerManager(context, this)
    }

    override fun onAccelerometerChanged(z: Float, orientation: FloatArray) {
        // TODO: limitare a 200Hz
        val accelerationOnZAxis = (z * Math.cos(orientation[1].toDouble()) * Math.cos(orientation[2].toDouble()));
        gravityZ = alpha * gravityZ + (1 - alpha) * accelerationOnZAxis.toFloat()
        val linearAccelerationZ: Float = accelerationOnZAxis.toFloat() - gravityZ
        accelerationSamplesCount++
        accelerationSumOfSquares += linearAccelerationZ.toFloat() * linearAccelerationZ.toFloat()
        //dynamicControlIndex = kotlin.math.sqrt(accelerationSumOfSquares / accelerationSamplesCount)
        //dynamicControlIndex = 1/dynamicControlIndex
        //sum+=dynamicControlIndex

        // ogni tre campioni
        /*if(accelerationSamplesCount == 3)
        {
            /*
            mean.updateValues(sum) // aggiorno i valori nella classe "MeanDCI"
            sample = 0 // riporto a zero i sample
            sum = 0F // riporto a zero la somma degli ultimi 3 sample
            accelerationSamplesCount = 0
            accelerationSumOfSquares = 0F
            */
            dynamicControlIndex = kotlin.math.sqrt(accelerationSumOfSquares/accelerationSamplesCount)
            accelerationSumOfSquares = 0f
            accelerationSamplesCount = 0
        }
         */
    }

    fun calculateCurrentDCI(){
        dynamicControlIndex = kotlin.math.sqrt(accelerationSumOfSquares/accelerationSamplesCount)
        // TODO: calcolare media finale sui punti visti
        // final_average += dynamicControlIndex
        accelerationSumOfSquares = 0f
        accelerationSamplesCount = 0
        if(dynamicControlIndex == Float.NaN) return
        if(dynamicControlIndex > 1) {
            sum += 1 / dynamicControlIndex
            sample++
        }
    }

    fun getCurrentDCIinString(): String{
        var formattedString = String.format("%.3f", 1/dynamicControlIndex)
        return formattedString
    }

    fun getCurrentDCI(): Float{
        val n = 1/dynamicControlIndex
        return n
    }

    fun getMediaDCI(): Float{
        var media = sum/sample
        return media
    }

    fun startOperating(){
        accelerometerManager.startListening()
    }

    fun stopOperating(){
        accelerometerManager.stopListening()
    }
}