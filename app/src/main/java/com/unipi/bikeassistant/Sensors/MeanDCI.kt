package com.unipi.bikeassistant.Sensors

class MeanDCI {
    private var nSample: Int = 0
    private var totalSum: Float = 0.0f
    private var mean: Float = 0.0f

    fun updateValues(sum: Float)
    {
        // la funzione è chiamata ogni 3 campioni, dunque il numero totale
        // di sample viene incrementato di 3 ogni volta
        nSample+=3
        // aggiorno il totale della somma dei campioni misurati
        // questo serve perchè ogni volta potrò fare somma totale fratto numero campioni
        // ed ottenere la media del parametro DCI
        totalSum+=sum
    }

    fun computeMean(): Float {
        mean = totalSum/nSample
        return mean
    }
}