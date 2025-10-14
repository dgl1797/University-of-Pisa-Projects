package it.unipi.hadoop;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import org.apache.hadoop.io.Writable;
import org.apache.hadoop.util.bloom.HashFunction;
import org.apache.hadoop.util.bloom.Key;
import org.apache.hadoop.util.hash.Hash;

public class BloomFilter implements Writable{
    private boolean[] filter;
    private int nBits;
    private int nHash;

    private HashFunction h;

    /**
     * @param n -> number of elements to compute
     * @param p -> probability of false positive
     */
    public BloomFilter(int n, double p) {
        // formulas for calculating the filter's parameters
        final double log2squared = Math.pow(Math.log(2), 2);
        final double m_full = -((n * Math.log(p)) / (log2squared));

        final int m = (int) Math.ceil(m_full);

        final double k_full = m / n * Math.log(2);
        final int k = (int) Math.ceil(k_full);

        // filter setup
        this.nBits = m;
        this.nHash = k;
        h = new HashFunction(this.nBits, this.nHash, Hash.MURMUR_HASH);
        filter = new boolean[this.nBits];
    }

    public BloomFilter() {
    }

    /**
     * @return boolean[] return the filter
     */
    public boolean[] getFilter() {
        return filter;
    }

    /**
     * @param filter the filter to set
     */
    public void add(Key word) {
        int[] indexes = h.hash(word);
        for (int i : indexes) {
            filter[i] = true;
        }
    }

    /**
     * @param word -> the string to be checked
     * @return boolean saying if the parameter is present in the bloom filter
     */
    public boolean check(Key word) {
        int[] indexes = h.hash(word);
        for (int i : indexes) {
            if (filter[i] == false)
                return false;
        }
        return true;
    }

    /**
     * @return int return the nBits
     */
    public int getNBits() {
        return nBits;
    }

    /**
     * @return int return the nHash
     */
    public int getNHash() {
        return nHash;
    }

    /**
     * @param nHash the nHash to set
     */
    public void setNHash(int nHash) {
        this.nHash = nHash;
    }

    public String toString() {
        return "n=" + this.nBits + "; k=" + this.nHash;
    }
    
    public String getHashed(Key word) {
        String result = "";
        int[] indexes = h.hash(word);
        for (int i : indexes) {
            result += "" + i + " -> " + (filter[i] == true ? 1 : 0) + "\n";
        }
        return result;
    }

    @Override
    public void write(DataOutput out) throws IOException {
        out.writeInt(this.nBits);
        out.writeInt(this.nHash);
        for (int i = 0; i < this.nBits; i++) {
            out.writeBoolean(filter[i]);
        }
    }

    @Override
    public void readFields(DataInput in) throws IOException {
        this.nBits = in.readInt();
        this.nHash = in.readInt();
        this.filter = new boolean[this.nBits];
        h = new HashFunction(this.nBits, this.nHash, Hash.MURMUR_HASH);
        for (int i = 0; i < this.nBits; i++) {
            filter[i] = in.readBoolean();
        }
    }

    public void set(int n, int k, boolean[] filterField) {
        this.nBits = n;
        this.nHash = k;
        this.filter = filterField.clone();
    }

}
