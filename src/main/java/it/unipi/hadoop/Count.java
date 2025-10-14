package it.unipi.hadoop;

import org.apache.hadoop.fs.Path;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.NLineInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class Count {
    private static final String COUNTER_OUTPUT_PATH = "counters.output.path";

    public static class CounterMapper extends Mapper<Object, Text, IntWritable, Text> {
        private final IntWritable vote = new IntWritable(0);
        
        @Override
        public void map(final Object key, final Text value, final Context context)
                throws IOException, InterruptedException {

            final String[] line = value.toString().split("\t");
            if (line.length >= 2) {
                vote.set(Math.round(Float.parseFloat(line[1])));
                context.write(vote, new Text(line[0]));
            }
        }
    }

    public static class CounterReducer extends Reducer<IntWritable, Text, IntWritable, IntWritable> {
        @Override
        public void reduce(final IntWritable key, final Iterable<Text> values, final Context context)
                throws IOException, InterruptedException {
            final int vote = key.get();
            int counter = 0;

            for (Text t : values) {
                counter += 1;
            }

            context.write(key, new IntWritable(counter));

            Path counterOutput = new Path(
                    context.getConfiguration().get(COUNTER_OUTPUT_PATH) + Path.SEPARATOR + "count_" + vote);
            FileSystem fs = FileSystem.get(context.getConfiguration());
            try (FSDataOutputStream fsdos = fs.create(counterOutput)) {
                new IntWritable(counter).write(fsdos);
            } catch (Exception e) {
                throw new IOException("Error while creating file "+counterOutput+": " + e.getMessage());
            }
        }
    }
    public static void main(String args[]) throws IOException, ClassNotFoundException, InterruptedException {
        Path countersPath = new Path(
                Path.SEPARATOR + "project" + Path.SEPARATOR + "counters");

        for (String arg : args) {
            System.out.println("arg: " + arg);
        }
        System.out.println("env: " + countersPath.toString());
        
        // job initialization
        final Configuration conf = new Configuration();
        final Job job = Job.getInstance(conf, "ProjectJob");
        job.setJarByClass(Count.class);

        // map's output key-value configuration
        job.setMapOutputKeyClass(IntWritable.class);
        job.setMapOutputValueClass(Text.class);
        
        // reduce's output key-value configuration
        job.setOutputKeyClass(IntWritable.class);
        job.setOutputValueClass(IntWritable.class);

        // setting map/reduce classes
        job.setMapperClass(CounterMapper.class);
        job.setReducerClass(CounterReducer.class);

        // set job input format
        job.setInputFormatClass(NLineInputFormat.class);
        NLineInputFormat.setNumLinesPerSplit(job, 10000);

        // setting I/O formats
        FileInputFormat.setInputPaths(job, new Path(args[1]));
        FileOutputFormat.setOutputPath(job, new Path(args[2]));
       
        // setting counters output in hadoop's environment variable "bloomfilters.output.path"
        job.getConfiguration().set(COUNTER_OUTPUT_PATH, countersPath.toString());

        //starting job
        long startTime = System.nanoTime();
        if (job.waitForCompletion(true)) {
            long endTime = System.nanoTime();
            System.out.println("Total time for execution: "+((endTime-startTime)/1000000000)+"s");
        } else {
            System.exit(1);
        }
    }
}
