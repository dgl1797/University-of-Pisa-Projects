package it.unipi.hadoop;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.NLineInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.bloom.Key;

public class ProjectTest {

    // DOCKER: hadoop jar /tmp/projectbloomfilter.jar it.unipi.hadoop.ProjectTest /project/input/tsr_short.tsv /project/test
    
    // VMs: hadoop jar ~/project/testbloomfilter.jar it.unipi.hadoop.ProjectTest /project/input/title_ratings.tsv /project/test

    private static final String FILTER_OUTPUT_PATH = "bloomfilters.output.path";
    
    public static class ProjectMapper extends Mapper<Object, Text, IntWritable, Text> {
        private final IntWritable vote = new IntWritable(0);
        private static final Text movie = new Text();

        @Override
        public void map(final Object key, final Text value, final Context context)
                throws IOException, InterruptedException {
            final String[] line = value.toString().split("\t");
            if (line.length >= 2) {
                vote.set(Math.round(Float.parseFloat(line[1])));
                movie.set(line[0]);
                context.write(vote, movie);
            }
        }
    }

    public static class ProjectReducer extends Reducer<IntWritable, Text, IntWritable, Text> {
        
        private final BloomFilter[] filters = new BloomFilter[10];

        private final int[] truePositives = new int[10];
        private final int[] trueNegatives = new int[10];
        private final int[] falsePositives = new int[10];
        private final int[] falseNegatives = new int[10];

        @Override
        public void setup(Context context) throws IOException, InterruptedException {
            for (int i = 1; i <= 10; i++) {
                filters[i - 1] = new BloomFilter();
                Path filtersOutput = new Path(
                        context.getConfiguration().get(FILTER_OUTPUT_PATH) + Path.SEPARATOR + "filter_" + i);
                FileSystem fs = FileSystem.get(context.getConfiguration());
                try (FSDataInputStream fsdis = fs.open(filtersOutput)) {
                    filters[i - 1].readFields(fsdis);
                } catch (Exception e) {
                    continue;
                }
            }
        }

        @Override
        public void reduce(final IntWritable key, final Iterable<Text> values, final Context context)
                throws IOException, InterruptedException {

            final int vote = key.get();

            for (Text value : values) {
                for (int i = 1; i <= 10; i++) {

                    // check if each received value is inside the bloomfilter
                    if (filters[i - 1].check(new Key(value.toString().getBytes()))) {
                        // the filter tells the value is present
                        if (vote == i) {
                            // the value should be in the filter
                            truePositives[i - 1]++;
                        } else {
                            // the value should not be in the filter
                            falsePositives[i - 1]++;
                        }
                    } else {
                        // the filter tells the value is not present
                        if (vote != i) {
                            // the value should not be in the filter
                            trueNegatives[i - 1]++;
                        } else {
                            // the value should be in the filter
                            falseNegatives[i - 1]++;
                        }
                    }
                }
            }
        }

        @Override
        public void cleanup(Context context) throws IOException, InterruptedException {
            for (int i = 0; i < 10; i++) {
                context.write(
                    new IntWritable(i+1),
                        new Text("TP=" + truePositives[i]
                                + "; TN=" + trueNegatives[i]
                                + "; FP=" + falsePositives[i] + "; FN="+falseNegatives[i])
                );
            }
        }
    }
    
    public static void main(final String[] args ) throws IOException, ClassNotFoundException, InterruptedException
    {
        Path filtersPath = new Path(
                Path.SEPARATOR + "project" + Path.SEPARATOR + "output" + Path.SEPARATOR + "filters");
        for (String arg : args) {
            System.out.println("arg: " + arg);
        }
        System.out.println("env: "+filtersPath.toString());
        // job initialization
        final Configuration conf = new Configuration();
        final Job job = Job.getInstance(conf, "ProjectJob");
        job.setJarByClass(Project.class);
        
        // map's output key-value configuration
        job.setMapOutputKeyClass(IntWritable.class);
        job.setMapOutputValueClass(Text.class);

        // reduce's output key-value configuration
        job.setOutputKeyClass(IntWritable.class);
        job.setOutputValueClass(Text.class);

        // setting map/reduce classes
        job.setMapperClass(ProjectMapper.class);
        job.setReducerClass(ProjectReducer.class);

        // set job input format
        job.setInputFormatClass(NLineInputFormat.class);
        NLineInputFormat.setNumLinesPerSplit(job, 10000);

        // setting I/O formats
        FileInputFormat.setInputPaths(job, new Path(args[1]));
        FileOutputFormat.setOutputPath(job, new Path(args[2]));
        
        // setting filters output in hadoop's environment variable "bloomfilters.output.path"
        job.getConfiguration().set(FILTER_OUTPUT_PATH, filtersPath.toString());

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
