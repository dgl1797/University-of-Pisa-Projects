package it.unipi.hadoop;

import org.apache.hadoop.io.Text;
import java.io.IOException;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.NLineInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.bloom.Key;

/**
 * DOCKER: hadoop jar /tmp/projectbloomfilter.jar it.unipi.hadoop.Project /project/input/tsr_short.tsv /project/output

 * VMs: hadoop jar ~/project/projectbloomfilter.jar it.unipi.hadoop.Project /project/input/title_ratings.tsv /project/output
 */
public class Project 
{
    private static final String COUNTER_OUTPUT_PATH = "counters.output.path";
    private static final String FILTER_OUTPUT_PATH = "bloomfilters.output.path";
    private static final String FILTER_CONFIGURATION = "bloomfilters.probability";
    // mapper class takes as input a Text and must produce outputs as Int-Text where Int is the vote and Text 
    // the movie id
    public static class ProjectMapper extends Mapper<Object, Text, IntWritable, Text> {
        private final IntWritable vote = new IntWritable(0);
        private static final Text movie = new Text();

        @Override
        public void map(final Object key, final Text value, final Context context)
                throws IOException, InterruptedException {
            final String[] line = value.toString().split("\\s+");
            if (line.length >= 2) {
                vote.set(Math.round(Float.parseFloat(line[1])));
                movie.set(line[0]);
                context.write(vote, movie);
            }
        }
    }
    
    /**
     * reducer class takes as input the Int key (vote) and Text values (aggregated movies id that received that vote)
     * and produce a simple text to write into the final file
     */
    public static class ProjectReducer extends Reducer<IntWritable, Text, IntWritable, Text> {
        private final int[] dimensions = new int[10];

        @Override
        public void setup(Context context) throws IOException {
            for (int i = 0; i < 10; i++) {
                Path counterOutput = new Path(
                    context.getConfiguration().get(COUNTER_OUTPUT_PATH) + Path.SEPARATOR + "count_" + (i+1));
                FileSystem fs = FileSystem.get(context.getConfiguration());
                try (FSDataInputStream fsdis = fs.open(counterOutput)) {
                    IntWritable val = new IntWritable(0);
                    val.readFields(fsdis);
                    dimensions[i] = val.get();
                } catch (Exception e) {
                    throw new IOException("Error while reading file "+counterOutput.toString()+": " + e.getMessage());
                }
            }
        }

        @Override
        public void reduce(final IntWritable key, final Iterable<Text> values, final Context context)
                throws IOException, InterruptedException {

            final BloomFilter bf = new BloomFilter(dimensions[key.get()-1],
                Double.parseDouble(context.getConfiguration().get(FILTER_CONFIGURATION)));
            // adding all the values with the same vote to the bloomfilter
            for (Text s : values) {
                bf.add(new Key(s.toString().getBytes()));
            }
            // creating a file with the serialized filter
            int vote = key.get();
            Path filtersOutput = new Path(
                    context.getConfiguration().get(FILTER_OUTPUT_PATH) + Path.SEPARATOR + "filter_" + vote
                );
            FileSystem fs = FileSystem.get(context.getConfiguration());
            try(FSDataOutputStream fsdos = fs.create(filtersOutput)) {
                bf.write(fsdos);
            } catch (Exception e) {
                throw new IOException("Error while writing filter in " + FILTER_OUTPUT_PATH);
            }
        }
    }

    public static void main(final String[] args ) throws IOException, ClassNotFoundException, InterruptedException
    {
        Path countersPath = new Path(
                Path.SEPARATOR + "project" + Path.SEPARATOR + "counters");
        for (String arg : args) {
            System.out.println("arg: " + arg);
        }
        // bloom filters probability
        double bfp = 0.05;
        try {
            bfp = Double.parseDouble(args[3]);
        } catch (Exception e) {
            bfp = 0.05;
        }
        if (args.length < 4) {
            System.out.println("p=" + bfp);
        }
        System.out.println("env: "+countersPath.toString());
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
        job.getConfiguration().set(FILTER_OUTPUT_PATH, args[2] + Path.SEPARATOR + "filters");
        job.getConfiguration().set(COUNTER_OUTPUT_PATH, countersPath.toString());
        job.getConfiguration().set(FILTER_CONFIGURATION, ""+bfp);

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
