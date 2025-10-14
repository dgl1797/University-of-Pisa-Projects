hdfs dfs -rm -r -f /project/counters
sudo cp /root/counterbloomfilter.jar ~/project
hadoop jar ~/project/counterbloomfilter.jar it.unipi.hadoop.Counter /project/input/title_ratings.tsv /project/counters/output