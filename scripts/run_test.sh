hdfs dfs -rm -r -f /project/test
sudo cp /root/testbloomfilter.jar ~/project
hadoop jar ~/project/testbloomfilter.jar it.unipi.hadoop.ProjectTest /project/input/title_ratings.tsv /project/test