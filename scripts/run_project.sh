hdfs dfs -rm -r -f /project/output
sudo cp /root/projectbloomfilter.jar ~/project
echo "Enter desired probability of false positives:"
read p
hadoop jar ~/project/projectbloomfilter.jar it.unipi.hadoop.Project /project/input/title_ratings.tsv /project/output $p