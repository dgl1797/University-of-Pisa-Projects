sudo cp /root/bloomfilter.py ~/project
sudo cp /root/main.py ~/project
hdfs dfs -rm -r -f /project/spark_output
echo "Enter desired probability of false positives:"
read p
spark-submit --py-files ~/project/bloomfilter.py ~/project/main.py $p --master yarn --deploy-mode cluster 10