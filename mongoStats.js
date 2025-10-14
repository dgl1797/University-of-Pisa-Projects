// min max and average token count of summaries
db.articles.aggregate({$match: {updated: true}}, 
    {$group: 
        {_id: null, 
            minToken: {$min: "$token_count"}, 
            maxToken: {$max: "$token_count"}, 
            tokenAvg: {$avg: "$token_count"}}})

// histogram of summaries token count


db.articles.aggregate([
    {
        $match: {updated: true}
    },
    {
        $bucket: {
        default: "Other",
        boundaries: [ 0, 500, 1000, 1500, 2000, 2500, 3000, 3500 ],      
        groupBy: "$token_count",     
        output: {
            count: {$sum: 1}
            }
        }
    }
])