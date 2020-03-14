# Election Map
Try it out [here](http://jakewalker56.github.io/articles/election-map.html)

This is a tool for visualizing hypothetical changes in demographic preference.  It ties together national demographic exit polling data with district level census and 2016 election data.  The combination of demographic exit polling data and census data also lets us back into an implied turnout rate for each demographic.

# Data Sources
District level election results are shockingly not published by the FEC, but The Daily Kos has compiled district level data [here](https://docs.google.com/spreadsheets/d/1VfkHtzBTP5gf4jAu8tcVQgsBJ1IDvXEHjuMqYlOgYbA/edit#gid=1474862967). State demographic data is pulled from census data by district [here](https://www.census.gov/mycd/). Demographic voting preferences come from CNN exit polls [here](http://www.cnn.com/election/results/exit-polls). With all that you can back into approximate turnout numbers and margins per demographic. 

# Methodology
We first make a prediction for turnout and margin based on district level population demographic data.  We then compare it to observed turnout and margin and make some adjustments to approximate observed turnout and margin per demographic group in each district.  This information then allows us to hypothetically shift the behavior of one demographic group and observe the outcome.

The obvious approach to this approximation is to calculate the difference between the expected and observed values and apply a uniform shift to each demographic group.  For turnout, you simply multiply the expected turnout for each group by the ratio of the observed total district turnout to the expected total district turnout.

It is more problematic to apply a uniform margin shift across all demographic groups, because it can give you a margins above 100% or below 0%.  Imagine the real world example of a district in California that votes 30 points more Democratic than the demographics alone would predict.  Since the national margin for African Americans is ~80 points, applying a 30 point shift would leave them at 110% Democrat, -10% Republican.

Instead, we follow the following algorithm to shift margins while preserving the total number of votes and guaranteeing no demographic group goes outside of [0,100]:
1. Calculate the total "slack" for a given margin shift (e.g. calculate the total number of people in each demographic group that you would have predicted to vote for the other side). This number represents the set of people you have available to convert to a new side
2. Calculate the total number of voters you need to move from one side to the other to acheive your desired margin shift
3. Assign voters to flip from each demographic group  proportional to the percentage of slack made up by members of that group
4. After assigning flip voters, calculate the new margins for each group


    
