
# MLB Odds

<!-- badges: start -->
<!-- badges: end -->

Parse MLB odds from [sportsbookreviewsonline.com](https://www.sportsbookreviewsonline.com/scoresoddsarchives/mlb/mlboddsarchives.htm)

- seasons 2010 to 2019
- ML/OU for all seasons
- ML/RL/OU for 2014 to 2019

If you plan on running the code yourself make sure to fix a bad rotation number in the file `mlb odds 2018.xlsx`. You can do it by hand or by running `mlb18[(mlb18$Rot == 907 & mlb18$Date == 518 & mlb18$Team == 'CIN'), 'Rot'] <- 908` and resaving the file.


