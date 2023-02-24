# Cherry blossom prediction: Liestal, SWITZERLAND

## Data availability

* A large portion of the historic temperature data is available on rnoaa.
* However, no data available for Jan-April 2023 (i.e., cannot make a temperature-based prediction)
  * It means that we cannot use the same method we used for the Kyoto case (AGDD-based lightgbm) and must use a method that does not rely on the meteorological data.

## Data processing

1. On /data/liestal.csv, there are about ~40 historic bloom observation days (bloom_doy).
  * Not enough to build a complex ML model.
  * Perhaps we can utilize the data for the other cities in Switzerland.
  * We extracted a list of cities that have at least 38 bloom observations (like Liestal).

2. Trend plot
  * If the cities have similar trend and intercept for bloom_doy vs. year, then we can treat the observations from other cities as replicates for Liestal.
  * However, we observed on the trend plot that many cities show different trend size and intercepts.

## Model selection

* Since there seem to be different trends and intercepts, we cannot fit a regular linear regression.
* To account for the variability that may be present between the cities, we use the mixed effects models.
* A mixed effects model with Random slope + intercepts
  * Random effect: city. 
  * Fixed effects: year, altitude (alt)
  * We expect that the variable city accounts for the effect of the latitude and longitude since they are distinct in each city.

## Model fitting

## Model evaluation
* Training and test sets.

## Final prediction
* Our model predicts that Liestal's cherry blossom bloom date for 2023 is -.

