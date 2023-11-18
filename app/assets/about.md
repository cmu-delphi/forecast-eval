### Forecast Evaluation Dashboard

This dashboard was developed by:

* Chris Scott (Delphi Group, Google Fellow)
* Kate Harwood (Delphi Group, Google Fellow)
* Jed Grabman (Delphi Group, Google Fellow)

with the Forecast Evaluation Research Collaborative:

* Ryan Tibshirani (Delphi Group)
* Nicholas Reich (Reich Lab)
* Evan Ray (Reich Lab)
* Daniel McDonald (Delphi Group)
* Estee Cramer (Reich Lab)
* Logan Brooks (Delphi Group)
* Johannes Bracher (Karlsruhe Institute)
* Jacob Bien (Delphi Group)

Forecast data in all states and U.S. territories are supplied by:

[![COVID-19 Forecast Hub](forecast-hub-logo.png)](https://covid19forecasthub.org)

### Forecast Evaluation Research Collaborative

The Forecast Evaluation Research Collaborative was founded by:

* Carnegie Mellon University [Delphi Group](https://delphi.cmu.edu)
* UMass-Amherst [Reich Lab](https://reichlab.io/)

Both groups are funded by the CDC as Centers of Excellence for Influenza and COVID-19 Forecasting. We have partnered together on this project to focus on providing a robust set of tools and methods for evaluating the performance of epidemic forecasts.  
  
The collaborative’s mission is to help epidemiological researchers gain insights into the performance of their forecasts and lead to more accurate forecasting of epidemics.  
  
Both groups lead initiatives related to COVID-19 data and forecast curation. The Reich Lab created and maintains the [COVID-19 Forecast Hub](https://covid19forecasthub.org/), a collaborative effort with over 80 groups submitting forecasts to be part of the official [CDC COVID-19 ensemble forecast](https://www.cdc.gov/coronavirus/2019-ncov/covid-data/mathematical-modeling.html). The Delphi Group created and maintains COVIDcast, a platform for [epidemiological surveillance data](https://delphi.cmu.edu/covidcast/), and runs the [U.S. COVID-19 Trends and Impact Survey in partnership with Facebook](https://delphi.cmu.edu/covidcast/surveys/).
  
The Forecaster Evaluation Dashboard is a collaborative project, which has been made possible by the 13 pro bono Google.org Fellows who have spent 6 months working full-time with the Delphi Group. Google.org is [committed](https://www.google.org/covid-19/) to the recovery of lives and communities that have been impacted by COVID-19 and investing in developing the science to mitigate the damage of future pandemics.  
  
### About the Data

#### **Sources**

**Observed cases and deaths** are from the [COVID-19 Data Repository](https://github.com/CSSEGISandData/COVID-19) by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University.

**Observed hospitalizations** are from the U.S. Department of Health & Human Services and is the sum of all adult and pediatric COVID-19 hospital admissions.
  
**Forecaster predictions** are drawn from the [COVID-19 Forecast Hub GitHub repository](https://github.com/reichlab/covid19-forecast-hub/)  
  
Data for the dashboard is pulled from these sources on Sunday, Monday, and Tuesday each week.

#### **Terms**

*   **Forecaster**:  A named model that produces forecasts, e.g., "COVIDhub-ensemble"
    
*   **Forecast**: A set of quantile predictions for a specific target variable, epidemiological week, and location 
    
*   **Target Variable**: What the forecast is predicting, e.g., “weekly incident cases”
        
*   **Epidemiological week (MMWR week)**: A standardized week that starts on a Sunday. See the [CDC definition](https://wwwn.cdc.gov/nndss/document/MMWR_week_overview.pdf) for additional details.

*   **Horizon**: The duration of time between when a prediction was made and the end of the corresponding epidemiological week. Following the [Reich Lab definition](https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md#target), a forecast has a horizon of 1 week if it was produced no later than the Monday of the epidemiological week it forecasts. Thus, forecasts made 5-11 days before the end of the corresponding epidemiological week have a horizon of 1 week, 12-18 days before have a horizon of 2 weeks, etc. 
  
#### **Dashboard Inclusion Criteria**
A forecast is only included if all the following criteria are met:

*   The target variable is the weekly incidence of either cases or deaths, or the daily incidence of hospitalizations
*   The horizon is no more than 4 weeks ahead
*   The location is a U.S. state, territory, or the nation as a whole
*   All dates are parsable. If a date is not in yyyy/mm/dd format, the forecast may be dropped.
*   The forecast was made on or before the Monday of the relevant week. If multiple versions of a forecast are submitted then only the last forecast that meets the date restriction is included.

#### **How Hospitalization Forecasts are Processed**
Though hospitalizations are forecasted on a daily basis, in keeping with the cases and death scoring and plotting, we show the hospitalization scores on a weekly basis in the dashboard. We only look at forecasts for one target day a week (currently Wednesdays), and calculate the weekly horizons accordingly. Hospitalization horizons are calculated in the following manner:
* 2 days ahead: Forecast date is on or before the Monday preceeding the target date (Wednesday)
* 9 days ahead: Forecast date equal to or before 7 days before the Monday preceeding the target date
* 16 days ahead: Forecast date is equal to or before 14 days before the Monday preceeding the target date
* 23 days ahead: Forecast date equal to or before 21 days before the Monday preceeding the target date

#### **Notes on the Data**

*   If a forecast does not include an explicit point estimate, the 0.5 quantile is taken as the point estimate for calculating absolute error.
*   The weighted interval score is only shown for forecasts that have predictions for all quantiles (23 quantiles for deaths and hospitalizations and 7 for cases)
*   Totaling over all states and territories does not include nationwide forecasts. To ensure that values are comparable, these totals also exclude any locations that are absent from any file that was submitted by one of the selected forecasters.
*   For scoring, we include revisions of observed values, which means that the scores for forecasts made in the past can change as our understanding of the ground truth changes.
*   The observed data can also be viewed **'as of'** a certain date, which shows what observed data a forecaster had available
when a past forecast was made (but the forecasts are always scored on the latest revision of the observed data).

#### **Accessing the Data**
The forecasts and scores are available as RDS files and are uploaded weekly to a publicly accessible AWS bucket.  

You can use the url https://forecast-eval.s3.us-east-2.amazonaws.com/ + filename to download
any of the files from the bucket.

For instance: https://forecast-eval.s3.us-east-2.amazonaws.com/score_cards_nation_cases.csv.gz to download scores for nation level case predictions.

The available files are:
* predictions_cards_confirmed_incidence_num.csv.gz (forecasts for cases)
* predictions_cards_deaths_incidence_num.csv.gz (forecasts for deaths)
* predictions_cards_confirmed_admissions_covid_1d.csv.gz (forecasts for hospitalizations)
* score_cards_nation_cases.csv.gz
* score_cards_nation_deaths.csv.gz
* score_cards_state_cases.csv.gz
* score_cards_state_deaths.csv.gz
* score_cards_state_hospitalizations.csv.gz
* score_cards_nation_hospitalizations.csv.gz

You can also connect to AWS and retrieve the data in R. Example of retrieving state cases file:

```
library(aws.s3)
library(data.table)

Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket = tryCatch(
  {
    get_bucket(bucket = 'forecast-eval')
  },
  error = function(e) {
    e
  }
)

stateCases = tryCatch(
  {
    s3read_using(fread, object = "score_cards_state_cases.csv.gz", bucket = s3bucket)
  },
  error = function(e) {
    e
  }
)
```

or using the URL of the file:

```
library(data.table)
stateCases <- fread("https://forecast-eval.s3.us-east-2.amazonaws.com/score_cards_state_cases.csv.gz")
```
  

##### Forecasts with actuals

If you are interested in getting the forecasts paired with the corresponding actual values (if you were e.g. testing different evaluation methods), that can be found in [the Amazon S3 bucket](https://forecast-eval.s3.us-east-2.amazonaws.com/) in 3 zip files.
These files are static, generated using [the aggregation script](https://raw.githubusercontent.com/cmu-delphi/forecast-eval/main/app/assets/forecastsWithActuals.R), and forecast and actual data available on June 12, 2023. The latest forecast date available for each target signal is

* [cases](https://forecast-eval.s3.us-east-2.amazonaws.com/cases.zip): 2023-02-13
* [hospitalizations](https://forecast-eval.s3.us-east-2.amazonaws.com/hospitalizations.zip):
  * 1 week: 2023-06-05
  * 2 week: 2023-06-05
  * 3 week: 2023-06-05
  * 4 week: 2023-06-05
* [deaths](https://forecast-eval.s3.us-east-2.amazonaws.com/deaths.zip): 2023-03-06

If the S3 bucket is down, these files are also available on [Delphi's file-hosting site](https://www.cmu.edu/delphi-web/forecast-eval-scores).
