### Forecast Evaluation Dashboard

This dashboard was developed by:

* Jed Grabman (Delphi Group, Google Fellow)
* Kate Harwood (Delphi Group, Google Fellow)
* Chris Scott (Delphi Group, Google Fellow)

with the Forecast Evaluation Research Collaborative:

* Nicholas Reich (Reich Lab)
* Jacob Bien (Delphi Group)
* Logan Brooks (Delphi Group)
* Estee Cramer (Reich Lab)
* Daniel McDonald (Delphi Group)

Forecast data in all states and U.S. territories are supplied by:

[![COVID-19 Forecast Hub](forecast-hub-logo.png)](https://covid19forecasthub.org)

### Forecast Evaluation Research Collaborative

The Forecast Evaluation Research Collaborative was founded by:

* Carnegie Mellon University [Delphi Group](https://delphi.cmu.edu)
* UMass-Amherst [Reich Lab](https://reichlab.io/)

Both groups are funded by the CDC as Centers of Excellence for Influenza and COVID-19 Forecasting. We have partnered together on this project to focus on providing a robust set of tools and methods for evaluating the performance of epidemic forecasts.  
  
The collaborative’s mission is to help epidemiological researchers gain insights into the performance of their forecasts and lead to more accurate forecasting of epidemics.  
  
Both groups lead initiatives related to COVID-19 data and forecast curation. The Reich Lab created and maintains the [COVID-19 Forecast Hub](https://covid19forecasthub.org/), a collaborative effort with over 80 groups submitting forecasts to be part of the official [CDC COVID-19 ensemble forecast](https://www.cdc.gov/coronavirus/2019-ncov/covid-data/mathematical-modeling.html). The Delphi Group created and maintains COVIDcast, a platform for [epidemiological surveillance data](https://delphi.cmu.edu/covidcast/), and runs the [Delphi Pandemic Survey via Facebook](https://delphi.cmu.edu/covidcast/surveys/).
  
The Forecaster Evaluation Dashboard is a collaborative project, which has been made possible by the 13 pro bono Google.org Fellows who have spent 6 months working full-time with the Delphi Group. Google.org is [committed](https://www.google.org/covid-19/) to the recovery of lives and communities that have been impacted by COVID-19 and investing in developing the science to mitigate the damage of future pandemics.  
  
### About the Data

#### **Sources**

**Observed values** are from the [COVID-19 Data Repository](https://github.com/CSSEGISandData/COVID-19) by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University.  
  
**Forecaster predictions** are drawn from the [COVID-19 Forecast Hub GitHub repository](https://github.com/reichlab/covid19-forecast-hub/)  
  
Data for the dashboard is pulled from these sources on Mondays and Tuesdays.  

#### **Terms**

*   **Forecaster**:  A named model that produces forecasts, e.g., "COVIDhub-ensemble"
    
*   **Forecast**: A set of quantile predictions for a specific target variable, epidemiological week, and location 
    
*   **Target Variable**: What the forecast is predicting, e.g., “weekly incident cases”
    
*   **Horizon**: The duration of time between when the prediction was made and the predicted event in units of epidemiological weeks.
        
*   **Epidemiological week (MMWR week)**: A standardized week that starts on a Sunday. See the [CDC definition](https://wwwn.cdc.gov/nndss/document/MMWR_week_overview.pdf) for additional details.

*   **Horizon**: The duration of time between when a prediction was made and the end of the corresponding epidemiological week. Following the [Reich Lab definition](https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md#target), a forecast has a horizon of 1 week if it was produced no later than the Monday of the epidemiological week it forecasts. Thus, forecasts made 5-11 days before the end of the corresponding epidemiological week have a horizon of 1 week, 12-18 days before have a horizon of 2 weeks, etc. 
  
#### **Dashboard Inclusion Criteria**
A forecast is only included if all the following criteria are met:

*   The target variable is the weekly incidence of either cases or deaths
*   The horizon is no more than 4 weeks ahead
*   The location is a U.S. state, territory, or the nation as a whole
*   All dates are parsable. If a date is not in yyyy/mm/dd format, the forecast may be dropped.
*   The forecast was made on or before the Monday of the relevant week. If multiple versions of a forecast are submitted then only the last forecast that meets the date restriction is included.

#### **Notes on the Data**

*   If a forecast does not include an explicit point estimate, the 0.5 quantile is taken as the point estimate for calculating absolute error.
*   WIS is only shown for forecasts that have predictions for all quantiles (23 quantiles for deaths and 15 for cases)
*   Totaling over all states and territories does not include nationwide forecasts. To ensure that values are comparable, these totals also exclude any locations that are absent from any file that was submitted by one of the selected forecasters.
*   We include revisions of observed values, which means that the scores for forecasts made in the past can change as our understanding of the ground truth changes.

#### **Accessing the Data**
The forecasts and scores are available as RDS files and are uploaded weekly to a publicly accessible AWS bucket.  

You can use the url https://forecast-eval.s3.us-east-2.amazonaws.com/ + filename to download
any of the files from the bucket. For instance: https://forecast-eval.s3.us-east-2.amazonaws.com/score_cards_nation_cases.rds to download scores for nation level case predictions.  

The available files are:
* predictions_cards.rds (forecasts)
* score_cards_nation_cases.rds
* score_cards_nation_deaths.rds
* score_cards_state_cases.rds
* score_cards_state_deaths.rds
  
  
