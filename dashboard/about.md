### Forecast Evaluation Dashboard

This dashboard was developed by

* Jed Grabman (Delphi Group, Google Fellow)
* Kate Harwood (Delphi Group, Google Fellow)
* Chris Scott (Delphi Group, Google Fellow)

with the Forecast Evaluation Research Collaborative by

* Nicholas Reich (Reich Lab)
* Jacob Bien (Delphi Group)
* Logan Brooks (Delphi Group)
* Estee Cramer (Reich Lab)
* Daniel McDonald (Delphi Group)

Forecast data in all states and U.S. territories are supplied by:

[![COVID-19 Forecast Hub](forecast-hub-logo.png)](https://covid19forecasthub.org)

### Forecast Evaluation Research Collaborative

The Forecast Evaluation Research Collaborative was founded:

* Carnegie Mellon University [Delphi Group](https://delphi.cmu.edu)
* UMass-Amherst [Reich Lab](https://reichlab.io/)

Both groups are funded by the CDC as Centers of Excellence for Influenza and COVID-19 Forecasting. We have partnered together on this project to focus on providing a robust set of tools and methods for evaluating the performance of epidemic forecasts.  
  
The collaborative’s mission is to help epidemiological researchers gain insights into the performance of their forecasts, and ultimately lead to more accurate forecasting of epidemics.  
  
Both groups have led initiatives related to COVID-19 data and forecast curation. The Reich Lab has created the [COVID-19 Forecast Hub](https://covid19forecasthub.org/), a collaborative effort with over 80 groups submitting forecasts to be part of the official [CDC COVID-19 ensemble forecast](https://www.cdc.gov/coronavirus/2019-ncov/covid-data/mathematical-modeling.html). The Delphi Group has created COVIDcast, a platform for [epidemiological surveillance data](https://delphi.cmu.edu/covidcast/), and runs the [Delphi Pandemic Survey via Facebook](https://delphi.cmu.edu/covidcast/surveys/), which is a [valuable signal](https://delphi.cmu.edu/blog/2020/09/21/can-symptoms-surveys-improve-covid-19-forecasts/) for Delphi’s participation in the ensemble forecast.  
  
The Forecaster Evaluation Dashboard is a collaborative project, which has been made possible by the 13 pro bono Google.org Fellows who have spent 6 months working full-time with the Delphi Group. Google.org is [committed](https://www.google.org/covid-19/) to the recovery of lives and communities that have been impacted by COVID-19 and investing in developing the science to mitigate the damage of future pandemics.  
  
### About the Data

#### **Sources**

**Observed values** are from the [COVID-19 Data Repository](https://github.com/CSSEGISandData/COVID-19) by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University.  
  
**Forecaster predictions** are drawn from the [COVID-19 Forecast Hub GitHub repository](https://github.com/reichlab/covid19-forecast-hub/)  
  
Data for the dashboard is pulled once a week from these sources, on Tuesdays.  

#### **Terms**

*   **Forecaster**
    
    A model producing quantile predictions
    
*   **Forecast**
    
    A set of data that, for all locales in a geo type, includes predictions for a target variable for each of a certain number of quantiles for each of a certain number of horizons
    
*   **Target Variable**
    
    What the forecast is predicting, ie: “weekly incident cases”
    
*   **Horizon**
    
    The duration of time between when the prediction was made and the predicted event, typically in units of epidemiological weeks.
    
*   **Epidemiological Week (Epi-week)**
    
    Week that starts on a Sunday. If it is Sunday or Monday, the next epi-week is the week that starts on that Sunday (going back a day if it is Monday). If it is Tuesday-Saturday, it is the week that starts on the subsequent Sunday, following [CDC convention](https://wwwn.cdc.gov/nndss/document/MMWR_week_overview.pdf).
    
*   **Point Forecast**
    
    The value that each forecaster picks as their “most likely” prediction. For many forecasters this is the 50th quantile of the predictive distribution, for others it might be the mean of the distribution.
    
*   **Geo Type**
    
    States or U.S. as a nation

#### **Dashboard Inclusion Criteria**

*   Includes only weekly deaths incidence and weekly case incidence target variables
*   Includes only horizon < 5 weeks ahead
*   Includes only geo values that are 2 characters (states / territories / nation)
*   Includes only non-NA target dates (if the date is not in yyyy/mm/dd, the prediction will not be included)
*   Includes only predictions with at least 3 quantile values
*   Includes only one file per forecaster per week (according to forecast date). That file must be from a Sunday or Monday. If both are present, we keep the Monday data.
*   If a forecaster updates a file after that Monday, we do not include the new predictions

#### **Notes on the Data**

*   When totaling over all locations, these locations include states and territories and do not include nationwide forecasts. We only include states and territories common to the selected forecasters (over all time) that have data for at least one location.
*   We do include revisions of observed values, meaning the scores for forecasts made in the past can change. Scores change as our understanding of the truth changes.