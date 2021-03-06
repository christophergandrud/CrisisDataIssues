I tested a number of different lag-lead type variables in logistic models with Revisions as the dependent variable. Below are results from three types of independent variable:

- No lag or lead

- Average of three years before a crisis start year.

- Average of three years after a crisis start year.

I also examined Keefer's original 3 year lag-lead variables, but they are basically just a composite of the lag and lead results and are therefore less informative for this exploration. 

I think the *lags* could indicate the effects of the independent variables on policy choices (i.e. do you choose a policy that hides costs) and *leads* could indicate effects on transparency (i.e. do you reveal new costs when they are discovered).

## Data comments

You can download the most recent Stata formatted version of the data from: [https://github.com/christophergandrud/CrisisDataIssues/blob/master/data/KeeferExtended.dta?raw=true](https://github.com/christophergandrud/CrisisDataIssues/blob/master/data/KeeferExtended.dta?raw=true). The dependent variable is called **YearMergeRevise**.

The dependent variable is a dummy equaling 1 if a revision was made in Laeven and Valencia (2012) compared to an earlier data set (Honohan and Klingebiel 2003 or Caprio and Klingebiel 1996) for a given country-crisis and 0 otherwise. A large part of the day yesterday was spent cleaning up this variable. I can go into details if you like. One important note is that to avoid double (or triple) counting individual crises, if a crisis is marked as having different start dates I only included the start date given in Laeven and Valencia (2012). Changing start dates counted as a data revision. For example, earlier data sets mark Mexico's mid-1990s crisis as starting in 1995, but Laeven and Valencia (2012) mark it as starting in 1994. So the Mexico 1995 observations are dropped and 1994 is counted as having a revision. 

Full details of the changes I made are embedded in the code [here](https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/RevisedRevision.R) and lines 188-210 [here](https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/DataCreators/KeeferDataExtender.R).

I only included crises before 2001, as it wouldn't make sense to include crises that aren't included in the pre-Laeven and Valencia (2012) data sets. All models use WEAVE robust standard errors. Results from these are identical to using `vce(cluster country)` in Stata.




## No Lag or Lead

As a baseline these are the results from variables without a lag or a lead. Note: **DiEiec** is the dichotomous electoral competitiveness variable. **stabns** is the political stability variable.


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
  <caption align="bottom" style="margin-top:0.3em;">No Lags or Leads</caption>
  <tr>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 3</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 4</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 5</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 6</b></th>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">(Intercept)</td>
    <td style="padding-right: 12px; border: none;">-1.94<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-0.38</td>
    <td style="padding-right: 12px; border: none;">-2.01<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-1.00</td>
    <td style="padding-right: 12px; border: none;">-2.09<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-11.91<sup style="vertical-align: 4px;">***</sup></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.42)</td>
    <td style="padding-right: 12px; border: none;">(0.36)</td>
    <td style="padding-right: 12px; border: none;">(0.45)</td>
    <td style="padding-right: 12px; border: none;">(0.67)</td>
    <td style="padding-right: 12px; border: none;">(0.46)</td>
    <td style="padding-right: 12px; border: none;">(2.66)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">checks</td>
    <td style="padding-right: 12px; border: none;">0.32<sup style="vertical-align: 4px;">*</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.17</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.17</td>
    <td style="padding-right: 12px; border: none;">-0.09</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.13)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.19)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.20)</td>
    <td style="padding-right: 12px; border: none;">(0.23)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">allhouse</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-1.01<sup style="vertical-align: 4px;">*</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-0.79</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.47)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.50)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">DiEiec</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.71</td>
    <td style="padding-right: 12px; border: none;">0.66</td>
    <td style="padding-right: 12px; border: none;">0.67</td>
    <td style="padding-right: 12px; border: none;">0.37</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.69)</td>
    <td style="padding-right: 12px; border: none;">(0.59)</td>
    <td style="padding-right: 12px; border: none;">(0.70)</td>
    <td style="padding-right: 12px; border: none;">(0.81)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">stabns</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.47</td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.66)</td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">log(GDPperCapita)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 4px;">***</sup></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.32)</td>
  </tr>
  <tr>
    <td style="border-top: 1px solid black;">AIC</td>
    <td style="border-top: 1px solid black;">132.86</td>
    <td style="border-top: 1px solid black;">117.29</td>
    <td style="border-top: 1px solid black;">133.83</td>
    <td style="border-top: 1px solid black;">117.99</td>
    <td style="border-top: 1px solid black;">132.13</td>
    <td style="border-top: 1px solid black;">105.22</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">BIC</td>
    <td style="padding-right: 12px; border: none;">138.43</td>
    <td style="padding-right: 12px; border: none;">122.54</td>
    <td style="padding-right: 12px; border: none;">142.19</td>
    <td style="padding-right: 12px; border: none;">125.87</td>
    <td style="padding-right: 12px; border: none;">143.21</td>
    <td style="padding-right: 12px; border: none;">115.99</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">Log Likelihood</td>
    <td style="padding-right: 12px; border: none;">-64.43</td>
    <td style="padding-right: 12px; border: none;">-56.64</td>
    <td style="padding-right: 12px; border: none;">-63.91</td>
    <td style="padding-right: 12px; border: none;">-56.00</td>
    <td style="padding-right: 12px; border: none;">-62.06</td>
    <td style="padding-right: 12px; border: none;">-48.61</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">Deviance</td>
    <td style="padding-right: 12px; border: none;">128.86</td>
    <td style="padding-right: 12px; border: none;">113.29</td>
    <td style="padding-right: 12px; border: none;">127.83</td>
    <td style="padding-right: 12px; border: none;">111.99</td>
    <td style="padding-right: 12px; border: none;">124.13</td>
    <td style="padding-right: 12px; border: none;">97.22</td>
  </tr>
  <tr>
    <td style="border-bottom: 2px solid black;">Num. obs.</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;" colspan="7"><span style="font-size:0.8em"><sup style="vertical-align: 4px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 4px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 4px;">*</sup>p &lt; 0.05</span></td>
  </tr>
</table>


## Average of 3 years before the crisis start date

The following results use independent variables that are the average value for the three years before the crisis start year. Note that except for the first two models, as in Keefer, **checks** and **allhouse** are the residuals of a regression with **DiEiec** to separate out the unique **checks**/**allhouse** component. **IncomeLag3** is the lag of GDP per Capita.


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
  <caption align="bottom" style="margin-top:0.3em;">Average of Three Years Before a Crisis Starts</caption>
  <tr>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 3</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 4</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 5</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 6</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 7</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 8</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 9</b></th>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">(Intercept)</td>
    <td style="padding-right: 12px; border: none;">-1.96<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-0.23</td>
    <td style="padding-right: 12px; border: none;">-1.20<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-1.01<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-2.19<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-3.50<sup style="vertical-align: 4px;">**</sup></td>
    <td style="padding-right: 12px; border: none;">-2.40<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-12.76<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-12.43<sup style="vertical-align: 4px;">***</sup></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.41)</td>
    <td style="padding-right: 12px; border: none;">(0.41)</td>
    <td style="padding-right: 12px; border: none;">(0.23)</td>
    <td style="padding-right: 12px; border: none;">(0.24)</td>
    <td style="padding-right: 12px; border: none;">(0.49)</td>
    <td style="padding-right: 12px; border: none;">(1.16)</td>
    <td style="padding-right: 12px; border: none;">(0.55)</td>
    <td style="padding-right: 12px; border: none;">(2.87)</td>
    <td style="padding-right: 12px; border: none;">(3.29)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">ChecksLag3</td>
    <td style="padding-right: 12px; border: none;">0.28<sup style="vertical-align: 4px;">*</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.12)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">allhouseLag3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-1.18<sup style="vertical-align: 4px;">*</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.52)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">ChecksResidualsLag3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.87</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-0.44</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-0.49</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.80</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.68)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.79)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.82)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.05)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">allhouseResidualsLag3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">1.04</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-2.51</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.58)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.67)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">DiEiecLag3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">1.59<sup style="vertical-align: 4px;">*</sup></td>
    <td style="padding-right: 12px; border: none;">3.61<sup style="vertical-align: 4px;">*</sup></td>
    <td style="padding-right: 12px; border: none;">1.85<sup style="vertical-align: 4px;">**</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.11</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.63)</td>
    <td style="padding-right: 12px; border: none;">(1.60)</td>
    <td style="padding-right: 12px; border: none;">(0.68)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.02)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">stabnsLag3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-0.11</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.41</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.38)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.74)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">log(IncomeLag3)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">1.36<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">1.31<sup style="vertical-align: 4px;">**</sup></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.33)</td>
    <td style="padding-right: 12px; border: none;">(0.42)</td>
  </tr>
  <tr>
    <td style="border-top: 1px solid black;">AIC</td>
    <td style="border-top: 1px solid black;">116.99</td>
    <td style="border-top: 1px solid black;">104.51</td>
    <td style="border-top: 1px solid black;">120.80</td>
    <td style="border-top: 1px solid black;">106.13</td>
    <td style="border-top: 1px solid black;">116.29</td>
    <td style="border-top: 1px solid black;">103.05</td>
    <td style="border-top: 1px solid black;">107.97</td>
    <td style="border-top: 1px solid black;">84.61</td>
    <td style="border-top: 1px solid black;">79.31</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">BIC</td>
    <td style="padding-right: 12px; border: none;">122.41</td>
    <td style="padding-right: 12px; border: none;">109.56</td>
    <td style="padding-right: 12px; border: none;">126.22</td>
    <td style="padding-right: 12px; border: none;">111.18</td>
    <td style="padding-right: 12px; border: none;">124.42</td>
    <td style="padding-right: 12px; border: none;">110.62</td>
    <td style="padding-right: 12px; border: none;">118.55</td>
    <td style="padding-right: 12px; border: none;">89.80</td>
    <td style="padding-right: 12px; border: none;">91.69</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">Log Likelihood</td>
    <td style="padding-right: 12px; border: none;">-56.49</td>
    <td style="padding-right: 12px; border: none;">-50.26</td>
    <td style="padding-right: 12px; border: none;">-58.40</td>
    <td style="padding-right: 12px; border: none;">-51.07</td>
    <td style="padding-right: 12px; border: none;">-55.15</td>
    <td style="padding-right: 12px; border: none;">-48.52</td>
    <td style="padding-right: 12px; border: none;">-49.98</td>
    <td style="padding-right: 12px; border: none;">-40.30</td>
    <td style="padding-right: 12px; border: none;">-34.65</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">Deviance</td>
    <td style="padding-right: 12px; border: none;">112.99</td>
    <td style="padding-right: 12px; border: none;">100.51</td>
    <td style="padding-right: 12px; border: none;">116.80</td>
    <td style="padding-right: 12px; border: none;">102.13</td>
    <td style="padding-right: 12px; border: none;">110.29</td>
    <td style="padding-right: 12px; border: none;">97.05</td>
    <td style="padding-right: 12px; border: none;">99.97</td>
    <td style="padding-right: 12px; border: none;">80.61</td>
    <td style="padding-right: 12px; border: none;">69.31</td>
  </tr>
  <tr>
    <td style="border-bottom: 2px solid black;">Num. obs.</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;" colspan="10"><span style="font-size:0.8em"><sup style="vertical-align: 4px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 4px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 4px;">*</sup>p &lt; 0.05</span></td>
  </tr>
</table>


## Average of 3 years after the crisis start date

Here are the same models, but the independent variables based on the 3 years following a crisis start year.


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
  <caption align="bottom" style="margin-top:0.3em;">Average of Three Years After a Crisis Starts</caption>
  <tr>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 3</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 4</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 5</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 6</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 7</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 8</b></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 9</b></th>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">(Intercept)</td>
    <td style="padding-right: 12px; border: none;">-2.84<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-0.17</td>
    <td style="padding-right: 12px; border: none;">-1.08<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-1.03<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-3.15<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-4.92<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-3.31<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-11.97<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">-10.84<sup style="vertical-align: 4px;">***</sup></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.53)</td>
    <td style="padding-right: 12px; border: none;">(0.33)</td>
    <td style="padding-right: 12px; border: none;">(0.21)</td>
    <td style="padding-right: 12px; border: none;">(0.23)</td>
    <td style="padding-right: 12px; border: none;">(0.60)</td>
    <td style="padding-right: 12px; border: none;">(1.30)</td>
    <td style="padding-right: 12px; border: none;">(0.65)</td>
    <td style="padding-right: 12px; border: none;">(2.51)</td>
    <td style="padding-right: 12px; border: none;">(2.62)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">ChecksLead3</td>
    <td style="padding-right: 12px; border: none;">0.61<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.15)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">allhouseLead3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-1.58<sup style="vertical-align: 4px;">**</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.50)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">ChecksResidualsLead3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-0.20</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-3.43<sup style="vertical-align: 4px;">**</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-3.29<sup style="vertical-align: 4px;">**</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-1.33</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.70)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.14)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.19)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.31)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">allhouseResidualsLead3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.67</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">-4.23<sup style="vertical-align: 4px;">*</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.62)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.67)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">DiEiecLead3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">3.00<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">5.13<sup style="vertical-align: 4px;">**</sup></td>
    <td style="padding-right: 12px; border: none;">2.80<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.86</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.75)</td>
    <td style="padding-right: 12px; border: none;">(1.63)</td>
    <td style="padding-right: 12px; border: none;">(0.77)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.90)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">stabnsLead3</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">2.01</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">0.84</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.37)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(1.48)</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">log(IncomeLead3)</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 4px;">***</sup></td>
    <td style="padding-right: 12px; border: none;">1.07<sup style="vertical-align: 4px;">**</sup></td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">(0.29)</td>
    <td style="padding-right: 12px; border: none;">(0.33)</td>
  </tr>
  <tr>
    <td style="border-top: 1px solid black;">AIC</td>
    <td style="border-top: 1px solid black;">123.69</td>
    <td style="border-top: 1px solid black;">110.80</td>
    <td style="border-top: 1px solid black;">142.22</td>
    <td style="border-top: 1px solid black;">120.02</td>
    <td style="border-top: 1px solid black;">125.44</td>
    <td style="border-top: 1px solid black;">111.20</td>
    <td style="border-top: 1px solid black;">121.67</td>
    <td style="border-top: 1px solid black;">104.95</td>
    <td style="border-top: 1px solid black;">106.19</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">BIC</td>
    <td style="padding-right: 12px; border: none;">129.30</td>
    <td style="padding-right: 12px; border: none;">116.03</td>
    <td style="padding-right: 12px; border: none;">147.83</td>
    <td style="padding-right: 12px; border: none;">125.25</td>
    <td style="padding-right: 12px; border: none;">133.85</td>
    <td style="padding-right: 12px; border: none;">119.05</td>
    <td style="padding-right: 12px; border: none;">132.72</td>
    <td style="padding-right: 12px; border: none;">110.47</td>
    <td style="padding-right: 12px; border: none;">119.60</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">Log Likelihood</td>
    <td style="padding-right: 12px; border: none;">-59.84</td>
    <td style="padding-right: 12px; border: none;">-53.40</td>
    <td style="padding-right: 12px; border: none;">-69.11</td>
    <td style="padding-right: 12px; border: none;">-58.01</td>
    <td style="padding-right: 12px; border: none;">-59.72</td>
    <td style="padding-right: 12px; border: none;">-52.60</td>
    <td style="padding-right: 12px; border: none;">-56.84</td>
    <td style="padding-right: 12px; border: none;">-50.47</td>
    <td style="padding-right: 12px; border: none;">-48.09</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">Deviance</td>
    <td style="padding-right: 12px; border: none;">119.69</td>
    <td style="padding-right: 12px; border: none;">106.80</td>
    <td style="padding-right: 12px; border: none;">138.22</td>
    <td style="padding-right: 12px; border: none;">116.02</td>
    <td style="padding-right: 12px; border: none;">119.44</td>
    <td style="padding-right: 12px; border: none;">105.20</td>
    <td style="padding-right: 12px; border: none;">113.67</td>
    <td style="padding-right: 12px; border: none;">100.95</td>
    <td style="padding-right: 12px; border: none;">96.19</td>
  </tr>
  <tr>
    <td style="border-bottom: 2px solid black;">Num. obs.</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
    <td style="border-bottom: 2px solid black;">129</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;" colspan="10"><span style="font-size:0.8em"><sup style="vertical-align: 4px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 4px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 4px;">*</sup>p &lt; 0.05</span></td>
  </tr>
</table>

