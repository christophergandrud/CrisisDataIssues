I tested a number of different lag-lead type variables in logistic models with Revisions as the dependent variable. Below are results from three types:

- No lag or lead

- Average of three years before a crisis start year.

- Average of three years after a crisis starty year.

I also examined Keefer's original 3 year lag-lead variables, but they are basically just a composit of the lag and lead results and are therefore less informative for this exploration.

You can download the most recent Stata formatted version of the data from: [https://github.com/christophergandrud/CrisisDataIssues/blob/master/data/KeeferExtended.dta?raw=true](https://github.com/christophergandrud/CrisisDataIssues/blob/master/data/KeeferExtended.dta?raw=true)

The dependent variable is a dummy equalling 1 if a revision was made in Laeven and Valencia (2012) compared to an earlier data set (Honohan and Klingebiel 2003 or Caprio and Klingebiel 1996) for a given country-crisis and 0 otherwise. A large part of the day yesterday was spent cleaning up this variable. I can go into details if you like. One important note is that to avoid double (or triple) counting individual crises, if a crisis is marked as having different start dates I only included the start date given in Laeven and Valencia (2012). Changing start dates counted as a data revision. For example, earlier data sets mark Mexico's mid-1990s crisis as starting in 1995, but Laeven and Valencia (2012) mark it as starting in 1994. So the Mexico 1995 observations are dropped and 1994 is counted as having a revision. 

Full details of the changes I made are embedded in the code [here](https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/RevisedRevision.R) and lines 180-202 [here](https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/DataCreators/KeeferDataExtender.R).

I only included crises before 2001, as it wouldn't make sense to include crises that aren't included in the pre-Laeven and Valencia (2012) data sets. All models use WEAVE robust standard errors. These are identical to using vce(cluster country) in Stata.




## No Lag or Lead

As a baseline these are the results from variables without a lag or a lead. Note: `DiEiec` is the dichotomous electoral competetiveness variable.


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
  <caption align="bottom" style="margin-top:0.3em;">No Lags or Lead</caption>
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

