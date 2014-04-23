# Replication material for:

**It’s Not How Much, but When They Spend:
Political institutions, contingent liabilities, and the costs of responding to financial crises**

*Christopher Gandrud and Mark Hallerberg*

Updated: 23 April 2014

Paper status: The paper is currently a working draft.

## Paper summary

How do political institutions shape the costs of responding to financial crises? Previous research contends that policy-makers in more electorally competitive countries choose policies less costly to taxpayers. In this research note we re-evaluate Keefer’s (2007) contribution published in International Organization using an updated theoretical model and data. We make the novel argument that political institutions shape when politicians spend, more than how much they spend, in response to financial crises. We include the possibility that politicians can shift costs into the future by using policies that create contingent liabilities. Politicians facing removal pressures have incentives to create contingent, rather than immediately realized liabilities. However, the overall costs in the longer-term will not vary substantially between politicians that face strong and weak removal pressures. We illustrate this dynamic by first updating Keefer’s empirical analyses using new data on the fiscal costs of financial crises that includes many of the delayed costs not in the original data set. We further substantiate our argument with Eurostat’s unique yearly, cross-country comparable data on the costs of aiding financial institutions during the late 2000s financial crisis. Trends in this data indicate that politicians in democracies tend to increase contingent liabilities, while also tempering realized liabilities, before elections.

## Replication files

### Data

Files to create the data sets are in the *CrisisDataIssues/DataCreators* directory.

### Figures

The R source code to replicate all of the figures in the paper is in the file:

*FiguresReplicationV1.R*

### Tables

The table and replication analysis of [Keefer (2007)](http://dx.doi.org/10.1017/S0020818307070208) can be recreated with two files:

- *KeeferBasic.do*

- *StataResultsTableMaker.R*

The first is a Stata do-file that runs the analysis. The second takes the output of the analysis and creates the LaTeX formatted table.

The comparative fiscal costs sample table in the Supplementary Materials is created with the file:

*CountrySampleTable.R*
