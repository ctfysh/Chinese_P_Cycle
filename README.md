Chinese P cycle
=================================================

The programs depend on R packages: "dplyr", "tidyr", "sqldf" and "triangle". You can install the latest released version from CRAN with:

``` r
install.packages("dplyr")
install.packages("tidyr")
install.packages("sqldf")
install.packages("triangle")
```

1 Data manipulation
-------------------------------------------------
Worksheets in “data.xlsx” are first transformed into three .csv files, namely “d_acts.csv”, “d_pars.csv” and “d_nodes.csv”, respectively. We then use “data.r” to manipulate these raw data and convert them into R data format “data.Rdata”.

2 P flow calculation
-------------------------------------------------
- **P flow modeling**: P flow modelling is conducted by running “pflow.r” with “data.Rdata”. Each P flow is expressed in the form of PF[“N##”, “N##”,], an R array with 3 dimensions, representing a P flow from the source compartment to the sink compartment. Detailed description of all the variables used in this file can be found in the Supplementary Information.

- **P flow aggregation**: The “cflow.r” contains an R function “cf” which transforms the basic P flows into the aggregated 102 P flows.

- **P flow calculation for all figures**: The “fig.r” provides the code for calculating the data that are used in figures in the Manuscript and Supplementary Information based on the results of “pflows.r”.

- **P flow executive codes**: The “run.r” is run with the above codes to get the final outputs of basic P flows, aggregated 102 P flows and P flows used in figures.


3 Uncertainty analysis
-------------------------------------------------
- **Constrained Monte Carlo sampling**: The “cmc.r” contains the functions of constrained Monte Carlo sampling, which are used to sample the structured random variables (a group of positive variables that sum up to 1), the ordered variables (a group of positive variables in order) and the independent variables. Since truncated distribution is used to truncate the sampling values to proper scopes, all these variables can be assumed with any distribution without worrying about sampling out of boundary data (such as being negative).

- **Uncertainty of key P flows in figures**: Uncertainty of key P flows presented in Fig. 2A and Fig. 2E are analyzed using “pflow.r”, “fig2a_2e.r”, a part of “fig.r” that includes the codes of P flow calculation for Fig. 2A and Fig. 2E (for the sake of clarity, we rename the codes “fig2a_2e.r”), and “run_ua_fig.r”, the executive code for uncertainty analysis. This code can distinguish the uncertainty of P flows due to the uncertainty of parameters and activity data. 

- **Uncertainty of 102 P flows**: The full uncertainty of 102 P flows due to all activity data and parameters are analyzed using “pflow.r”, “cflow.r” and “run_ua_cf.r”, the executive code providing the statistical descriptions of all 102 P flows in 1600-2012. 

- **Result combination**: As results of the above uncertainty analysis are first saved as individual R data format databases named as "fig_x_x.Rdata" or "cf_x.Rdata" for the sake of computation convenience, we thus combine these data with "res_bind.r" to present the final results. The uncertainty estimates of key P flows are shown in Fig. S4 and Fig. 2E, respectively. The single-point estimates and uncertainties of the annual 102 P flows (means, medians, SDs, CVs, 5th, 25th, 75th and 95th percentiles) are all shown in the file “flow102.xlsx”.

