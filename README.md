
# Overview
This repository includes all code which was used to conduct the analysis in my 
thesis. The 'TVMVP' package has all necessary functions, except the rolling 
window which compares different covariance estimation methods.

In 'mega_rol_pred.R' you find the script for the rolling window evaluation of 
the minimum variance portfolios, and similarly for w/ returns constraint in 
'MVP_w_returns_constr.R', and maximum Sharpe ratio portfolios in 'max_sharpe.R'.

The analysis can be found in 'OMX.R' (MVP), 'OMX_15_19.R' (MVP), 'SP500.R' (MVP),
and 'Empirical_max_SR.R' (Max SR portfolio). Some which are included in the paper,
and some extra which did not get included. Sadly, I cannot share the data that 
was used.

In 'SimulationStudy.R' you find the complete simulation study.

RDS-files with results can be found in the results-folder, simply run 'readRDS("results/which folder/file")'
in order to load the results.

For any questions, please contact me on my student email erik.lillrank.9607@student.uu.se,
or my personal email erik.lillrank@gmail.com.