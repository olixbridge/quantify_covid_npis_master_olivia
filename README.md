dlm_run_both_provinces
generates data and store to csv files for both Ontario and Quebec
window_size can be changed


|Filename|	Description|	Last verified by|
| :---: | :---: | :---: |
|change_point.R 	|The progression of Rt in the absence of intervention	|July 15
|annotated_plot_npi_qc.R	|The Rt plot for the two provinces with annotated NPIs	|July 15
|fig_3.1_figure_poisson_regression_get_r_1.5_0.8.Rmd	|Give EpiEstim a fixed Rt and w to see if the algorithm can reconstruct the Rt.	|July 15
|state_space_model_synthetic_rt.R	|The synthetic Rt plot	|July 15
|dlm_run_both_provinces.R| Generates dataset from dlm | June |
|fig_2.2_positivity_each_province.Rmd| positivity plot for each provinces in Canada| June | 
|fig_2.4_cases_vs_testing.Rmd| Actual cases vs. reported cases by Gu's algorithm | June |
		
		
		

Note
1. Please be careful when running the file 'dlm_run_both_provinces.R' as it may rewrite the data in the data_output folder. Currently, the data are generated using 2000 iterations. 
