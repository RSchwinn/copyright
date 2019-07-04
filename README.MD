# 225 Years of Copyright Registrations

This repository provides reproducible data and code for the Tulane Law Review article *225 Years of Copyright Registrations*. To reproduce the tables in the main text, use RStudio to run `step_1_data_prep.R`, then compile the `main_text_results.Rmd` file using the RStudio's `knit` command.

OLS and Cochrane-Orcutt results are easily obtained by modifying the `estimation_technique` argument of the `make_tables` function in the `step_2_data_analysis.R` file.

If, after viewing the comments in the code, you have any questions, please direct your questions to Zvi Rosen (zvi @ zvirosen.com) or Richard Schwinn (richardschwinn @ gmail.com).