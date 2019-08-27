# 225 Years of Copyright Registrations

This repository provides reproducible data and code for the Tulane Law Review article *225 Years of Copyright Registrations*. To reproduce the tables in the main text, use RStudio to run `step_1_data_prep.R`, then compile the `main_text_results.Rmd` file using the RStudio's `knit` command.

OLS and Cochrane-Orcutt results are easily obtained by modifying the `estimation_technique` argument of the `make_tables` function in the `step_2_data_analysis.R` file.

If, after viewing the comments in the code, you have any questions, please direct your questions to Zvi Rosen (zvi @ zvirosen.com) or Richard Schwinn (richardschwinn @ gmail.com).

# In order to reproduce the findings in the main paper, please do the following
 
1. Download the repository from this link: 
https://github.com/RSchwinn/copyright/
(You may need to register. Don’t worry, it and everything else related to this project is free.)
 
2. Download and install R, from https://cloud.r-project.org/, and RStudio, from https://www.rstudio.com/products/rstudio/download/#download.
 
3. After both R and RStudio are installed, unzip the repository from step (1), if it was not automatically unzipped, and open the file copyright.Rproj file.
 
4. From the “Files” panel, select and open step_1_data_prep.R and run each line of code in the file (in order). To run a line of code, put the cursor on a given line and press the RUN button.
 
5. Next, select and open step_2_data_analysis.R and run each line of code in the file.

6. Finally, open main_text_results.Rmd and press the KNIT button. There is a ball of yarn next to the KNIT button.
