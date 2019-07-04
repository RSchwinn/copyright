### 
### Note:
###      The overall sumary statistics, such as R-squared, are not displayed in the
###      combined tables. Please use the manual appraoch to find these 
###      estimates.
### 

# loads packages and data ----
# Installs all required packages 

qload = function(..., char, repo = "http://cran.us.r-project.org"){
    if (!missing(char)) {
        new_packages <- char
    } else {
        new_packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    }
    oneload = function (x){
        if(!(x %in% installed.packages(.Library))){
            install.packages(x,
                             repos = repo,
                             # dependencies = T,
                             character.only = T)
        }
    }
    invisible(sapply(new_packages, 
                     oneload))
    invisible(sapply(new_packages,
                     require, character.only = T))
}

qload(
    tidyverse,
    orcutt,
    prais,
    ggplot2,
    zoo,
    mFilter,
    openxlsx,
    broom,
    flextable,
    pander,
    huxtable,
    jtools
)

df = openxlsx::read.xlsx("copyright_data_ready_for_analysis.xlsx")
rownames(df) = df$year

# formulas for regressions in main text ----
t3_formulas = c("total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log ", 
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log + 
                copyright.act.1831", 
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log +
                real.gdp_log + 
                copyright.act.1831 + 
                copyright.act.1870 ",
                
                "total.registrations.post.1790_log ~
                real.registration.fee_log + 
                real.gdp_log +
                copyright.act.1831 + 
                copyright.act.1870 + 
                copyright.act.1909 ", 
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log + 
                copyright.act.1831 + 
                copyright.act.1870 + 
                copyright.act.1909 + 
                copyright.act.1976", 
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log + 
                copyright.act.1831 +
                copyright.act.1870 + 
                copyright.act.1909 + 
                copyright.act.1976 + 
                Berne.1988" ) 


t4_formulas = c("total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log + 
                copyright.act.1831",
                
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log +
                real.gdp_log + 
                copyright.act.1831 +
                total.term_log +
                total.term_log2",
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log + 
                total.term_log + 
                total.term_log2" ) 

t5_formulas = c("total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log ",
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log +
                total.term_log",
                
                "total.registrations.post.1790_log ~
                real.registration.fee_log + 
                real.gdp_log + 
                total.term_log +
                total.term_log2",
                
                "total.registrations.post.1790_log ~ 
                real.registration.fee_log + 
                real.gdp_log + 
                total.term_log + 
                total.term_log2 + 
                real.gdp_log.x.1909 +
                copyright.act.1909" ) 



t6_formulas = c("total.registrations.post.1790.per100k_log ~ 
                real.registration.fee_log + 
                real.gdp_log ",
                
                "total.registrations.post.1790.per100k_log ~ 
                real.registration.fee_log + 
                real.gdp_log +
                total.term_log",
                
                "total.registrations.post.1790.per100k_log ~
                real.registration.fee_log + 
                real.gdp_log + 
                total.term_log +
                total.term_log2",
                
                "total.registrations.post.1790.per100k_log ~ 
                real.registration.fee_log + 
                real.gdp_log + 
                total.term_log + 
                total.term_log2 + 
                real.gdp_log.x.1909 +
                copyright.act.1909" ) 


t7_formulas = c("renewals.total_log ~ 
                real.renewal.fee_log  +
                real.rec.spending_log + 
                registrations_lagged_28_log
                ",
                
                "renewals.total_log ~ 
                real.renewal.fee_log  +
                real.rec.spending_log + 
                registrations_lagged_28_log + 
                year
                ",
                
                "renewals.total_log ~ 
                real.renewal.fee_log  +
                real.rec.spending_log + 
                registrations_lagged_28_log +
                renewal.ext.1962 +
                auto.renewal.1992",
                
                "renewals.total_log ~ 
                real.renewal.fee_log  +
                real.rec.spending_log + 
                registrations_lagged_28_log +
                year +
                renewal.ext.1962 +
                auto.renewal.1992"
                )

# functions for displaying regression results ----
make_tidy = function(x){
    x = summary(x)
    class(x) = "summary.lm"
    return(x)
} 

make_tables = function(formulas_vector = t3_formulas,
                       estimation_technique = "lm",
                       dataframe = df,
                       min_year = -Inf,
                       max_year = Inf,
                       # std.error, statistic, p.value 
                       error_format = "statistic" 
                       ){
    require(huxtable)
    
    dataframe = dataframe %>%
        filter(year > (min_year - 1), year < (max_year + 1))
    
    results = list()
    adj_R_squared = c()
    durbin_watson = c()
    
    results_string = ""
    make_tidy = function(x){
        x = summary(x)
        class(x) = "summary.lm"
        # class(x) = "lm"
        # tidy(x)
        return(x)
    }
    
    if(estimation_technique == "lm"){
        for(i in seq_along(formulas_vector)){
            results[[i]] = lm(formulas_vector[i], data = dataframe) %>% make_tidy
            results_string = paste0(c(results_string, ", results[[", i, "]]"), collapse = "") %>%
                gsub("^, ", "", .)
        }
        results_string
        table_command = paste0("the_table = huxreg(", results_string, ", error_format = '({", error_format, "})' )")
        eval(parse(text = table_command ))
    }
    
    if(estimation_technique == "prais"){
        for(i in seq_along(formulas_vector)){
            
            # runs prais-winsten analysis
            temp = prais::prais_winsten(formulas_vector[i], 
                                        data = dataframe)
            
            # calculates adjusted R squared
            y = temp$model[,1]
            y_hat = temp$fitted.values
            parameters = ncol(temp$model)-1
            N = nrow(temp$model)
            R2 = 1-(sum((y_hat - y)^2))/sum((y-mean(y))^2)
            adj_R_squared[i] = 1-(1-R2)*((N-1)/(N-(parameters)-1))
            
            # adds results to the results list
            results[[i]] = temp %>% make_tidy
            
            # retrieves durbin-watson statistic
            durbin_watson[i] = results[[i]]$dw[2]
            
            
            # adds to a string that will call the results
            results_string = paste0(c(results_string, ", results[[", i, "]]"), collapse = "") %>%
                gsub("^, ", "", .)
        }
        results_string
        table_command = paste0("the_table = huxreg(", results_string, ", error_format = '({", error_format, "})', statistics = c(N = 'nobs') )")
        eval(parse(text = table_command ))
        
        the_table = rbind(the_table[1:(nrow(the_table)-2),],
                          c("Durbin-Watson", durbin_watson),
                          c("adj. R-squared", adj_R_squared),
                          the_table[(nrow(the_table)-1):nrow(the_table),])
    }
    
    if(estimation_technique == "orcutt"){
        for(i in seq_along(formulas_vector)){
            results[[i]] = orcutt::cochrane.orcutt(lm(formulas_vector[i], data = dataframe)) %>% make_tidy
            results_string = paste0(c(results_string, ", results[[", i, "]]"), collapse = "") %>%
                gsub("^, ", "", .)
        }
        results_string
        table_command = paste0("the_table = huxreg(", results_string, ", error_format = '({", error_format, "})' )")
        eval(parse(text = table_command ))
        
        the_table
    }
    return(the_table)
}

# VIEW combined results tables from main paper ----

make_tables(t3_formulas, estimation_technique = "prais") # Use "lm" for OLS
make_tables(t4_formulas, estimation_technique = "prais") # Or use "orcutt" for cochrane-orcutt
make_tables(t5_formulas, estimation_technique = "prais")
make_tables(t6_formulas, estimation_technique = "prais")
make_tables(t7_formulas, estimation_technique = "prais",
            min_year = 1909,
            max_year = 2006)


# Table 3 Estimations (Manual)  ----
t3_formulas

t3_1 = prais_winsten(t3_formulas[1], df)
# produce summaries as follows:
t3_2 = prais_winsten(t3_formulas[2], df) 
t3_3 = prais_winsten(t3_formulas[3], df) 
t3_4 = prais_winsten(t3_formulas[4], df) 
t3_5 = prais_winsten(t3_formulas[5], df) 
t3_6 = prais_winsten(t3_formulas[6], df) 


# Table 4 Estimations (Manual)  ----
t4_formulas

t4_1 = prais_winsten(t4_formulas[1], df) 
t4_2 = prais_winsten(t4_formulas[2], df) 
t4_3 = prais_winsten(t4_formulas[3], df)

# Table 5 Estimations (Manual)  ----
t5_formulas 

t5_1 = prais_winsten(t5_formulas[1], df) 
t5_2 = prais_winsten(t5_formulas[2], df) 
t5_3 = prais_winsten(t5_formulas[3], df) 
t5_4 = prais_winsten(t5_formulas[4], df) 

# Table 6 Estimations (Manual)  ----
t6_formulas 

t6_1 = prais_winsten(t6_formulas[1], df) 
t6_2 = prais_winsten(t6_formulas[2], df) 
t6_3 = prais_winsten(t6_formulas[3], df) 
t6_4 = prais_winsten(t6_formulas[4], df) 

# Table 7 Estimations (Manual)  ----

t7_formulas 

t7_1 = prais_winsten(t7_formulas[1], df) 
t7_2 = prais_winsten(t7_formulas[2], df) 
t7_3 = prais_winsten(t7_formulas[3], df) 
t7_4 = prais_winsten(t7_formulas[4], df) 
