# (1) loads packages ----
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
                             character.only = T,
                             dependencies = TRUE)
        }
    }
    invisible(sapply(new_packages, 
                     oneload))
    invisible(sapply(new_packages,
                     require, character.only = T))
}

qload(
      tidyverse,
      readxl,
      ggplot2,
      zoo,
      mFilter,
      openxlsx
    )

# (2) loads data ----
# Loads and preps the data 

# reads the data
df = read_xlsx('copyright_data.xlsx', sheet = 1) 

# renames some codes
df$code = str_replace_all(df$code, "reg\\.", "registrations.") %>%
    str_replace_all("ren\\.", "renewals.") 

# makes a list of the most important codes
original_codes = df %>% 
    dplyr::select(`Field Name`, code) %>%
    dplyr::filter(!is.na(code))

# abbreviates other codes
df$code[is.na(df$code)] = abbreviate(df$`Field Name`[is.na(df$code)]) 
new_codes = df %>% 
    dplyr::select(`Field Name`, code) %>%
    dplyr::filter(!is.na(code))

# transposes data
df = df %>%
    t %>%
    data.frame(stringsAsFactors = F)
    
# copies header to colnames
colnames(df) = df[2, ] 

# removes full names and codes from the data 
df = df[-(1:2), ] %>%
# creates a variable from the rownames
mutate(year = rownames(.))


df = df[-227,]  %>% # removes extra blank year
    dplyr::select(year, everything()) %>%  # arranges year last
    # renames some variables
    mutate(original.total.registrations.post.1790 = registrations.total.w.pages,
        original.total.registrations.post.1870 = registrations.total) %>%
    rename(total.registrations.post.1790 = registrations.total.w.pages,
           total.registrations.post.1870 = registrations.total)

# (3) interpolates missing 1897, 1902, and 1978 data ----
interpolate_function = function(data = df,
                                variables_to_interpolate = c("registrations.class.A"),
                                the_year = 1897){
    years_to_interpolate = ((the_year-1):(the_year+1))
    
    data[(data$year %in% the_year), variables_to_interpolate] = NA
    
    data[(data$year %in% years_to_interpolate), variables_to_interpolate] = 
       round(zoo::na.approx(data[(data$year %in% years_to_interpolate),
                            variables_to_interpolate]))
    return(data)
}

df = interpolate_function(data = df,
                          variables_to_interpolate = c("registrations.class.A",
                                                       "registrations.class.B",
                                                       "registrations.class.CD",
                                                       "registrations.class.E",
                                                       "registrations.class.F",
                                                       "registrations.class.FGHIJK",
                                                       "registrations.class.GHIJK",
                                                       "registrations.class.GHIK",
                                                       "registrations.class.J",
                                                       "total.registrations.post.1870",
                                                       "total.registrations.post.1790"),
                          the_year = 1897)



df = interpolate_function(data = df,
                          variables_to_interpolate = c("registrations.class.A"),
                         the_year = 1902) 


# The 1902 totals are short by the count of class A registrations. Thus, the interpolated 1902 class A registrations are added.
df$total.registrations.post.1790 = as.numeric(df$total.registrations.post.1790)
df$total.registrations.post.1870 = as.numeric(df$total.registrations.post.1870)
df$registrations.class.A = as.numeric(df$registrations.class.A)
df[df$year == 1902, c("total.registrations.post.1790", "total.registrations.post.1870")] = df[df$year == 1902, c("total.registrations.post.1790", "total.registrations.post.1870")] + df[df$year == 1902, "registrations.class.A"]

df = df %>%
    interpolate_function(variables_to_interpolate = c("registrations.class.A",
                                                      "total.registrations.post.1870",
                                                      "total.registrations.post.1790"),
                         the_year = 1978)

# Replaces NAs with zeros
df = df %>% 
    replace(is.na(.), 0)

# (4) updates 2000-2015 based on applications received ----
# During the 2000-2015 era, the copyright office faced problems completing registrations within a year of their receipt. The next several steps replaces post-2000 registrations with a measure based on the number of applications received during each year

# Saves original registrations estimates and rounds earlier estimates. The rounding is only relevant to the years in which totals are recorded as deposits divided by two.
df = df %>% 
    mutate(original.total.registrations.post.1790 = total.registrations.post.1790,
           original.total.registrations.post.1870 = total.registrations.post.1870,
           total.registrations.post.1790 = round(as.numeric(total.registrations.post.1790)),
           total.registrations.post.1870 = round(as.numeric(total.registrations.post.1870)),
           total.registrations.post.1790 = (as.numeric(total.registrations.post.1790)),
           total.registrations.post.1870 = (as.numeric(total.registrations.post.1870))
           )

# Measures the percent of applications received versus those registered during the 15 year period from 1986 through 2000
percent_of_apps_registered = sum(as.numeric(filter(df, year > 1986, year < 2000)$total.registrations.post.1870)) / sum (as.numeric(filter(df, year > 1986, year < 2000)$applications))


df$total.registrations.post.1790[df$year %in% 2001:2015] =
   round(as.numeric(df$applications)[df$year %in% 2001:2015]*percent_of_apps_registered)

df$total.registrations.post.1870[df$year %in% 2001:2015] =
   round(as.numeric(df$applications)[df$year %in% 2001:2015]*percent_of_apps_registered)


# View(df[, c("total.registrations.post.1790", "original.total.registrations.post.1790")])
                
# (5) removes registration counts from the 1790 series for class N starting in 1972 ----
# sound recording applications were specifically rejected in years previous to their adoption in 1972, whereas other forms, such as motion animations, were simply accepted into other categories. Therefore, we remove class N from the 1790 totals.
# 
df = df %>%
    mutate(
        total.registrations.post.1790 =
            as.numeric(total.registrations.post.1790) -
            # J, K, and LM were removed in previous versions of the paper. 
            # The signs and general significance levels of the parameter results are unchanged.
            # as.numeric(registrations.class.J) - 
            # as.numeric(registrations.class.K) -
            # as.numeric(registrations.class.LM) -
            as.numeric(registrations.class.N)
    )

# (6) selects the main variables and creates labelled tables ----

dependent_vars = data.frame(
    rbind(
        c("applications", "Copyright applications received"),
        c("original.total.registrations.post.1790", "Original Counts for Total Registrations including Pages Data 1790+"),
        c("original.total.registrations.post.1870", "Original Counts for Total Registrations 1870+"),
        c("total.registrations.post.1790", "Total Registrations including Pages Data 1790+"),
        c("total.registrations.post.1870", "Total Registrations 1870+"),
        c("registrations.class.A", "Class A Registrations (Books)"),
        c("registrations.class.B", "Class B Registrations (Periodicals)"),
        c("registrations.class.CD", "Registrations for Classes C and D (Dramatic Works and Lectures)"),
        c("registrations.class.E", "Class E Registrations (Music)"),
        c("registrations.class.GHIJK", "Registrations for Classes G-K (Artwork)"),
        c("registrations.class.LM", "Registrations for Classes L and M (Total Motion Pictures)"),
        c("renewals.total", "Total Renewals"),
        c("renewals.class.A", "Class A Renewals (Books)"),
        c("renewals.class.B", "Class B Renewals (Periodicals)"),
        c("renewals.class.CD", "Renewals for Classes C and D (Dramatic Works and Lectures)"),
        c("renewals.class.E", "Class E Renewals (Music)"),
        c("renewals.class.GHIJK", "Renewals for Classes G-K (Artwork)"),
        c("renewals.class.LM", "Renewals for Classes L and M (Total Motion Pictures)"),
        c("renewals.rate.total", "Total Renewal Rate"),
        c("renewals.rate.total.AR", "Total Renewal Rate (for Annual Reports)"),
        c("renewals.rate.A", "Class A Renewals Rate (Books)"),
        c("renewals.rate.B", "Class B Renewals Rate (Periodicals)"),
        c("renewals.rate.CD", "Renewals Rate for Classes C and D (Dramatic Works and Lectures)"),
        c("renewals.rate.E", "Class E Renewals Rate (Music)"),
        c("renewals.rate.GHIJK", "Renewals Rate for Classes G-K (Artwork)"),
        c("renewals.rate.LM", "Renewals Rate for Classes L and M (Total Motion Pictures)"),  
        # c("registrations.class.C", "Class C Registrations (Lectures and other works prepared for oral delivery)"),
        # c("registrations.class.D", "Class D Registrations (Dramatic and Dramatico-musical compositions)"),
        # c("registrations.class.F", "Class F Registrations (Map)"),
        # c("registrations.class.G", "Class G Registrations (Works of Art)"),
        # c("registrations.class.H", "Class H Registrations (Reproductions of works of art)"),
        # c("registrations.class.I", "Class I Registrations (Drawings or plastic works of a scientific or technical character)"),
        # c("registrations.class.J", "Class J Registrations (photographs)"),
        # c("registrations.class.K", "Class K Registrations (prints and pictorial illustrations)"),
        # c("registrations.class.L", "Class L Registrations (Photoplays)"),
        # c("registrations.class.M", "Class M Registrations (Motion pictures other than photoplays)"),
        c("registrations.class.N", "Class N Registrations (Sound Recordings)")#,
        # c("registrations.class.FGHIJK", "Registrations for Classes F-K (Artwork and More)"),
        # c("registrations.class.GHIK", "Registrations for Classes G, H, I, and K (Artwork without Photos)"),
        # c("renewals.class.C", "Class C Renewals (Lectures and other works prepared for oral delivery)"),
        # c("renewals.class.D", "Class D Renewals (Dramatic and Dramatico-musical compositions)"),
        # c("renewals.class.F", "Class F Renewals (Map)"),
        # c("renewals.class.G", "Class G Renewals (Works of Art)"),
        # c("renewals.class.H", "Class H Renewals (Reproductions of works of art)"),
        # c("renewals.class.I", "Class I Renewals (Drawings or plastic works of a scientific or technical character)"),
        # c("renewals.class.J", "Class J Renewals (photographs)"),
        # c("renewals.class.K", "Class K Renewals (prints and pictorial illustrations)"),
        # c("renewals.class.L", "Class L Renewals (Photoplays)"),
        # c("renewals.class.M", "Class M Renewals (Motion pictures other than photoplays)"),
        # c("renewals.class.N", "Class N Renewals (Sound Recordings)"),
        # c("renewals.class.FGHIJK", "Renewals for Classes F-K (Artwork and More)"),
        # c("renewals.class.GHIK", "Renewals for Classes G, H, I, and K (Artwork without Photos)"),
        # c("renewals.rate.C", "Class C Renewal Rates (Lectures and other works prepared for oral delivery)"),
        # c("renewals.rate.D", "Class D Renewal Rates (Dramatic and Dramatico-musical compositions)"),
        # c("renewals.rate.F", "Class F Renewal Rates (Map)"),
        # c("renewals.rate.G", "Class G Renewal Rates (Works of Art)"),
        # c("renewals.rate.H", "Class H Renewal Rates (Reproductions of works of art)"),
        # c("renewals.rate.I", "Class I Renewal Rates (Drawings or plastic works of a scientific or technical character)"),
        # c("renewals.rate.J", "Class J Renewal Rates (photographs)"),
        # c("renewals.rate.K", "Class K Renewal Rates (prints and pictorial illustrations)"),
        # c("renewals.rate.L", "Class L Renewal Rates (Photoplays)"),
        # c("renewals.rate.M", "Class M Renewal Rates (Motion pictures other than photoplays)"),
        # c("renewals.rate.N", "Class N Renewal Rates (Sound Recordings)"))
        # c("renewals.rate.FGHIJK", "Renewal Rate for Classes F-K (Artwork and More)"),
        # c("renewals.rate.GHIK", "Renewal Rate for Classes G, H, I, and K (Artwork without Photos)"),
    ),
    stringsAsFactors = F
)

names(dependent_vars) = c("code", "variables")


independent_vars = data.frame(
    rbind(
        c("registration.fee", "Registration fee"),
        c("renewal.fee", "Renewal fee"),
        c("initial.term", "Initial term of copyright protection"),
        c("renewal.term", "Term of renewals"),
        c("total.term", "Total term of copyright protection"),
        c("real.gdp", "US real\\, GDP_t"),
        c("real.rec.spending", "Real recreation spending"),
        c("gdp.deflator", "US GDP deflator"),
        c("recession", "Recession indicator"),
        c("cpi", "Consumer price index"),
        c("pop", "Real\\, GDP_t"),
        c("renewal.ext.1962", "1_{1962\\,  renewal\\, extension}"),
        c("sound.1972", "1_{1972\\, inclusion\\, of\\, sound\\, recordings}"),
        c("copyright.act.1976", "1_{1976\\,  revision}"),
        c("copyright.act.1909", "1_{1909\\,  revision}"),
        c("copyright.act.1870", "1_{1870\\,  revision}"),
        c("copyright.act.1831", "1_{1831\\,  revision}"),
        c("Berne.1988", "1_{1988\\,  Berne\\, convention}"),
        c("Bono.1998", "1_{1998\\,  Term\\, Extension\\, Act}"),
        c("year", "trend")
    ),
    stringsAsFactors = F
)

names(independent_vars) = c("code", "variables")

main_variables = c(independent_vars$code, dependent_vars$code)
unused_variables = names(df)[!names(df) %in% main_variables]


unused_variables_df = df %>%
    select(unused_variables)

full_df = df
df = df %>%
    select(main_variables) %>%
    mutate(recession = as.logical(recession)) %>%
    mutate_all(as.numeric) %>%
    mutate(auto.renewal.1992 = ifelse(year<1992,0,1))

rownames(df) = df$year 

# (7) recalculates the renewal rates based on the fixed registration statistics ----


# use thist table to confirm process was done correctly
sdf = df
names(sdf) = paste0(names(sdf),"_s")

# this makes a list of names to be recalculated
df = df %>% 
    mutate(
        renewals.rate.total = (100*renewals.total/(lag(total.registrations.post.1870, 28) %>% replace_na(0))),
        # renewals.rate.total.AR = (100*renewals.total.AR/(lag(registrations.total.AR, 28) %>% replace_na(0))),
        renewals.rate.A = (100*renewals.class.A/(lag(registrations.class.A, 28) %>% replace_na(0))),
        renewals.rate.B = (100*renewals.class.B/(lag(registrations.class.B, 28) %>% replace_na(0))),
        renewals.rate.CD = (100*renewals.class.CD/(lag(registrations.class.CD, 28) %>% replace_na(0))),
        renewals.rate.E = (100*renewals.class.E/(lag(registrations.class.E, 28) %>% replace_na(0))),
        renewals.rate.GHIJK = (100*renewals.class.GHIJK/(lag(registrations.class.GHIJK, 28) %>% replace_na(0))),
        renewals.rate.LM = (100*renewals.class.LM/(lag(registrations.class.LM, 28) %>% replace_na(0)))
        )

it = cbind(df[, c(
    "renewals.rate.total",
    "renewals.rate.total.AR",
    "renewals.rate.A",
    "renewals.rate.B",
    "renewals.rate.CD",
    "renewals.rate.E",
    "renewals.rate.GHIJK",
    "renewals.rate.LM"
)],
sdf[, c(
    "renewals.rate.total_s",
    "renewals.rate.total.AR_s",
    "renewals.rate.A_s",
    "renewals.rate.B_s",
    "renewals.rate.CD_s",
    "renewals.rate.E_s",
    "renewals.rate.GHIJK_s",
    "renewals.rate.LM_s"
)])
it = it[, sort(names(it))]

# View(it) to confirm recalculating rates was managed properly

# (8) creates variations on the variables such as by taking logarithms  ----

names_before_gen = names(df)

per100k = function(x){
    10^6*x/pop}

hp_smooth = function(x, part = "trend", start_year=1790, lambda = 6.25) {
    require(mFilter)
    ts_data = ts(x,
                 start = c(as.numeric(start_year), 1),
                 frequency = 1)
    new_net = hpfilter(ts_data, freq = lambda, type = "lambda")
    x = as.numeric(new_net[[part]])
    return(x)
}

df = df %>% 
    mutate( # the mutate function creates new variables from existing ones
        total.registrations.post.1790.per100k = total.registrations.post.1790*10^6/pop, 
        total.registrations.post.1870.per100k = total.registrations.post.1870*10^6/pop,
        registrations.class.A.per100k = registrations.class.A*10^6/pop,
        registrations.class.B.per100k = registrations.class.B*10^6/pop,       
        registrations.class.CD.per100k = registrations.class.CD*10^6/pop,        
        registrations.class.E.per100k = registrations.class.E*10^6/pop,        
        registrations.class.GHIJK.per100k = registrations.class.GHIJK*10^6/pop,
        registrations.class.LM.per100k = registrations.class.LM*10^6/pop,
        real.registration.fee = registration.fee * (100 / cpi),
        real.renewal.fee = renewal.fee * (100 / cpi),
        real.renewal.fee_log = log(real.renewal.fee),
        real.gdp.per100k = real.gdp*10^6/pop,
        real.gdp.trend = hp_smooth(real.gdp),
        real.gdp.cycle = real.gdp/real.gdp.trend
    ) %>%
    mutate_at(c(vars(matches("registration")), 
                vars(matches("real.gdp")), 
                vars(matches("term"))), 
              list(log = log)) %>%
    mutate(
        real.gdp.growth = 100*(real.gdp - lag(real.gdp))/lag(real.gdp),
        real.gdp.growth.lag1 = lag(real.gdp.growth),
        real.gdp.growth.lag2 = lag(real.gdp.growth.lag1),
        real.gdp.growth.lag3 = lag(real.gdp.growth.lag2),
        real.gdp.growth.lag4 = lag(real.gdp.growth.lag3),
        real.gdp_log.x.1909 = real.gdp_log * copyright.act.1909,
        real.gdp.x.1909 = real.gdp * copyright.act.1909,
        real.gdp.trend_log.x.1909 = real.gdp.trend_log*copyright.act.1909,
        total.term2 = total.term^2,
        total.term_log2 = total.term_log^2,
        renewals.total_log = log(renewals.total),
        real.rec.spending_log = log(real.rec.spending),
        registrations_lagged_28 = lag(total.registrations.post.1790, 28),
        registrations_lagged_28_log = log(registrations_lagged_28)
        )

# This changes empty cells to zero
df[is.na(df)] = 0

# This changes -Inf cells to zero
df[df == -Inf] = 0

# This creates variables measuring the expected life od copyright. See Landes and Posner.
df$depreciation.rate = NA
df$depreciation.rate[120:217] = log((df$total.registrations.post.1870[92:189]/df$renewals.total[120:217]))/28

df$expected.life = NA
df$expected.life[120:217] = 1/df$depreciation.rate[120:217]

df$log.expected.life = NA
df$log.expected.life[120:217] = log(df$expected.life[120:217])

#interpolates missing recreation spending_data
df$real.rec.spending[120:141] = zoo::na.approx(df$real.rec.spending[120:141])
df$log.real.rec.spending = log(df$real.rec.spending)

# recreation spending per dollar of GDP
df$rec.percent.of.gdp = df$real.rec.spending/df$real.gdp

df$reg.per.gdp = df$total.registrations.post.1870/df$real.gdp
df$reg.per.cap.gdp = df$total.registrations.post.1870.per100k/df$real.gdp

generated_variables = colnames(df)[!(colnames(df) %in% names_before_gen)]

# (9) saves the wrangled data ----
openxlsx::write.xlsx(df, "copyright_data_ready_for_analysis.xlsx")


# (compare the data to the pre-submission data which were produced under a different set of assumptions) ----
#
# Previously step 4 was skipped and step 5 removed the counts for more classes. 
# alt_df = readRDS("full_analysis/saved_prior.RDS")
# 
# old = round(exp(alt_df$log.reg.total.w.pages))
# new = round((df$total.registrations.post.1790))
# class_N = round(as.numeric(df$registrations.class.N))
# 
# it = data.frame(year = 1790:2015, new, old, difference = (new-old), class_N)
# View(it)
# 