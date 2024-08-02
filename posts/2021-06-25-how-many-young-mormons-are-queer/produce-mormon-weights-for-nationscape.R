
# 2021-06-25

# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(haven)
library(labelled)
library(lubridate)

library(survey)

# devtools::install_github("y2analytics/y2clerk") # helper for .sav frequencies
library(y2clerk)

PATH <- str_c(getwd(), "/nationscape/")

# read datasets (exported 2021-06-21) -------------------------------------

# download here: https://www.voterstudygroup.org/publication/nationscape-data-set

files_all <- list.files(path = str_c(PATH, "/DATA"), recursive = T) # get file names
files_sav <- files_all[str_sub(files_all,-3,-1)=="sav"] # now .sav only

responses_full <-
  str_c(PATH,"DATA/",files_sav) %>%
  map_dfr(haven::read_sav)

responses_mormon <- 
  responses_full %>% 
  filter(religion == 3) %>% 
  mutate(
    birth_date = start_date %m-% months(12*age-6), # turning age into birth year
    birth_year = year(birth_date), 
    generation = case_when(
      between(birth_year, 1997, 2012) ~ "Gen_Z",
      between(birth_year, 1981, 1996) ~ "Millennial",
      between(birth_year, 1965, 1980) ~ "Gen_X",
      between(birth_year, 1946, 1964) ~ "Boomer_Silent", # combine boomer and silent
      between(birth_year, #1928, # official Pew cutoff misses a couple older folks
              1900, 1945) ~ "Boomer_Silent",
    )
  ) %>% 
  mutate(
    gender_pew = as_factor(gender),
    census_region_pew = as_factor(census_region),
    age_pew = case_when(
      between(age, 18, 29) ~ "18-29",
      between(age, 30, 49) ~ "30-49",
      between(age, 50, 64) ~ "50-64",
      between(age, 65, 120) ~ "65+",
      T ~ NA_character_
    ) %>% fct_relevel(.,c("18-29", "30-49", "50-64", "65+")),
    race_pew = case_when(
      race_ethnicity == 1 ~ "White",
      race_ethnicity > 1 ~ "Non-white",
      T ~ NA_character_
    ) %>% fct_relevel(., c("White", "Non-white")),
    education_pew = case_when(
      education <= 4 ~ "High school or less",
      education %in% 5:7 ~ "Some college",
      education == 8 ~ "College grad",
      education %in% 9:11 ~ "Post grad",
      T ~ NA_character_
    ) %>% fct_relevel(., c("High school or less", "Some college", "College grad", "Post grad")),
    pid_legacy = pid7_legacy %>% as_factor() %>% as.character(),
    pid_pew = case_when(
      str_detect(pid_legacy, "Democ") ~ "Democrat",
      str_detect(pid_legacy, "Repub") ~ "Republican",
      str_detect(pid_legacy, "Indep") ~ "Independent",
      pid_legacy == "Not Asked" | is.na(pid_legacy) & as.character(pid7) == "3" ~ "Democrat",
      pid_legacy == "Not Asked" | is.na(pid_legacy) & as.character(pid7) == "5" ~ "Republican",
      pid_legacy == "Not Asked" | is.na(pid_legacy) & as.character(pid7) == "4" ~ "Independent",
      T ~ NA_character_
    ) %>% fct_relevel(., c("Democrat", "Republican", "Independent"))
  )

valid_responses <- responses_mormon %>% 
  filter(
    !is.na(census_region_pew) &
      !is.na(age_pew) &
      !is.na(gender_pew) &
      !is.na(race_pew) &
      !is.na(education_pew) &
      !is.na(pid_pew)
  )

invalid_responses <- responses_mormon %>% 
  filter(
    !(responses_mormon$response_id %in% valid_responses$response_id)
  )

comparison <- bind_rows(
  valid_responses %>% 
    select(ends_with("pew")) %>% 
    freqs(digits = 4) %>% 
    mutate(wt = "unweighted"),
  valid_responses %>% 
    select(ends_with("pew"), weight) %>% 
    freqs(wt = weight, digits = 4) %>% 
    mutate(wt = "original_weights")
)

# define targets ----------------------------- --------------------------------

# region ------------------------------------------------------------------
# https://assets.pewresearch.org/wp-content/uploads/sites/11/2012/07/Mormons2revised.gif

valid_responses %>% 
  freqs(census_region_pew, prompt = T)

# unweighted nationscape:
# census_region Northeast   0.06
# census_region Midwest     0.10 
# census_region South       0.20 
# census_region West        0.64

# pew 2009 (+- 4.5%)
# Northeast   0.04
# Midwest     0.07
# South       0.12
# West        0.76

pew_region <- c(0.04, 0.07, 0.12, 0.76)
names(pew_region) <- c("Northeast", "Midwest", "South", "West")

# age ---------------------------------------------------------------------

valid_responses %>% 
  freqs(age_pew, prompt = T)

# unweighted nationscape:
# age_pew 18-29   0.26
# age_pew 30-49   0.40 
# age_pew 50-64   0.21 
# age_pew 65+     0.12

# pew 2014:
# 18-29   0.22
# 30-49   0.40
# 50-64   0.22
# 65+     0.16

pew_age <- c(0.22, 0.4, 0.22, 0.16)
names(pew_age) <- c("18-29", "30-49", "50-64", "65+")

# gender ------------------------------------------------------------------

valid_responses %>% 
  freqs(gender_pew, prompt = T)

# unweighted nationscape:
# gender Female  0.51
# gender Male    0.49

# pew 2014:
# Female  0.56
# Male    0.44

pew_gender <- c(0.56, 0.44)
names(pew_gender) <- c("Female", "Male")

# race --------------------------------------------------------------------

valid_responses %>%
  select(race_pew) %>%
  freqs()

# unweighted nationscape:
# race_pew White       0.82
# race_pew Non-white   0.18

# pew 2014:
# race_pew White       0.85
# race_pew Non-white   0.15

pew_race <- c(0.85, 0.15)
names(pew_race) <- c("White", "Non-white")

# education ---------------------------------------------------------------

valid_responses %>% 
  select(education_pew) %>% 
  freqs()

# unweighted nationscape:
# education_pew High school or less    0.22
# education_pew Some college           0.42
# education_pew College grad           0.22
# education_pew Post grad              0.14

# pew 2014:
# education_pew High school or less    0.27
# education_pew Some college           0.40
# education_pew College grad           0.23
# education_pew Post grad              0.10

pew_educ <- c(0.27, 0.4, 0.23, 0.1)
names(pew_educ) <- c("High school or less", "Some college", "College grad", "Post grad")

# party id ----------------------------------------------------------------

valid_responses %>% 
  select(pid_pew) %>% 
  freqs()

# unweighted nationscape:
# pid_pew Democrat            0.30
# pid_pew Republican          0.60
# pid_pew Independent         0.10

# pew 2018:
# Democrat            0.19
# Republican          0.70
# Independent         0.11

pew_pid <- c(0.19, 0.70, 0.11)
names(pew_pid) <- c("Democrat", "Republican", "Independent")

# produce pop ns ----------------------------- --------------------------------

unweighted_resp <- valid_responses

# region --------------------------------------------------------------

unweighted_resp %>% freqs(census_region_pew)
population_var <- pew_region

value <- names(population_var)
pop_prop <- population_var*nrow(unweighted_resp)

region.pps <- data.frame(value, pop_prop)
names(region.pps) <- c('census_region_pew', 'prop')
region.pps

# age --------------------------------------------------------------

unweighted_resp %>% freqs(age_pew)
population_var <- pew_age

value <- names(population_var)
pop_prop <- population_var*nrow(unweighted_resp)
age.pps <- data.frame(value, pop_prop)
names(age.pps) <- c('age_pew', 'prop')
age.pps

# gender --------------------------------------------------------------

unweighted_resp %>% freqs(gender_pew)
population_var <- pew_gender

value <- names(population_var)
pop_prop <- population_var*nrow(unweighted_resp)
gender.pps <- data.frame(value, pop_prop)
names(gender.pps) <- c('gender_pew', 'prop')
gender.pps

# race --------------------------------------------------------------

unweighted_resp %>% freqs(race_pew)
population_var <- pew_race

value <- names(population_var)
pop_prop <- population_var*nrow(unweighted_resp)

race.pps <- data.frame(value, pop_prop)
names(race.pps) <- c('race_pew', 'prop')
race.pps


# education --------------------------------------------------------------

unweighted_resp %>% freqs(education_pew)
population_var <- pew_educ

value <- names(population_var)
pop_prop <- population_var*nrow(unweighted_resp)

education.pps <- data.frame(value, pop_prop)
names(education.pps) <- c('education_pew', 'prop')
education.pps

# pid --------------------------------------------------------------

unweighted_resp %>% freqs(pid_pew)
population_var <- pew_pid

value <- names(population_var)
pop_prop <- population_var*nrow(unweighted_resp)

pid.pps <- data.frame(value, pop_prop)
names(pid.pps) <- c('pid_pew', 'prop')
pid.pps


# rake ----------------------------- --------------------------------

svy.unweight <- svydesign(
  ids =~ 1, 
  data = unweighted_resp
)

ss.rake <- rake(
  design = svy.unweight,
  sample.margins = list(
    ~census_region_pew,
    ~age_pew,
    ~gender_pew,
    ~race_pew,
    ~education_pew , 
    ~pid_pew
  ),
  population.margins = list(
    region.pps,
    age.pps,
    gender.pps,
    race.pps,
    education.pps , 
    pid.pps
  )
)

unweighted_resp$wts <- weights(ss.rake)
summary(unweighted_resp$wts)

invalid_responses$wts <- 1

weighted_resp <- bind_rows(
  unweighted_resp %>% select(response_id,   final_wt = wts, ends_with("pew")),
  invalid_responses %>% select(response_id, final_wt = wts, ends_with("pew"))
)

nrow(weighted_resp) == nrow(responses_mormon)

weighted_resp$final_wt %>% summary()

# re-do comparison ----------------------------- --------------------------------

responses_mormon_wt <- 
  responses_mormon %>% 
  left_join(
    weighted_resp %>% select(response_id, final_wt), by = "response_id"
  )

wt_freqs <- 
  responses_mormon_wt %>%
  select(ends_with("pew"), final_wt) %>% 
  freqs(wt = final_wt, digits = 4)

final_comparison <- 
  bind_rows(
    comparison,
    wt_freqs %>% mutate(wt = "new_weights")
  ) %>% 
  select(variable, label, result, wt) %>% spread(wt, result) %>% 
  mutate(
    label = fct_relevel(label, c(names(pew_age),names(pew_region),names(pew_educ),names(pew_gender),names(pew_pid),names(pew_race)))
  ) %>% 
  arrange(variable,label) %>% 
  mutate(
    target = c(pew_age, pew_region, pew_educ, pew_gender, pew_pid, NA, pew_race),
  ) %>% 
  arrange(variable) %>% 
  transmute(
    variable,
    level = label,
    unweighted,
    nationscape_wt = original_weights,
    mormon_wt = new_weights,
    population_target = target,
    unweighted_diff_from_target = round(unweighted-target,3),
    nationscape_wt_diff_from_target = round(nationscape_wt-target,3),
    mormon_wt_diff_from_target = round(mormon_wt-target,3)
  )

# check targets
final_comparison %>% view()
  
# test q o i
responses_mormon_wt %>% 
  group_by(generation) %>% 
  freqs(orientation_group, wt = final_wt, nas_group = F, nas=F) %>% view()

# merge back --------------------------------------------------------------

responses_full_new <- 
  responses_full %>% 
  left_join(
    responses_mormon_rewt %>% select(response_id, weight_mormon = final_wt)
  )

# write out resources -----------------------------------------------------

responses_mormon_wt %>% 
  select(response_id, weight_mormon = final_wt) %>% 
  write_csv(
    # str_c(getwd(), "/nationscape/DATA/","mormon_weights.csv")
  )



final_comparison %>% 
  write_csv(
    # str_c(getwd(), "/nationscape/DATA/","mormon_weight_summary.csv")
  )

final_comparison %>% 
  select(contains("diff_from")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(rmse = sqrt(sum(value^2, na.rm = T)/nrow(final_comparison))) %>% 
  transmute(method = c("mormon_wt", "nationscape_wt", "no wt"), rmse) %>% 
  write_csv(
    # str_c(getwd(), "/nationscape/DATA/","mormon_weight_performance.csv")
  )
