---
  title: "cpstn"
format: html
---
  
  
  # Load packages ------------------------------------------------------------

# Import data --------------------------------------------------------------

# Data cleaning ------------------------------------------------------------

# Summary statistics -------------------------------------------------------

# Data visualization -------------------------------------------------------

# Model estimation ---------------------------------------------------------

# Export tables and figures ------------------------------------------------
  # Loading 
  
#| message: false
#| warning: false


# Load Packages -----------------------------------------------------------

library(readr)
library(tidyverse)
library(skimr)
library(ggthemes)
library(hrbrthemes)
library(DT)
library(stargazer)
library(broom)
library(sf)
library(tigris)
library(ggplot2)
library(margins)
library(yardstick)
library(WVPlots)
library(pROC)
library(glmnet)
library(gamlr)
library(Matrix)

setwd('/Users/bchoe/My Drive/suny-geneseo/spring2026/nada-cameron')



# Import Data -------------------------------------------------------------

char_vars <- c("CensusTract", "County", "State")

# binary flags (ONLY actual 0/1 variables)
binary_vars <- c(
  "Urban", "GroupQuartersFlag", "LILATracts_1And10",
  "LILATracts_halfAnd10", "LILATracts_1And20",
  "LILATracts_Vehicle", "HUNVFlag", "LowIncomeTracts",
  "LA1and10", "LAhalfand10", "LA1and20",
  "LATracts_half", "LATracts1", "LATracts10",
  "LATracts20", "LATractsVehicle_20"
)

fara2019 <- read_csv("data/Food Access Research Atlas.csv") |>
  filter(State == "New York") |>
  rename(POP2010 = Pop2010) |> 
  mutate(Year = 2019,
         County = sub(" County$", "", County))

fara2019_clean <- fara2019 %>%
  mutate(across(all_of(char_vars), as.character)) %>%
  mutate(across(all_of(binary_vars), ~ factor(., levels = c(0,1)))) %>%
  mutate(across(
    .cols = setdiff(names(.), c(char_vars, binary_vars)),
    ~ as.numeric(.)
  )) 


fara2015 <- read_csv("data/FoodAccessResearchAtlasData2015.csv") |>
  filter(State == "New York") |> 
  mutate(Year = 2015)

fara2015_clean <- fara2015 %>%
  mutate(across(all_of(char_vars), as.character)) %>%
  mutate(across(all_of(binary_vars), ~ factor(., levels = c(0,1)))) %>%
  mutate(across(
    .cols = setdiff(names(.), c(char_vars, binary_vars)),
    ~ as.numeric(.)
  ))


fara_panel <- bind_rows(fara2015_clean, fara2019_clean)

fara_panel <- fara_panel |> 
  mutate(TractLOWI = TractLOWI / POP2010,
         TractKids = TractKids / POP2010,
         TractSeniors = TractSeniors / POP2010,
         TractWhite = TractWhite / POP2010,
         TractBlack = TractBlack / POP2010,
         TractAsian = TractAsian / POP2010,
         TractNHOPI = TractNHOPI / POP2010,
         TractAIAN = TractAIAN / POP2010,
         TractOMultir = TractOMultir / POP2010,
         TractHispanic = TractHispanic /POP2010,
         TractHUNV = TractHUNV / OHU2010,
         TractSNAP = TractSNAP / OHU2010
  )






var_lookup <- read_csv('data/data-dict.csv')

fara2015_sum <- fara2015_clean |> 
  skim()

fara2019_sum <- fara2019_clean |> 
  skim()

fara2019_sum_high_prop_NAs <- fara2019_sum |> 
  filter(complete_rate < .7) |> 
  select(1:4)

var_lookup_high_prop_NAs <- var_lookup |> 
  filter(Field %in% fara2019_sum_high_prop_NAs$skim_variable)

fara_panel_no_high_NA <- fara_panel |> 
  select(-fara2019_sum_high_prop_NAs$skim_variable) |> 
  select(-State) |> 
  drop_na() |> 
  mutate(CensusTract = factor(CensusTract),
         County = factor(County),
         Year = factor(Year)) 


# Data Prep ---------------------------------------------------------------

X <- fara_panel_no_high_NA |> 
  select(-LAhalfand10, -LA1and10, -LA1and20, 
         -LATracts_half, -LATracts1, -LATracts10, -LATracts20,
         -GroupQuartersFlag,
         -starts_with("LI"), -starts_with("LATracts"))

X_matrix <- model.matrix(
  ~ Urban * Year * ( . ) + CensusTract, 
  data=X)[,-1] |> 
  Matrix(sparse = TRUE)

# -LATracts_half, -LATracts1, -LATracts10, -LATracts20,
# -LATracts_half, -LATracts1, -LATracts10
y_halfand10 <- as.integer(fara_panel_no_high_NA$LAhalfand10) - 1
y_1and10 <- as.integer(fara_panel_no_high_NA$LA1and10) - 1
y_1and20 <- as.integer(fara_panel_no_high_NA$LA1and20) - 1

y_Tracts_half <- as.integer(fara_panel_no_high_NA$LATracts_half) - 1
y_Tracts1 <- as.integer(fara_panel_no_high_NA$LATracts1) - 1
y_Tracts10 <- as.integer(fara_panel_no_high_NA$LATracts10) - 1
y_Tracts20 <- as.integer(fara_panel_no_high_NA$LATracts20) - 1

skim(y_Tracts10)
skim(y_Tracts20)


rm(fara_panel,
   fara2015, fara2015_clean, fara2015_sum,
   fara2019, fara2019_clean, fara2019_sum,
   fara2019_sum_high_prop_NAs
)

# Lasso Linear Probability Models -----------------------------------------

set.seed(320)
# Lasso Models
model_halfand10 <- cv.glmnet(
  x         = X_matrix,
  y         = y_halfand10,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)

model_1and10 <- cv.glmnet(
  x         = X_matrix,
  y         = y_1and10,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)

model_1and20 <- cv.glmnet(
  x         = X_matrix,
  y         = y_1and20,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)

model_1and20 <- cv.glmnet(
  x         = X_matrix,
  y         = y_1and20,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)

model_Tracts_half <- cv.glmnet(
  x         = X_matrix,
  y         = y_Tracts_half,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)

model_Tracts1 <- cv.glmnet(
  x         = X_matrix,
  y         = y_Tracts1,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)

# model_Tracts10 <- cv.glmnet(
#   x         = X_matrix,
#   y         = y_Tracts10,   
#   alpha     = 1,
#   # family = "binomial", 
#   # intercept = FALSE 
# )
# model_Tracts20 <- cv.glmnet(
#   x         = X_matrix,
#   y         = y_Tracts20,   
#   alpha     = 1,
#   # family = "binomial", 
#   # intercept = FALSE 
# )


# -LATracts_half, -LATracts1, -LATracts10, -LATracts20,
# -LATracts_half, -LATracts1, -LATracts10

beta_1se_halfand10 <- coef(model_halfand10, s = "lambda.1se")
beta_min_halfand10 <- coef(model_halfand10, s = "lambda.min")
beta_1se_1and10 <- coef(model_1and10, s = "lambda.1se")
beta_min_1and10 <- coef(model_1and10, s = "lambda.min")
beta_1se_1and20 <- coef(model_1and20, s = "lambda.1se")
beta_min_1and20 <- coef(model_1and20, s = "lambda.min")

beta_1se_Tracts_half <- coef(model_Tracts_half, s = "lambda.1se")
beta_min_Tracts_half <- coef(model_Tracts_half, s = "lambda.min")
beta_1se_Tracts1 <- coef(model_Tracts1, s = "lambda.1se")
beta_min_Tracts1 <- coef(model_Tracts1, s = "lambda.min")
# beta_1se_Tracts10 <- coef(model_Tracts10, s = "lambda.1se")
# beta_min_Tracts10 <- coef(model_Tracts10, s = "lambda.min")
# beta_1se_Tracts20 <- coef(model_Tracts20, s = "lambda.1se")
# beta_min_Tracts20 <- coef(model_Tracts20, s = "lambda.min")

betas <- data.frame(
  term = rownames(beta_1se_halfand10),
  
  beta_1se_halfand10 = as.numeric(beta_1se_halfand10),
  beta_min_halfand10 = as.numeric(beta_min_halfand10),
  
  beta_1se_1and10 = as.numeric(beta_1se_1and10),
  beta_min_1and10 = as.numeric(beta_min_1and10),
  
  beta_1se_1and20 = as.numeric(beta_1se_1and20),
  beta_min_1and20 = as.numeric(beta_min_1and20),
  
  beta_1se_Tracts_half = as.numeric(beta_1se_Tracts_half),
  beta_min_Tracts_half = as.numeric(beta_min_Tracts_half),
  
  beta_1se_Tracts1 = as.numeric(beta_1se_Tracts1),
  beta_min_Tracts1 = as.numeric(beta_min_Tracts1)
  
  # beta_1se_Tracts10 = as.numeric(beta_1se_Tracts10),
  # beta_min_Tracts10 = as.numeric(beta_min_Tracts10),
  # 
  # beta_1se_Tracts20 = as.numeric(beta_1se_Tracts20),
  # beta_min_Tracts20 = as.numeric(beta_min_Tracts20)
)

betas_nz <- betas |>
  filter(beta_1se_halfand10 != 0 | beta_min_halfand10 != 0 |
           beta_1se_1and10 != 0 | beta_min_1and10 != 0 |
           beta_1se_1and20 != 0 | beta_min_1and20 != 0 |
           beta_1se_Tracts_half != 0 | beta_min_Tracts_half != 0 |
           beta_1se_Tracts1 != 0 | beta_min_Tracts1 != 0 
  ) |>
  arrange(abs(beta_1se_1and10), abs(beta_min_1and10))


betas_nz |> 
  write_csv("output/betas_nonzero_v5.csv")


# OLS with Selected Variables ---------------------------------------------

clean_term_names <- function(terms) {
  terms %>%
    str_replace_all("Urban0", "Urban") %>%
    str_replace_all("Urban1", "Urban") %>%
    str_replace_all("Year2015", "Year") %>%
    str_replace_all("Year2019", "Year") %>%
    str_replace_all("HUNVFlag0", "HUNVFlag") %>%
    str_replace_all("HUNVFlag1", "HUNVFlag") %>%
    str_replace_all("LowIncomeTracts0", "LowIncomeTracts") %>%
    str_replace_all("LowIncomeTracts1", "LowIncomeTracts") %>%
    str_replace_all("GroupQuartersFlag0", "GroupQuartersFlag") %>%
    str_replace_all("GroupQuartersFlag1", "GroupQuartersFlag") %>%
    unique()
}

get_clean_terms <- function(beta_vec) {
  term_df <- tibble(
    term = rownames(beta_vec),
    beta = as.numeric(beta_vec)
  ) %>%
    filter(beta != 0)
  
  selected_terms <- term_df %>%
    pull(term) %>%
    clean_term_names()
  
  # Keep only interpretable 2-way interactions
  twoway_terms_clean <- selected_terms[
    str_count(selected_terms, ":") == 1 &
      !str_detect(selected_terms, "CensusTract|County")
  ]
  
  # Main effects directly selected by lasso
  main_terms_clean <- selected_terms[
    !str_detect(selected_terms, ":") &
      selected_terms != "(Intercept)" &
      !str_detect(selected_terms, "^CensusTract|^County")
  ]
  
  # Add hierarchical parent terms from selected 2-way interactions
  parent_terms <- twoway_terms_clean %>%
    str_split(":", simplify = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    unlist(use.names = FALSE) %>%
    unique()
  
  parent_terms <- parent_terms[
    parent_terms != "" &
      parent_terms != "(Intercept)" &
      !str_detect(parent_terms, "^CensusTract|^County")
  ]
  
  main_terms_clean <- unique(c(main_terms_clean, parent_terms))
  
  list(
    main_terms_clean = main_terms_clean,
    twoway_terms_clean = twoway_terms_clean
  )
}

terms_halfand10   <- get_clean_terms(beta_1se_halfand10)
terms_1and10      <- get_clean_terms(beta_1se_1and10)
terms_1and20      <- get_clean_terms(beta_1se_1and20)
terms_Tracts_half <- get_clean_terms(beta_1se_Tracts_half)
terms_Tracts1     <- get_clean_terms(beta_1se_Tracts1)







make_formula <- function(y_name, main_terms, twoway_terms) {
  rhs_terms <- unique(c(main_terms, twoway_terms))
  as.formula(
    paste(y_name, "~", paste(rhs_terms, collapse = " + "))
  )
}

form_halfand10 <- make_formula(
  "y_halfand10",
  terms_halfand10$main_terms_clean,
  terms_halfand10$twoway_terms_clean
)

form_1and10 <- make_formula(
  "y_1and10",
  terms_1and10$main_terms_clean,
  terms_1and10$twoway_terms_clean
)

form_1and20 <- make_formula(
  "y_1and20",
  terms_1and20$main_terms_clean,
  terms_1and20$twoway_terms_clean
)

form_Tracts_half <- make_formula(
  "y_Tracts_half",
  terms_Tracts_half$main_terms_clean,
  terms_Tracts_half$twoway_terms_clean
)

form_Tracts1 <- make_formula(
  "y_Tracts1",
  terms_Tracts1$main_terms_clean,
  terms_Tracts1$twoway_terms_clean
)

fara_panel_no_high_NA <- fara_panel_no_high_NA |>
  mutate(Urban = as.numeric(as.character(Urban)))

mod_halfand10   <- lm(form_halfand10, data = fara_panel_no_high_NA)
mod_1and10      <- lm(form_1and10, data = fara_panel_no_high_NA)
mod_1and20      <- lm(form_1and20, data = fara_panel_no_high_NA)
mod_Tracts_half <- lm(form_Tracts_half, data = fara_panel_no_high_NA)
mod_Tracts1     <- lm(form_Tracts1, data = fara_panel_no_high_NA)







library(modelsummary)

modelsummary(
  list(
    "Half & 10" = mod_halfand10,
    "1 & 10" = mod_1and10,
    "1 & 20" = mod_1and20,
    "Tracts Half" = mod_Tracts_half,
    "Tracts 1" = mod_Tracts1
  ),
  stars = TRUE
)




