'%!in%' <- function(x,y)!('%in%'(x,y))

library('tidycensus')
library('tidyverse')

#check variables
v23 <- load_variables(2023, "acs5", cache = TRUE)
View(v23)

#specify variables and retrieve data
vars <- c(
  total_population = "B01001_001",
  total_child_pop = "B09001_001",
  white = "B03002_003",
  black = "B03002_004",
  native_aa = "B03002_005",
  asian = "B03002_006",
  latino = "B03002_012"
)

mont2023_race <- 
  get_acs(
  geography = "county",
  county = "Montgomery",
  variables = vars,
  state = "PA",
  year = 2023,
  survey = "acs5",
  geometry = FALSE
)

vars2 <- c(
 total_population = "B01001_001",
  male = "B01001_002",
  male_under5 = "B01001_003",
  male_age5_9 = "B01001_004",
  male_10_14 = "B01001_005",
  male_15_17 = "B01001_006",
 male_18_24 = "B01001_007",
  male25_34 = 'B01001_008',
male35_44 = "B01001_009",
male_45_54 = "B01001_010",
male_55_64 = "B01001_011",
male_65_74 = "B01001_012",
male_75_84 = "B01001_013",
male_85_over= "B01001_014",
female = "B01001_015",
female_under5 = "B01001_016",
female_age5_9 = "B01001_017",
female_10_14 = "B01001_018",
female_15_17 = "B01001_019",
female_18_24 = "B01001_020",
female25_34 = 'B01001_021',
female35_44 = "B01001_022",
female_45_54 = "B01001_023",
female_55_64 = "B01001_024",
female_65_74 = "B01001_025",
female_75_84 = "B01001_026",
female_85_over= "B01001_027")

mont2023_age <- 
  get_acs(
    geography = "county",
    county = "Montgomery",
    variables = vars2,
    state = "PA",
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  )

# remove margins of error 
mont2023_age$moe <- NULL
mont2023_race$moe <- NULL

write.csv(mont2023_age, file = "montco_age_acs_2023.csv")

write.csv(mont2023_race, file = "montco_race_acs_2023.csv")

#create percent
race_total <- mont2023_race[mont2023_race$variable == "total_population",]

mrace <- mont2023_race |> filter(variable != "total_population") |> 
  mutate(
  pct_race = estimate / race_total$estimate )

male_total <- mont2023_age[mont2023_age$variable == "male",]
female_total <- mont2023_age[mont2023_age$variable == "female",]

female_age <- mont2023_age |> filter(variable %!in% c("total_population", "male","female"), 
                                   str_detect(variable, pattern = "female"))

male_age <- mont2023_age |> filter(variable %!in% c("total_population", "male","female"), 
                                   variable %!in% female_age$variable) |> 
  mutate(age = parse_number(variable),
         age = case_when(
    variable ==  male_under5 ~  "Under 5",
    variable == male_age5_9 ~ "5 to 9"  ,
    variable == male_10_14 ~ "10 to 14" ,
    variable == male_15_17 ~  "15 to 19",
    variable == male_18_24 ~  "20 to 24" ,
    variable == male25_34 ~ "25 to 34",
    variable == male35_44 ~ "35 to 44",
    variable == male_45_54 ~ "45 to 54",
    variable == male_55_64 ~ "55 to 64",
    variable == male_65_74 ~ "65 to 74",
    variable == male_75_84 ~ "75 to 84",
    variable == male_85_over ~ "85 and over",
  ) )
                                 
Under 5      5 to 9    10 to 14    15 to 19    20 to 24    25 to 29    30 to 34    35 to 39    40 to 44 
0           0           0           1           1           1           1           1           1 
45 to 49    50 to 54    55 to 59    60 to 64    65 to 69    70 to 74    75 to 79    80 to 84 85 and over 