'%!in%' <- function(x,y)!('%in%'(x,y))

library('tidycensus')
library('tidyverse')

#check variables
v23 <- load_variables(2023, "acs5", cache = TRUE)
#View(v23)

#specify variables and retrieve data
vars <- c(
  total_population = "B01001_001",
  total_child_pop = "B09001_001",
  white = "B03002_003",
  black = "B03002_004",
  native_aa = "B03002_005",
  asian = "B03002_006",
  nhpi = "B03002_007",
  latino = "B03002_012",
  two_or_more_races = "B03002_019"
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


# reference see https://api.census.gov/data/2023/acs/acs1/variables.html
vars2 <- c(
 total_population = "B01001_001",
  male = "B01001_002",
  male_under5 = "B01001_003",
  male_age5_9 = "B01001_004",
  male_10_14 = "B01001_005",
  male_15_17 = "B01001_006",
 male_18_19 = "B01001_007",
  male20 = 'B01001_008',
male21 = "B01001_009",
male_22_24 = "B01001_010",
male_25_29 = "B01001_011",
male_30_34 = "B01001_012",
male_35_39 = "B01001_013",
male_40_44 = "B01001_014",
male_45_49 = "B01001_015",
male_50_54 = "B01001_016",
male_55_59 = "B01001_017",
male_60_61 = "B01001_018",
male_62_64 = "B01001_019",
male_65_66 = "B01001_020",
male_67_69 = "B01001_021",
male_70_74 = "B01001_022",
male_75_79 = "B01001_023",
male_80_84 = "B01001_024",
male_85_over = "B01001_025",
female = "B01001_026",
female_under5 = "B01001_027",
female_age5_9 = "B01001_028",
female_10_14 = "B01001_029",
female_15_17 = "B01001_030",
female_18_19 = "B01001_031",
#female20 = "B01001_0032",
female21 = "B01001_033",
female_22_24 = "B01001_034",
female_25_29 = "B01001_035",
female_30_34 = "B01001_036",
female_35_39 = "B01001_037",
female_40_44 = "B01001_038",
female_45_49 = "B01001_039",
female_50_54 = "B01001_040",
female_55_59 = "B01001_041",
female_60_61 = "B01001_042",
female_62_64 = "B01001_043",
female_65_66 = "B01001_044",
female_67_69 = "B01001_045",
female_70_74 = "B01001_046",
female_75_79 = "B01001_047",
female_80_84 = "B01001_048",
female_85_over = "B01001_049")

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

#write.csv(mont2023_age, file = "montco_age_acs_2023.csv")

#write.csv(mont2023_race, file = "montco_race_acs_2023.csv")

#create percent
race_total <- mont2023_race[mont2023_race$variable == "total_population",]

mrace <- mont2023_race |> filter(variable %!in% c("total_population","total_child_pop")) |> 
  mutate(
  pct_race = estimate / race_total$estimate )

# take totals 
male_total <- mont2023_age[mont2023_age$variable == "male",]
female_total <- mont2023_age[mont2023_age$variable == "female",]

#take female names to use as an index 
female_index <- mont2023_age |> filter(variable %!in% c("total_population", "male","female"), 
                                         str_detect(variable, pattern = "female"))
#recode females
female_age <- mont2023_age |> filter(variable %!in% c("total_population", "male","female"), 
                                   str_detect(variable, pattern = "female"))  |>                
  mutate(age = parse_number(variable),
         age = case_when(
           variable ==  "female_under5" ~  "under 15",
           variable == "female_age5_9" ~ "under 15"  ,
           variable == "female_10_14" ~ "under 15" ,
           variable == "female_15_17" ~  "15 to 19",
           variable == "female_18_19" ~  "15 to 19" ,
           variable == "female20" ~ "20 to 29",
           variable == "female21" ~ "20 to 29",
           variable == "female_22_24" ~ "20 to 29" ,
           variable == "female_25_29" ~ "20 to 29",
           variable == "female_30_34" ~ "30 to 44",
           variable == "female_35_39" ~ "30 to 44",
           variable == "female_40_44" ~ "30 to 44",
           variable == "female_45_49" ~ "45 to 64",
           variable == "female_50_54" ~ "45 to 64",
           variable == "female_55_59" ~ "45 to 64",
           variable == "female_60_61" ~ "45 to 64",
           variable == "female_62_64" ~ "45 to 64",
           variable == "female_65_66" ~ "65 and over",
           variable == "female_67_69" ~ "65 and over",
           variable == "female_70_74" ~ "65 and over",
           variable == "female_75_79" ~ "65 and over",
           variable == "female_80_84" ~ "65 and over",
           variable == "female_85_over" ~ "65 and over"
         )) |> group_by(age) |> summarize(female_grp_count = sum(estimate),
                                          female_pct = female_grp_count / female_total$estimate)
# recode males into groups 
male_age <- mont2023_age |> filter(variable %!in% c("total_population", "male","female"), 
                                   variable %!in% female_index$variable) |> 
  mutate(age = case_when(
variable ==  "male_under5" ~  "under 15",
variable == "male_age5_9" ~ "under 15"  ,
variable == "male_10_14" ~ "under 15" ,
variable == "male_15_17" ~  "15 to 19",
variable == "male_18_19" ~  "15 to 19" ,
variable == "male20" ~ "20 to 29" ,
variable == "male21" ~ "20 to 29" ,
variable == "male_22_24" ~ "20 to 29" ,
variable == "male_25_29" ~ "20 to 29",
variable == "male_30_34" ~ "30 to 44",
variable == "male_35_39" ~ "30 to 44",
variable == "male_40_44" ~ "30 to 44",
variable == "male_45_49" ~ "45 to 65",
variable == "male_50_54" ~ "45 to 65",
variable == "male_55_59" ~ "45 to 65",
variable == "male_60_61" ~ "45 to 65",
variable == "male_62_64" ~ "45 to 65",
variable == "male_65_66" ~ "65 and over",
variable == "male_67_69" ~ "65 and over",
variable == "male_70_74" ~ "65 and over",
variable == "male_75_79" ~ "65 and over",
variable == "male_80_84" ~ "65 and over",
variable == "male_85_over" ~ "65 and over"
)) |> group_by(age) |> 
  summarize(male_grp_count = sum(estimate),
                                 male_pct = male_grp_count / male_total$estimate) 

# join and save
acs_ages <- male_age |> left_join(female_age, by = "age") |> 
  mutate(
    total_count = male_grp_count + female_grp_count,
    total_pct = total_count / sum(total_count)) |> 
  write.csv("mont_acs_ages_2023.csv")

mrace |> write.csv("mont_acs_race.csv")
