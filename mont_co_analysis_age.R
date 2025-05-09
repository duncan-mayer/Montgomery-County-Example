#load libraries
library('readxl')
library('tidyverse')
library('patchwork')
#bring in data
mont_age <- read_excel("montco_materials/mont_co_clean.xlsx", sheet = "sex_age")


mont_age$age <- gsub(" years","",as.character(mont_age$age))
mont_age$female_ages <- gsub(" years","",as.character(mont_age$female_ages))
mont_age$male_ages <- gsub(" years","",as.character(mont_age$male_ages))



mont_age$age <- factor(mont_age$age, levels=unique(mont_age$age))
mont_age$female_ages <- factor(mont_age$female_ages, levels=unique(mont_age$female_ages))
mont_age$male_ages <- factor(mont_age$male_ages, levels=unique(mont_age$male_ages))
## ages by gender 
apply(mont_age, 2, FUN = class )

mont_age <- mont_age |> filter(count_total != 0) |> 
  mutate(age_grp = case_when(
    age ==  "15 to 19" ~  "15 to 19",
    age == "20 to 24" ~ "20 to 29",
    age == "25 to 29" ~ "20 to 29" ,
    age == "30 to 34" ~  "30 to 44",
    age == "35 to 39" ~  "30 to 44",
    age == "40 to 44" ~  "30 to 44",
    age == "45 to 49" ~  "45 to 64",
    age == "50 to 54" ~  "45 to 64",
    age == "55 to 59" ~  "45 to 64",
    age == "60 to 64" ~  "45 to 64",
    age == "65 to 69" ~  "65 and over",
    age == "70 to 74" ~  "65 and over",
    age == "75 to 79" ~  "65 and over",
    age == "80 to 84" ~  "65 and over",
    age == "85 and over" ~  "65 and over")) |> 
  group_by(age_grp) |> 
  summarise(male_age_grp = sum(count_male), 
                              male_age_grp_pct = sum(pct_male),
            female_age_grp = sum(count_female), 
            female_age_grp_pct = sum(pct_female),
            total_grp = sum(count_total),
            total_pct = sum(pct_of_total))

## female
sum(mont_age$male_age_grp_pct)
sum(mont_age$female_age_grp_pct)

write.csv(mont_age,"mont_ages.csv")
