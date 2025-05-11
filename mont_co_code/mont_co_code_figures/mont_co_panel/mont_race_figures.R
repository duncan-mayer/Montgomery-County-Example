'%!in%' <- function(x,y)!('%in%'(x,y))
library('tidyverse')
library('patchwork')
# race 
mont_race <- read_excel("montco_materials/mont_co_clean.xlsx", sheet = "race") |> 
  filter(race != "One Race") |> mutate(race2 = case_when(
    race == "White"~"White",
    race == "Black or African American"~"Black or AA",
    race == "Asian"~"Asian",
    race == "Native Hawaiian and Other Pacific Islander"~"NHPI",
    race == "American Indian and Alaska Native" ~ "AIAN",
    race == "Some Other Race" ~ "Other",
    race == "Two or More Races" ~ "Other",
    race == "Hispanic or Latino (of any race)" ~ "Hispanic/Latino"),
    race2 = factor(race2, 
                   levels=c("White", "Black or AA", "Hispanic/Latino","Asian",
                            "NHPI","AIAN"))) |> 
  filter(race2 != "Other")
head(mont_race)


( 
  mont_total <- ggplot(data = mont_race) + 
    geom_col(aes(x = race2, y = percent), color = "Black", fill= "Black") + 
    geom_text(aes(x = race2, y = percent, 
                  label = scales::percent(percent, accuracy = 1)), nudge_y = .03)+
    labs(x = NULL, y = "Percent (Montgomery Employees)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.25,.5,.75, 1), limits = c(0,1)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = .6, hjust=.5),
          text=element_text(size=14,  family="Times")) 
)

acs_race <- read.csv("mont_acs_race.csv")[-1]
head(acs_race)

acs_race <- acs_race |> mutate(race2 = case_when(
  variable == "white"~"White",
  variable == "black"~"Black or AA",
  variable == "asian"~"Asian",
  variable == "nhpi"~"NHPI",
  variable == "native_aa" ~ "AIAN",
  variable == "latino" ~ "Hispanic/Latino"),
  race2 = factor(race2, 
                 levels=c("White", "Black or AA", "Hispanic/Latino", "Asian",
                          "NHPI","AIAN"))) |> filter(race2 != "Other")

( 
  acs_total <- ggplot(data = acs_race) + 
    geom_col(aes(x = race2, y = pct_race), color = "Black", fill= "Black") + 
    geom_text(aes(x = race2, y = pct_race, 
                  label = scales::percent(pct_race, accuracy = 1)), nudge_y = .03)+
    labs(x = NULL, y = "Percent (ACS)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.25,.5,.75, 1), limits = c(0,1)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x=element_blank(),
          text=element_text(size=14,  family="Times")) 
)

(racefig <-  acs_total/ mont_total)
cowplot::save_plot(filename = "mont_race.png", plot = racefig , nrow = 2, ncol = 1, dpi = 1500)
