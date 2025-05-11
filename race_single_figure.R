'%!in%' <- function(x,y)!('%in%'(x,y))
library('tidyverse')
library('patchwork')

# race 
mont_race <- readxl::read_excel("montco_materials/mont_co_clean.xlsx", sheet = "race") |> 
  filter(race != "One Race") |> mutate(race2 = case_when(
    race == "White"~"White",
    race == "Black or African American"~"Black or AA",
    race == "Asian"~"Asian",
    race == "Native Hawaiian and Other Pacific Islander"~"NHPI",
    race == "American Indian and Alaska Native" ~ "AIAN",
    race == "Some Other Race" ~ "Other",
    race == "Two or More Races" ~ "Other",
    race == "Hispanic or Latino (of any race)" ~ "Hispanic/Latino"),
    source = "County Government") |> 
  filter(race2 != "Other") |> select(percent, race2, source)
head(mont_race)

acs_race <- read.csv("mont_acs_race.csv")[-1] 
acs_race <- acs_race |> 
  mutate(race2 = case_when(
  variable == "white"~"White",
  variable == "black"~"Black or AA",
  variable == "asian"~"Asian",
  variable == "nhpi"~"NHPI",
  variable == "native_aa" ~ "AIAN",
  variable == "latino" ~ "Hispanic/Latino"),
  source = "acs") |> filter(is.na(race2) == FALSE) |> 
  select(race2, pct_race, source) |> rename(percent = pct_race)
head(acs_race)
# create other category 
# acs_race[7,]$race2 <- "Other"
# acs_race[7,]$pct_race <- 0
# acs_race[7,]$pct_race <- 1- sum(acs_race$pct_race)

## join and plot 
df <- rbind(acs_race, mont_race)
head(df, 100)

df$race2 <- factor(df$race2, 
               levels=c("White", "Black or AA", "Hispanic/Latino","Asian", "NHPI","AIAN"))

(
  race_f1 <- ggplot(df, aes(x = race2, y = percent, fill = source)) + 
    geom_bar(position = position_dodge(width = .9), stat = "identity") +
    scale_fill_manual(values=c("black", "grey")) +
    geom_text(aes(x = race2, y = percent, group = source,
                  label = scales::percent(percent, accuracy = 1)), 
              position=position_dodge(width=.9), vjust = -.5)+
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.25,.5,.75, 1), limits = c(0,.85)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x = element_text(angle = 30, vjust = .6, hjust=.5),
          text=element_text(size=14,  family="Times"))   
)

cowplot::save_plot(filename = "mont_race_total_single.png", plot = race_f1 , nrow = 1, ncol = 1, dpi = 1500)
