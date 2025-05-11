# age figures 
library('tidyverse')
library('patchwork')

mont_age <- read.csv("mont_ages.csv")[-1]

acs_age <- read.csv("mont_acs_ages_2023.csv")[-1] |> filter(age != "under 15")

## mont county figures -
( 
  mont_total <- ggplot(data = mont_age) + 
    geom_col(aes(x = age_grp, y = total_pct), color = "Black", fill= "Black") + 
    geom_text(aes(x = age_grp, y = total_pct, 
                  label = scales::percent(total_pct, accuracy = 1)), nudge_y = .0125)+
    labs(x = NULL, y = "Percent (Montgomery Employees)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.15,.3,.45), limits = c(0,.45)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = .5, hjust=.5),
          text=element_text(size=14,  family="Times")) 
)


( 
  mont_female <- ggplot(data = mont_age) + 
    geom_col(aes(x = age_grp, y = female_age_grp_pct), color = "Black", fill= "Black") + 
    geom_text(aes(x = age_grp, y = female_age_grp_pct, 
                  label = scales::percent(female_age_grp_pct, accuracy = 1)), nudge_y = .0125)+
    labs(x = NULL, y = "Percent (Montgomery Employees)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.15,.3,.45), limits = c(0,.45)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = .5, hjust=.5),
          text=element_text(size=14,  family="Times")) 
)


( 
  mont_male <- ggplot(data = mont_age) + 
    geom_col(aes(x = age_grp, y = male_age_grp_pct), color = "Black", fill= "Black") + 
    geom_text(aes(x = age_grp, y = male_age_grp_pct, 
                  label = scales::percent(male_age_grp_pct, accuracy = 1)), nudge_y = .0125)+
    labs(x = NULL, y = "Percent (Montgomery Employees)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.15,.3,.45), limits = c(0,.45)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = .5, hjust=.5),
          text=element_text(size=14,  family="Times")) 
)
#######
### census figures
######
( 
  acs_total <- ggplot(data = acs_age) + 
    geom_col(aes(x = age, y = total_pct), color = "Black", fill= "Black") + 
    geom_text(aes(x = age, y = total_pct, 
                  label = scales::percent(total_pct, accuracy = 1)), nudge_y = .0125)+
    labs(x = NULL, y = "Percent (ACS)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.15,.3,.45), limits = c(0,.45)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x=element_blank(),
          text=element_text(size=14,  family="Times")) 
)

( 
  acs_male <- ggplot(data = acs_age) + 
    geom_col(aes(x = age, y = male_pct), color = "Black", fill= "Black") + 
    geom_text(aes(x = age, y = male_pct, 
                  label = scales::percent(male_pct, accuracy = 1)), nudge_y = .0125)+
    labs(x = NULL, y = "Percent (ACS)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.15,.3,.45), limits = c(0,.45)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x=element_blank(),
          text=element_text(size=14,  family="Times")) 
)

( 
  acs_female <- ggplot(data = acs_age) + 
    geom_col(aes(x = age, y = female_pct), color = "Black", fill= "Black") + 
    geom_text(aes(x = age, y = female_pct, 
                  label = scales::percent(female_pct, accuracy = 1)), nudge_y = .0125)+
    labs(x = NULL, y = "Percent (ACS)") +
    scale_y_continuous(labels = scales::percent, 
                       breaks = c(0,.15,.3,.45), limits = c(0,.45)) + 
    theme_bw() +
    theme(legend.title = element_blank(), 
          legend.position = "bottom",
          axis.text.x=element_blank(),
          text=element_text(size=14,  family="Times")) 
)


(f1_totals <- acs_total / mont_total)

(f2_male <- acs_male / mont_male)

(f3_female <- acs_female / mont_female)

cowplot::save_plot(filename = "mont_f1.png", plot = f1_totals , nrow = 2, ncol = 1, dpi = 1500)

cowplot::save_plot(filename = "mont_age_male.png", plot = f2_male , nrow = 2, ncol = 1, dpi = 1500)
cowplot::save_plot(filename = "mont_age_female.png", plot = f3_female , nrow = 2, ncol = 1, dpi = 1500)
