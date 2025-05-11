# alternative figures for age, this option used a single panel with bars side-by-side
library('tidyverse')
library('viridis')

acs_df <- read.csv("mont_acs_ages.csv")[-1] |> rename(age_grp = age) |> mutate(cat = "ACS") |> 
  select("age_grp","cat","total_pct") |> filter(age_grp != "under 15")

m_df <- read.csv("mont_ages.csv")[-1] |> mutate(cat = "County Government") |> 
  select("age_grp","cat","total_pct")

df <- rbind(acs_df, m_df)
head(df)

(
f1 <- ggplot(df, aes(x = age_grp, y = total_pct, fill = cat)) + 
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  scale_fill_manual(values=c("black", "grey")) +
geom_text(aes(x = age_grp, y = total_pct, group = cat,
              label = scales::percent(total_pct, accuracy = 1)), 
          position=position_dodge(width=.9), vjust = -.5)+
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::percent, 
                     breaks = c(0,.25,.5,.75, 1), limits = c(0,.5)) + 
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = .6, hjust=.5),
        text=element_text(size=14,  family="Times"))   
)

cowplot::save_plot(filename = "mont_age_single_total.png", plot = f1 , nrow = 1, ncol = 1, dpi = 1500)
