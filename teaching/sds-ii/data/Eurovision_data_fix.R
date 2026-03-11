# --- Eurovision Data ---

library(tidyverse)
library(janitor)
library(survival)
library(stringr)
library(lmtest)

# minimal reshape: rows = jury_country, columns = contestants, keep year
reshape_long <- function(df) {
  df %>%
    clean_names() %>%
    select(-total_score, -jury_score, -televoting_score) %>%
    pivot_longer(
      cols = -c(contestant, year),
      names_to  = "jury_country",
      values_to = "points"
    ) %>%
    rename(country = contestant) %>%     # contestant column is really "country"
    relocate(jury_country, year, country, points)
}

# Jury data
filepath <- "/Users/jeppelina/Documents/R/SDS II/Data/Eurovision_data/Final Results/Jury"
years    <- c(2016:2019, 2021:2023)

jury_list <- vector("list", length(years))
names(jury_list) <- years

for (yr in years) {
  file <- file.path(filepath, sprintf("%d_jury_results.csv", yr))
  df   <- read.csv(file, check.names = FALSE) |> as_tibble() %>% mutate(year = yr)
  jury_list[[as.character(yr)]] <- reshape_long(df)
}

jury_df <- bind_rows(jury_list) %>%
  mutate(jury_country = case_when(
           jury_country == "czechia" ~ "czech_republic",
           jury_country == "macedonia" ~ "north_macedonia",
           TRUE ~ jury_country),
         country = str_replace_all(str_to_lower(country), " ", "_")
         ) %>%
  filter(!is.na(jury_country))
  


# Song data
song_df <- read_csv("/Users/jeppelina/Documents/R/SDS II/Data/Eurovision_data/song_data.csv") 
song_df <- song_df %>% filter(year >= 2016 & year != 2020) %>%
  mutate(country = str_replace_all(str_to_lower(country), " ", "_"))



# Country neighbors
neighbours <- read_csv("/Users/jeppelina/Documents/R/SDS II/Data/Eurovision_data/Country_Voting_Pairs.csv")
country_df <- neighbours %>% distinct(from_country,to_country, .keep_all = TRUE)


#save data
write_csv(jury_df, "/Users/jeppelina/Documents/R/SDS II/Data/Eurovision_data/jury_df.csv")
write_csv(song_df, "/Users/jeppelina/Documents/R/SDS II/Data/Eurovision_data/song_df.csv")
write_csv(country_df, "/Users/jeppelina/Documents/R/SDS II/Data/Eurovision_data/country_df.csv")


# Join
df <- left_join(x = song_df,
                y = jury_df,
                by = c("country", "year")) %>%
  filter(jury_country != country) %>% #remove self from choice set
  mutate(twelve = case_when( # Make chosen variable the highest point
      points == 12 ~ 1,
      TRUE ~ 0),
      top_three = case_when(
        points >= 8 ~ 1,
        TRUE ~ 0
      )
  ) %>%
  left_join(
    y = country_df,
    by = c("jury_country" = "from_country", "country" = "to_country")
  ) %>%
  mutate(choiceset = paste0(jury_country, "_", year)) %>% # Make a choiceset variable for each jury
  select(choiceset, jury_country, year, country, twelve, points, neighbours, everything()) # sort variables



# Countries in df$country but not in df$jury_country
setdiff(unique(df$country), unique(df$jury_country))

# Countries in df$jury_country but not in df$country
setdiff(unique(df$jury_country), unique(df$country))


### REMOVE MISSING
df<- df %>%
  drop_na(style, BPM, gender, energy)


# Recode variables
df$style <- relevel(as.factor(df$style), "Pop")

# df <- df %>% group_by(choiceset) %>% mutate( #Center BPM by choice set
#   BPM_c = scale(as.numeric(BPM), center = TRUE, scale = FALSE)
# ) %>%
#   ungroup()

df <- df %>%
  mutate(across(c(energy, danceability, happiness, loudness,
                  acousticness, instrumentalness, liveness, speechiness), as.numeric),
         language_eng = case_when(
           language == "English" ~ 1,
           TRUE ~ 0
         ),
         minor_key = case_when(
           str_detect(key, "Minor") ~ 1,
           TRUE ~ 0
         ),
         one_singer = case_when(
           main_singers == 1 ~ 1,
           TRUE ~ 0
         )
  )


####################
# MODELS
####################

######## 12 POINTS ############
df_clean <- df %>%
  drop_na(style, BPM, gender, energy, minor_key)

m1 <- clogit(twelve ~ style + strata(choiceset), data = df_clean)
summary(m1) 

m2 <- clogit(twelve ~ style + neighbours + strata(choiceset), data = df_clean)
summary(m2) 

m3 <- clogit(twelve ~ style + neighbours + gender + one_singer + energy + strata(choiceset), data = df_clean)
summary(m3)  


# Likelyhood ratio tests
lmtest::lrtest(m1, m2)

######### Top 3 ################
m1_top3 <- clogit(top_three ~ style + strata(choiceset), data = df_clean)
summary(m1_top3) 

m2_top3 <- clogit(top_three ~ style + neighbours + strata(choiceset), data = df_clean)
summary(m2_top3) 

m3_top3 <- clogit(top_three ~ style + neighbours + gender + one_singer + energy + strata(choiceset), data = df_clean)
summary(m3_top3)  

