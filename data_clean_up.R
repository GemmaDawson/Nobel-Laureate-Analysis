library(pacman)
p_load(tidyverse)

# load the data
prizes <- read_csv(file = "data/Nobelprizes.csv")
laureates <- read_csv(file = "data/Nobelarchive.csv")


# The laureates df has errors for the `Birth Date` for row 343 ("Albert John Lutuli") & row 749 ("A. Michael Spence")
# I am not looking at birth year at this syage, so I will not correct this.

# Tidy laureates df
laureates$Category <- tolower(laureates$Category)
colnames(laureates) == 

# Get info on 2017 prizes & tidy
prize17 <- prizes %>% 
  filter(year == 2017) %>% 
  mutate(`Laureate Type` = if_else(condition = is.na(surname), true = "Organization", false = "Individual"),
         `Full Name` = if_else(condition = `Laureate Type` == "Individual", true = str_c(firstname, surname, sep = " "), false = "NA"),
         `Organization Name` = if_else(condition = `Laureate Type` == "Organization", true = firstname, false = "NA"))



nl2018