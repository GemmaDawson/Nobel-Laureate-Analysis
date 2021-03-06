library(tidyverse)
nl2018 <- read_rds(path = "data/AllNobelLaureates2018.rds")
tail(nl2018)
colnames(nl2018)

nl2018$`Full Name`[(nl2018$year == 2017 & nl2018$category == "peace")] <- "International Campaign to Abolish Nuclear Weapons (ICAN)"
nl2018$`Organization Name`[(nl2018$year == 2017 & nl2018$category == "peace")] <- "International Campaign to Abolish Nuclear Weapons (ICAN)"



# nl2019 <- nl2019 %>% 
#   mutate(name = ifelse(test = `Laureate Type` == "Individual", yes = `Full Name`, no = `Organization Name`),
#          country = ifelse(test = `Laureate Type` == "Individual", yes = `Birth Country`, no = `Organization Country`)) %>% 
#   select(year, category, share, laureate_type = `Laureate Type`, 
#          name, country, sex = Sex, AfricaBorn = AfricanBorn)



nl2019 <- nl2018 %>%
  add_row(year = 2019,
          category = "medicine",
          share = 1,
          `Laureate Type` = "Individual",
          `Full Name` = "William Kaelin Jr.",
          `Birth Country` = "United States of America",
          AfricanBorn = FALSE,
          Sex = "Male",
          `Organization Name` = "",
          `Organization Country` = "",
          yearsplit = TRUE,
          colourIndividual = "medicine",
          colourFemale = "medicinex",
          colourSTEM = "medicinex",
          colourFemaleSTEM = "medicinex",
          colourAfricanBorn = "medicinex",
          colourAfricanBornSTEM = "medicinex") %>%
  add_row(year = 2019,
          category = "medicine",
          share = 3,
          `Laureate Type` = "Individual",
          `Full Name` = "Sir Peter John Ratcliffe",
          `Birth Country` = "United Kingdom",
          AfricanBorn = FALSE,
          Sex = "Male",
          `Organization Name` = "",
          `Organization Country` = "",
          yearsplit = TRUE,
          colourIndividual = "medicine",
          colourFemale = "medicinex",
          colourSTEM = "medicinex",
          colourFemaleSTEM = "medicinex",
          colourAfricanBorn = "medicinex",
          colourAfricanBornSTEM = "medicinex") %>%
  add_row(year = 2019,
          category = "medicine",
          share = 3,
          `Laureate Type` = "Individual",
          `Full Name` = "Gregg Leonard Semenza",
          `Birth Country` = "United States of America",
          AfricanBorn = FALSE,
          Sex = "Male",
          `Organization Name` = "",
          `Organization Country` = "",
          yearsplit = TRUE,
          colourIndividual = "medicine",
          colourFemale = "medicinex",
          colourSTEM = "medicinex",
          colourFemaleSTEM = "medicinex",
          colourAfricanBorn = "medicinex",
          colourAfricanBornSTEM = "medicinex") %>%
  add_row(year = 2019,
          category = "physics",
          share = 1,
          `Laureate Type` = "Individual",
          `Full Name` = "Jim Peebles",
          `Birth Country` = "Canada",
          AfricanBorn = FALSE,
          Sex = "Male",
          `Organization Name` = "",
          `Organization Country` = "",
          yearsplit = TRUE,
          colourIndividual = "physics",
          colourFemale = "physicsx",
          colourSTEM = "physicsx",
          colourFemaleSTEM = "physicsx",
          colourAfricanBorn = "physicsx",
          colourAfricanBornSTEM = "physicsx") %>%
  add_row(year = 2019,
          category = "physics",
          share = 2,
          `Laureate Type` = "Individual",
          `Full Name` = "Michel G.E. Mayor",
          `Birth Country` = "Switzerland",
          AfricanBorn = FALSE,
          Sex = "Male",
          `Organization Name` = "",
          `Organization Country` = "",
          yearsplit = TRUE,
          colourIndividual = "physics",
          colourFemale = "physicsx",
          colourSTEM = "physicsx",
          colourFemaleSTEM = "physicsx",
          colourAfricanBorn = "physicsx",
          colourAfricanBornSTEM = "physicsx") %>%
  add_row(year = 2019,
          category = "physics",
          share = 2,
          `Laureate Type` = "Individual",
          `Full Name` = "Didier Queloz",
          `Birth Country` = "Switzerland",
          AfricanBorn = FALSE,
          Sex = "Male",
          `Organization Name` = "",
          `Organization Country` = "",
          yearsplit = TRUE,
          colourIndividual = "physics",
          colourFemale = "physicsx",
          colourSTEM = "physicsx",
          colourFemaleSTEM = "physicsx",
          colourAfricanBorn = "physicsx",
          colourAfricanBornSTEM = "physicsx") 

nl2019$colourIndividual <- factor(x = nl2019$colourIndividual, 
                                  levels = c("chemistry", "economics", "literature", 
                                             "medicine", "peace", "physics", 
                                             "chemistryx", "economicsx", "literaturex", 
                                             "medicinex", "peacex", "physicsx"))
nl2019$colourFemale <- factor(x = nl2019$colourFemale, 
                              levels = c("chemistry", "economics", "literature", 
                                         "medicine", "peace", "physics", 
                                         "chemistryx", "economicsx", "literaturex", 
                                         "medicinex", "peacex", "physicsx"))        
nl2019$colourSTEM <- factor(x = nl2019$colourSTEM, 
                            levels = c("chemistry", "economics", "literature", 
                                       "medicine", "peace", "physics", 
                                       "chemistryx", "economicsx", "literaturex", 
                                       "medicinex", "peacex", "physicsx"))
nl2019$colourFemaleSTEM <- factor(x = nl2019$colourFemaleSTEM, 
                                  levels = c("chemistry", "economics", "literature", 
                                             "medicine", "peace", "physics", 
                                             "chemistryx", "economicsx", "literaturex", 
                                             "medicinex", "peacex", "physicsx"))
nl2019$colourAfricanBorn <- factor(x = nl2019$colourAfricanBorn, 
                                   levels = c("chemistry", "economics", "literature", 
                                              "medicine", "peace", "physics", 
                                              "chemistryx", "economicsx", "literaturex", 
                                              "medicinex", "peacex", "physicsx"))
nl2019$colourAfricanBornSTEM <- factor(x = nl2019$colourAfricanBornSTEM, 
                                       levels = c("chemistry", "economics", "literature", 
                                                  "medicine", "peace", "physics", 
                                                  "chemistryx", "economicsx", "literaturex", 
                                                  "medicinex", "peacex", "physicsx"))

nl2019$colourAfricanBornFemaleSTEM <- str_c(nl2019$category, "x")


nl2019$colourAfricanBornFemaleSTEM[(nl2019$AfricanBorn == TRUE) & (nl2019$Sex == "Female") & (nl2019$category %in% c("chemistry", "medicine", "physics"))] <- nl2019$category[(nl2019$AfricanBorn == TRUE) & (nl2019$Sex == "Female") & (nl2019$category %in% c("chemistry", "medicine", "physics"))]


# %>%
#   add_row(year = 2019,
#           category = "chemistry",
#           share = 1,
#           `Laureate Type` = "Individual",
#           `Full Name` = "",
#           `Birth Country` = "",
#           AfricanBorn = FALSE,
#           Sex = NA,
#           `Organization Name` = "",
#           `Organization Country` = "",
#           yearsplit = TRUE,
#           colourIndividual = "grey",
#           colourFemale = "grey",
#           colourSTEM = "grey",
#           colourFemaleSTEM = "grey",
#           colourAfricanBorn = "grey",
#           colourAfricanBornSTEM = "grey")%>%
#   add_row(year = 2019,
#           category = "literature",
#           share = 1,
#           `Laureate Type` = "Individual",
#           `Full Name` = "",
#           `Birth Country` = "",
#           AfricanBorn = FALSE,
#           Sex = NA,
#           `Organization Name` = "",
#           `Organization Country` = "",
#           yearsplit = TRUE,
#           colourIndividual = "grey",
#           colourFemale = "grey",
#           colourSTEM = "grey",
#           colourFemaleSTEM = "grey",
#           colourAfricanBorn = "grey",
#           colourAfricanBornSTEM = "grey") %>%
#   add_row(year = 2019,
#           category = "peace",
#           share = 1,
#           `Laureate Type` = "Individual",
#           `Full Name` = "",
#           `Birth Country` = "",
#           AfricanBorn = FALSE,
#           Sex = NA,
#           `Organization Name` = "",
#           `Organization Country` = "",
#           yearsplit = TRUE,
#           colourIndividual = "grey",
#           colourFemale = "grey",
#           colourSTEM = "grey",
#           colourFemaleSTEM = "grey",
#           colourAfricanBorn = "grey",
#           colourAfricanBornSTEM = "grey") %>%
#   add_row(year = 2019,
#           category = "economics",
#           share = 1,
#           `Laureate Type` = "Individual",
#           `Full Name` = "",
#           `Birth Country` = "",
#           AfricanBorn = FALSE,
#           Sex = NA,
#           `Organization Name` = "",
#           `Organization Country` = "",
#           yearsplit = TRUE,
#           colourIndividual = "grey",
#           colourFemale = "grey",
#           colourSTEM = "grey",
#           colourFemaleSTEM = "grey",
#           colourAfricanBorn = "grey",
#           colourAfricanBornSTEM = "grey")