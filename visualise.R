





# colPal <- c("#d55e00", "#cc79a7", "#0072b2", "#f0e442", "#009e73", "" "#8f8f8f")

colPal <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
            "#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0")
names(colPal) <- c("chemistry", "economics", "literature", "medicine", "peace", "physics", 
                   "chemistryx", "economicsx", "literaturex", "medicinex", "peacex", "physicsx")

yearsplit <- min(nl2019$year) + (max(nl2019$year) - min(nl2019$year))/2
nl2019$yearsplit = if_else(nl2019$year >= yearsplit, TRUE, FALSE)

xLabels <- 
  # seq(from = 1900, to = 2020, by = 10)
c(1901, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2019)

toCreate <- data.frame(graphs = c("category", "colourIndividual", "colourFemale", "colourSTEM", 
                                  "colourFemaleSTEM", "colourAfricanBorn", "colourAfricanBornSTEM",
                                  "colourAfricanBornFemaleSTEM"), subtitle = NA)

awardsTotal <- nl2019 %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

indAwards <- nl2019 %>% 
  filter(`Laureate Type`=="Individual") %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()

orgAwards <- nl2019 %>%
  filter(`Laureate Type`!="Individual") %>%
  group_by(`Organization Name`) %>%
  summarise(n()) %>%
  nrow()

toCreate$subtitle[toCreate$graphs == "category"] <- str_c("Awarded ", awardsTotal, " times to ", indAwards,
                                                           " individuals and ", orgAwards, " organisations.")

totalIndAwards <- nl2019 %>% 
  filter(`Laureate Type`=="Individual") %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

totalIndLaureates <- nl2019 %>% 
  filter(`Laureate Type`=="Individual") %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()

  
toCreate$subtitle[toCreate$graphs == "colourIndividual"] <- str_c("Awarded ", totalIndAwards, " times to ", indAwards,
                                                           " individuals.")

nl2019Female <- nl2019 %>% 
  filter(Sex=="Female")

totalFemaleAwards <- nl2019 %>% 
  filter(Sex=="Female") %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

totalFemaleLaureates <- nl2019 %>% 
  filter(Sex=="Female") %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()


toCreate$subtitle[toCreate$graphs == "colourFemale"] <- str_c("Awarded ", totalFemaleAwards, " times to ", totalFemaleLaureates,
                                                                  " individuals.")

# ++++++++++++++++++++++++++++++

totalSTEMAwards <- nl2019 %>% 
  filter(category %in% c("chemistry", "medicine", "physics")) %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

totalSTEMLaureates <- nl2019 %>% 
  filter(category %in% c("chemistry", "medicine", "physics")) %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()


toCreate$subtitle[toCreate$graphs == "colourSTEM"] <- str_c("Awarded ", totalSTEMAwards, " times to ", totalSTEMLaureates,
                                                              " individuals.")


# ++++++++++++++++++++++++++++++

totalSTEMAwards <- nl2019 %>% 
  filter(category %in% c("chemistry", "medicine", "physics")) %>% 
  filter(Sex=="Female") %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

totalSTEMLaureates <- nl2019 %>% 
  filter(category %in% c("chemistry", "medicine", "physics"))%>% 
  filter(Sex=="Female") %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()


toCreate$subtitle[toCreate$graphs == "colourFemaleSTEM"] <- str_c("Awarded ", totalSTEMAwards, " times to ", totalSTEMLaureates,
                                                            " individuals.")

# "colourAfricanBorn"
# ++++++++++++++++++++++++++++++

totalAFRICAAwards <- nl2019 %>% 
  filter(AfricanBorn == TRUE) %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

totalAFRICALaureates <- nl2019 %>% 
  filter(AfricanBorn == TRUE) %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()


toCreate$subtitle[toCreate$graphs == "colourAfricanBorn"] <- str_c("Awarded ", totalAFRICAAwards, " times to ", totalAFRICALaureates,
                                                                  " individuals.")

# colourAfricanBornSTEM
# ++++++++++++++++++++++++++++++

totalAFRICAAwards <- nl2019 %>% 
  filter(AfricanBorn == TRUE) %>% 
  filter(category %in% c("chemistry", "medicine", "physics")) %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

totalAFRICALaureates <- nl2019 %>% 
  filter(AfricanBorn == TRUE) %>% 
  filter(category %in% c("chemistry", "medicine", "physics")) %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()


toCreate$subtitle[toCreate$graphs == "colourAfricanBornSTEM"] <- str_c("Awarded ", totalAFRICAAwards, " times to ", totalAFRICALaureates,
                                                                   " individuals.")

# colourAfricanBornFemaleSTEM
# ++++++++++++++++++++++++++++++

totalAFRICAAwards <- nl2019 %>% 
  filter(AfricanBorn == TRUE) %>%
  filter(Sex == "Female") %>% 
  filter(category %in% c("chemistry", "medicine", "physics")) %>% 
  group_by(year, category) %>% 
  summarise(n()) %>% 
  nrow()

totalAFRICALaureates <- nl2019 %>% 
  filter(AfricanBorn == TRUE) %>% 
  filter(Sex == "Female") %>% 
  filter(category %in% c("chemistry", "medicine", "physics")) %>% 
  group_by(`Full Name`) %>% 
  summarise(n()) %>% 
  nrow()


toCreate$subtitle[toCreate$graphs == "colourAfricanBornFemaleSTEM"] <- str_c("Awarded ", totalAFRICAAwards, " time to ", totalAFRICALaureates,
                                                                   " individual.")


# +++++++++++++++++++++++++++++++++++++++++++++++++++

for (g in seq_len(length(toCreate$graphs))) {
  
  ind <- which(colnames(nl2019) == toCreate$graphs[g])
  
  nl2019$colCAT <- pull(nl2019[,ind])
  
  myplot <- ggplot(nl2019, aes(x= year, fill = factor(colCAT), color=factor(colCAT))) + 
    geom_dotplot(binwidth=1, method="histodot", dotsize = 0.85, stackgroups = TRUE) + 
    facet_wrap(yearsplit ~ ., scales="free_x", labeller = "label_both", nrow=2) + 
    ggtitle("", subtitle = toCreate$subtitle[g]) +  
    theme(legend.position = "bottom",
          panel.background=element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          strip.text = element_blank(),
          legend.title = element_blank(),
          plot.subtitle = element_text(size = 30)) +
    guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
    scale_x_continuous(breaks=xLabels) + 
    scale_fill_manual(values = colPal, 
                      limits = c("chemistry", "economics", "literature", "medicine", "peace", "physics"), 
                      drop = FALSE) + 
    scale_colour_manual(values= colPal, 
                        limits = c("chemistry", "economics", "literature", "medicine", "peace", "physics"),
                        drop = FALSE)
  ggsave(filename = str_c("../AdaLovelaceDay_ORSSA/ALD2019-Women-In-Superposition/images/", toCreate$graphs[g], ".png"), 
         plot = myplot, device = "png",  height = 22.7, width = 37, units = "cm", dpi = 300)
    # print(myplot)
    # dev.off()
  
}
  
  
# ggplot(nl2019, aes(x= year, fill=factor(category), color=factor(category))) + 
#   geom_dotplot(binwidth=1, method="histodot", dotsize = 0.85, stackgroups = TRUE) + 
#   facet_wrap(yearsplit ~ ., scales="free_x", labeller = "label_both", nrow=2) + 
#   theme(legend.position = "bottom",
#         panel.background=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.line.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         strip.text = element_blank(),
#         legend.title = element_blank()) + 
#   ggtitle("", subtitle = "My subtitle") + 
#   guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
#   scale_x_continuous(breaks=xLabels)
# 
# 
# 
