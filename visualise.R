





ggplot(nl2018[nl2018$yearsplit==TRUE,], aes(x= factor(year))) + 
  geom_dotplot(binwidth=1, method="histodot", dotsize = 0.8, stackgroups = TRUE) + 
  theme(legend.position = "bottom", 
        title = element_blank(),
        panel.background=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank())


