library(tidyverse)
library(lubridate)

df <- tibble(
  DateCol = seq(dmy("15/03/2021"),
                 dmy("15/04/2021"),
                 "days"),
  valueCol = runif(32)
  
)

dfPlot <- df %>% 
  mutate(weekday = wday(DateCol, label =T, week_start = 7),
         month = month(DateCol, label = T),
         date = yday(DateCol),
         week = epiweek(DateCol))

dfPlot <- dfPlot %>%
  mutate(Type_Jour = case_when(date <= 76 ~ "jour normal", date == 104 ~ "jour normal", date == 81 ~ "jour normal", date == 77 ~ "Formation", weekday %in% c("Mon", "Wed", "Fri") ~ "Formation", weekday %in% c("Sat", "Sun") ~ "Weekend",   TRUE ~ "jour normal"))

dfPlot <- dfPlot %>% 
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week))


cty_by_var <- function(var) {
  ggplot(dfPlot, aes(weekday,-week, colour = factor({{var}}), fill = factor({{var}}))) +
  geom_tile(colour = "white")  + 
  geom_text(aes(label = day(DateCol)), size = 2.5, color = "black") +
  theme(aspect.ratio = 1/2,
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm"))) +
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Calendrier de Formation")+
    theme(legend.direction = "vertical",
          legend.box = "horizontal",
          legend.position = "bottom")}



discrete_palettes <- list(
  c("#EE5859", "#58585A", "grey"),
  RColorBrewer::brewer.pal(6, "Accent")
)


withr::with_options(
  list(ggplot2.discrete.fill = discrete_palettes), {
    print(cty_by_var(Type_Jour))
    # ggsave("horaire_formation.pdf")
    })



