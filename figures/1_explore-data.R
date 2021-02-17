## Assignment 5
## FISH 497 - Intro to Environmental Data Science
## 12 February 2021
## jemorris@uw.edu


## exploring the Siscowet Lake Trout dataset from {FSAdata}


# load libraries
library(here)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(PNWColors)
library(ggtext)


# set directories
data_dir = here::here("data")


# load data
fish = read_csv(file.path(data_dir, "siscowet.csv"))


# exploring the data
print(fish)


# histograms
hist(fish$pnldep)
hist(fish$mesh)
hist(fish$age)
hist(fish$len)
hist(fish$wgt) # appears to have outlier


# univariate scatterplots

# ... ~ age
fish %>%
  ggplot(aes(x = age, y = len)) +  
  geom_point() # positive relationship

fish %>%
  ggplot(aes(x = age, y = len)) +  
  geom_point() +
  # facet by location
  facet_wrap(~locID) # no age data at deer park

fish %>%
  ggplot(aes(x = age, y = len)) +  
  geom_point() +
  # facet by sex
  facet_wrap(~sex) # lots of na's for sex

fish %>%
  ggplot(aes(x = age, y = wgt)) +  
  geom_point() # positive relationship - outlier

fish %>%
  ggplot(aes(x = age, y = wgt)) +  
  geom_point() + 
  # facet by sex
  facet_wrap(~sex)

fish %>%
  ggplot(aes(x = age, y = wgt)) +  
  geom_point() + 
  # facet by location
  facet_wrap(~locID) # no age data at Deer Park

# ... ~ length
fish %>%
  ggplot(aes(x = len, y = wgt)) +  
  geom_point() # non-linear relationship

fish %>%
  ggplot(aes(x = len, y = wgt)) +  
  geom_point() +
  # facet by sex
  facet_wrap(~sex)

fish %>%
  ggplot(aes(x = len, y = wgt)) +  
  geom_point() +
  # facet by location
  facet_wrap(~locID)


# comparing weight ~ length

# eliminating outlier & na's
fish_sub = fish %>%
  dplyr::filter(wgt < 5000)

fish_sub %>%
  ggplot(aes(x = len, y = wgt, color = locID)) +  # location as color
  geom_point() +
  geom_smooth(method = "lm", se = F)

fish_sub %>%
  ggplot(aes(x = len, y = wgt, color = sex)) +  # sex as color
  geom_point() +
  geom_smooth(method = "lm", se = F) # no great difference in relationship by sex


# cleaning up the final figure
len_wgt = ggplot(fish_sub, aes(x = len, y = wgt, col = locID)) 


len_wgt + geom_jitter(shape = 16, alpha = 0.5, size = 2) +
  #facet_wrap(~locID) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "gray80", fill = NA),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), 
                     limits = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, 800)) +
  xlab("Total length (mm)") +
  ylab("Weight (g)") +
  #scale_color_viridis(discrete = T)
  #scale_color_viridis(option = "magma", discrete = T)
  #scale_color_manual(values = pnw_palette("Sunset", 4))
  #scale_color_manual(values = c("#a6cee3","#1f78b4", "#b2df8a", "#33a02c")) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_manual(values = c("#604e3c", "#8c9fb7", "#796880", "#274d52")) +
  scale_color_manual(values = c("#274d52", "#c7a2a6", "#818b70", "#604e3c")) +
  geom_smooth(method = "loess", se = F, size = 1, color = "gray15") 


# just want to highlight one location - Deer Park

# subset fish from Deer Park
dp = fish_sub %>%
  dplyr::filter(locID == "Deer Park")

# subset fish from all other locations
other = fish_sub %>%
  dplyr::filter(locID != "Deer Park")

highlight = ggplot() +
  geom_jitter(data = other, aes(x = len, y = wgt), color = "gray60", alpha = 0.5, shape = 16, size = 2) +
  geom_jitter(data = dp, aes(x = len, y = wgt), color = "indianred3", alpha = 0.3, shape = 16, size = 2) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "gray80", fill = NA),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), 
                     limits = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, 800)) +
  xlab("Total length (mm)") +
  ylab("Weight (g)") +
  geom_smooth(method = "loess")
highlight


len_wgt + geom_point(shape = 16) +
  #facet_wrap(~locID) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "gray80", fill = NA),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), 
                     limits = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, 800)) +
  xlab("Total length (mm)") +
  ylab("Weight (g)") +
  scale_color_manual(values = c("gray50", "gray50", "gray50", "#c7a2a6")) +
  annotate(geom = "text", x = 100, y = 4000, 
           label = "Deer Park", 
           color = "#3c6fde")



# final plot
final = ggplot() +
  geom_jitter(data = other, aes(x = len, y = wgt), color = "#878787", alpha = 0.5, shape = 16, size = 2) +
  geom_jitter(data = dp, aes(x = len, y = wgt), color = "#3c6fde", alpha = 0.3, shape = 16, size = 2) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "gray80", fill = NA),
    legend.position = "none",
    plot.subtitle = element_text(color = "gray25", face = "italic")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), 
                     limits = c(0, NA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, 800)) +
  xlab("Total length (mm)") +
  ylab("Weight (g)") +
  labs(color="Location",
       title = "Trout weight increases with total length",
       subtitle = "Siscowet Lake Trout captured at Deer Park share similar physiological traits\nwith those caught at other Michigan locations on Lake Superior") +
  annotate(geom = "text", x = 350, y = 2000, 
           label = "Deer Park", 
           color = "#3c6fde") +
  annotate(geom = "text", x = 700, y = 1300,
           label = "Blind Sucker",
           color = "#878787") +
  annotate(geom = "text", x = 700, y = 1100,
           label = "Grand Marais",
           color = "#878787")+
  annotate(geom = "text", x = 725, y = 900,
           label = "Little Lake Harbour",
           color = "#878787")
final
ggsave(plot = final, "figures//weight-vs-length.pdf", height = 5, width = 7, units = "in")
