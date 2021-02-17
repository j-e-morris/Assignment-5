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

nrow(filter(fish, locID == "Blind Sucker")) # 138
nrow(filter(fish, locID == "Deer Park")) # 397
nrow(filter(fish, locID == "Grand Marais")) # 152
nrow(filter(fish, locID == "Little Lake Harbor")) # 93

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
len_wgt = ggplot(fish_sub, aes(x = len, y = wgt, col = locID)) #+ 
  #geom_point() +
  #geom_smooth(method = "lm", se = F)



len_wgt + geom_jitter(shape = 16, alpha = 0.9, size = 2) +
  #facet_wrap(~locID) +
  theme(
    panel.background = element_blank(),
    #axis.line = element_line(color = "gray50"),
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
  geom_smooth(method = "loess", se = F, size = 1.5, color = "gray15") +
  #labs(color="Location",
  #     title = "<b> Trout weight increases with total length</b><br>
  #             <span style = 'font-size:10pt'>Siscowet Lake Trout captured at <span style='color:#F8766D'>Blind Sucker</span>, <span style='color:#00BA38'>Deer Park </span>, <span style='color:#00BA38'>Grand Marais </span>, and <span style='color:#619CFF'>Little Lake Harbor</span> have weights that increase with total length. </span>") #+
  #labs(color = "Location",
  #     title = "<b> Fish weight increases with total length</b><br>
  #     <span style = 'font-size:10pt'>Siscowet Lake Trout captured at <span style='color:#274d52'>Blind Sucker</span>, <span style = 'color:#c7a2a6'>Deer Park  </span>, <span style = 'color:#818b70'>Grand Marais </span>, and <span style = 'color:#604e3c'>Little Lake Harbor</span> have weights that increase with total length. </span>")
#