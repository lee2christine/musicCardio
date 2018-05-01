# Load in data
library(tidyverse)
island <- read_csv("Island Data - Full.csv")
attach(island)


# Create new variable: Pulse Pressure, using Systolic and Diastolic Heart Rate
sys_diff <- `Final Systolic` - `Initial Systolic`
dia_diff <- `Final Diastolic` - `Initial Diastolic`

ini_pulse <- `Initial Systolic` - `Initial Diastolic`
fin_pulse <- `Final Systolic` - `Final Diastolic`
pulse_diff <- fin_pulse - ini_pulse


# Create interaction plot
ggplot(data = island) +
  aes(x = `Music Treatment`, color = Gender, group = Gender, y = pulse_diff) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + 
  labs(y = "Change in Pulse Pressure", title = "Effect of Music on Pulse Pressure") +
  ylim(-50, 50)


# Regression and ANOVA
model <- lm(pulse_diff ~ Gender + `Music Treatment` + Gender:`Music Treatment`)
summary(model)
aov <- aov(pulse_diff ~ Gender + `Music Treatment` + Gender:`Music Treatment`)
summary(aov)

