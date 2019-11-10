##### BAE590 Module 11 homework script
##### Author: Kanjana Laosuntisuk
##### Date created: Nov 8, 2019
##### Last modified: Nov 10, 2019

# clear workspace & load packages
rm(list=ls(all=TRUE))

install.packages("corrr")

library(tidyverse)
library(modelr)
library(broom)
library(corrr)

# import & inspect data
meat <- read.csv("data/meat-production-2017.csv")
GDP <- read.csv("data/gdp-2017.csv")
pop <- read.csv("data/total-population-2017.csv")

head(meat)
str(meat)
head(GDP)
str(GDP)
head(pop)
str(pop)

##### visuallize data ------------------------------------------
# calculate total meat produced per country
meat %>%
  group_by(country) %>%
  mutate(total_meat_produced = sum(meat_produced, na.rm = TRUE)) %>%
  select(country, total_meat_produced) %>%
  distinct() -> meat_total
head(meat_total)
str(meat_total)

# join meat_total with GDP and population datasets
meat_total %>%
  left_join(GDP) %>%
  left_join(pop) %>%
  na.omit() -> meat_gdp_pop
head(meat_gdp_pop)
str(meat_gdp_pop)

# make a plot of total meat produced and GDP
meat_gdp_pop %>%
  ggplot(mapping = aes(x = gdp, y = total_meat_produced)) +
  geom_point(alpha = 0.5) +
  theme_bw()

# make a plot of total meat produced and population
meat_gdp_pop %>%
  ggplot(mapping = aes(x = total_population, y = total_meat_produced)) +
  geom_point(alpha = 0.5) +
  theme_bw()

# user-defined function for normalization
normalize <- function(x) {
  n <- (x - min(x)) / (max(x) - min(x))
  return(n)
}

# normalize the total meat produced, GDP and population columns
meat_gdp_pop$meat_norm <- normalize(meat_gdp_pop$total_meat_produced)
head(meat_gdp_pop)
summary(meat_gdp_pop$meat_norm)

meat_gdp_pop$gdp_norm <- normalize(meat_gdp_pop$gdp)
head(meat_gdp_pop)
summary(meat_gdp_pop$gdp_norm)

meat_gdp_pop$pop_norm <- normalize(meat_gdp_pop$total_population)
head(meat_gdp_pop)
summary(meat_gdp_pop$pop_norm)

# remake a plot of total meat produced and GDP
meat_gdp_pop %>%
  ggplot(mapping = aes(x = gdp_norm, y = meat_norm)) +
  geom_point(alpha = 0.5) +
  theme_bw()

# zoom in a plot of total meat produced vs GDP
ggplot(data = meat_gdp_pop) +
  geom_point(mapping = aes(x = gdp_norm, y = meat_norm), alpha = 0.5) +
  lims(x = c(0,0.05),
       y = c(0, 0.15)) +
  theme_bw()

# remake a plot of total meat produced and population
meat_gdp_pop %>%
  ggplot(mapping = aes(x = pop_norm, y = meat_norm)) +
  geom_point(alpha = 0.5) +
  theme_bw()

##### calculate and visuallize correlation ------------------------------------------
# calculate correlation coefficients
meat_gdp_pop %>%
  ungroup() %>%
  dplyr::select(meat_norm, gdp_norm, pop_norm) %>%
  correlate() %>%
  fashion()

# visualize correlation
meat_gdp_pop %>%
  ungroup() %>%
  dplyr::select(meat_norm, gdp_norm, pop_norm) %>%
  correlate() %>%
  rplot()

##### fit regression models ------------------------------------------
# make a dataset containing only normalized values
meat_gdp_pop %>%
  ungroup() %>%
  dplyr::select(meat_norm, gdp_norm, pop_norm) -> meat_gdp_pop_norm
head(meat_gdp_pop_norm)
str(meat_gdp_pop_norm)

### make a model of meat_norm as a function of gdp --------
meat_gdp_mod <- lm(meat_norm ~ gdp_norm, data = meat_gdp_pop_norm)
tidy(meat_gdp_mod)
glance(meat_gdp_mod)
summary(meat_gdp_mod)

# generate prediction and residuals for a meat_norm vs gdp_norm model
meat_gdp_norm_pred <- augment(meat_gdp_mod, data = meat_gdp_pop_norm)
head(meat_gdp_norm_pred)

# plot observed vs fitted total meat produced from a meat_norm vs gdp_norm model
ggplot(meat_gdp_norm_pred, aes(x = .fitted, y = meat_norm)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  lims(x = c(0,0.15),
       y = c(0, 0.15)) +
  labs(title = "Observed vs Fitted total meat produced",
       subtitle = "from total meat vs GDP model",
       x = "total meat_predicted",
       y = "total meat_observed") +
  theme_bw()

# plot residuals vs total meat produced
ggplot(meat_gdp_norm_pred, aes(x = meat_norm, y = .resid)) + 
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.5) +
  lims(x = c(0,0.15),
       y = c(-0.2, 0.2)) +
  labs(title = "Residuals vs total meat produced",
       subtitle = "from total meat vs GDP model",
       x = "total meat_observed",
       y = "residuals")

### make a model of meat_norm as a function of gdp --------
meat_pop_mod <- lm(meat_norm ~ pop_norm, data = meat_gdp_pop_norm)
tidy(meat_pop_mod)
glance(meat_pop_mod)
summary(meat_pop_mod)

# generate prediction and residuals for a meat_norm vs pop_norm model
meat_pop_norm_pred <- augment(meat_pop_mod, data = meat_gdp_pop_norm)
head(meat_pop_norm_pred)

# plot observed vs fitted total meat produced from a meat_norm vs pop_norm model
ggplot(meat_pop_norm_pred, aes(x = .fitted, y = meat_norm)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  lims(x = c(0,0.15),
       y = c(0, 0.15)) +
  labs(title = "Observed vs Fitted total meat produced",
       subtitle = "from total meat vs population model",
       x = "total meat_predicted",
       y = "total meat_observed") +
  theme_bw()

# plot residuals vs total meat produced from a meat_norm vs pop_norm model
ggplot(meat_pop_norm_pred, aes(x = meat_norm, y = .resid)) + 
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.5) +
  lims(x = c(0,0.15),
       y = c(-0.2, 0.2)) +
  labs(title = "Residuals vs total meat produced",
       subtitle = "from total meat vs pop model",
       x = "total meat_observed",
       y = "residuals")

### make a model of meat_norm as a function of gdp --------
meat_gdp_pop_mod1 <- lm(meat_norm ~ gdp_norm + pop_norm, data = meat_gdp_pop_norm)
tidy(meat_gdp_pop_mod1)
glance(meat_gdp_pop_mod1)
summary(meat_gdp_pop_mod1)

meat_gdp_pop_mod2 <- lm(meat_norm ~ gdp_norm * pop_norm, data = meat_gdp_pop_norm)
tidy(meat_gdp_pop_mod2)
glance(meat_gdp_pop_mod2)
summary(meat_gdp_pop_mod2)

# generate prediction and residuals for a meat_norm vs gdp_norm & pop_norm model
meat_gdp_pop_norm_pred1 <- augment(meat_gdp_pop_mod1, data = meat_gdp_pop_norm)
head(meat_gdp_pop_norm_pred1)

meat_gdp_pop_norm_pred2 <- augment(meat_gdp_pop_mod2, data = meat_gdp_pop_norm)
head(meat_gdp_pop_norm_pred2)

# plot observed vs fitted total meat produced from a meat_norm vs gdp_norm + pop_norm model
ggplot(meat_gdp_pop_norm_pred1, aes(x = .fitted, y = meat_norm)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  lims(x = c(0,0.15),
       y = c(0, 0.15)) +
  labs(title = "Observed vs Fitted total meat produced",
       subtitle = "from total meat vs gdp + population model",
       x = "total meat_predicted",
       y = "total meat_observed") +
  theme_bw()

# plot residuals vs total meat produced from a meat_norm vs gdp_norm + pop_norm model
ggplot(meat_gdp_pop_norm_pred1, aes(x = meat_norm, y = .resid)) + 
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.5) +
  lims(x = c(0,0.15),
       y = c(-0.2, 0.2)) +
  labs(title = "Residuals vs total meat produced",
       subtitle = "from total meat vs gdp + population model",
       x = "total meat_observed",
       y = "residuals")

# plot observed vs fitted total meat produced from a meat_norm vs gdp_norm * pop_norm model
ggplot(meat_gdp_pop_norm_pred2, aes(x = .fitted, y = meat_norm)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  lims(x = c(0,0.15),
       y = c(0, 0.15)) +
  labs(title = "Observed vs Fitted total meat produced",
       subtitle = "from total meat vs gdp * population model",
       x = "total meat_predicted",
       y = "total meat_observed") +
  theme_bw()

# plot residuals vs total meat produced from a meat_norm vs gdp_norm * pop_norm model
ggplot(meat_gdp_pop_norm_pred2, aes(x = meat_norm, y = .resid)) + 
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.5) +
  lims(x = c(0,0.15),
       y = c(-0.2, 0.2)) +
  labs(title = "Residuals vs total meat produced",
       subtitle = "from total meat vs gdp * population model",
       x = "total meat_observed",
       y = "residuals")

### make a model of log(meat_norm) as a function of log(gdp) --------
# transform meat_norm, gdp_norm, and pop_norm to log()
meat_gdp_pop_norm %>%
  mutate(logmeat = log(meat_norm),
         loggdp = log(gdp_norm),
         logpop = log(pop_norm)) %>%
  select(logmeat, loggdp, logpop) -> meat_gdp_pop_log
head(meat_gdp_pop_log)
str(meat_gdp_pop_log)
summary(meat_gdp_pop_log)

# remove -Inf from tibble
meat_gdp_pop_log <- meat_gdp_pop_log[Reduce(`&`, lapply(meat_gdp_pop_log, is.finite)),]
summary(meat_gdp_pop_log)

# make a model of log(meat_norm) as a function of log(gdp)
meat_gdp_log_mod <- lm(logmeat ~ loggdp, data = meat_gdp_pop_log)
tidy(meat_gdp_log_mod)
glance(meat_gdp_log_mod)
summary(meat_gdp_log_mod)

# plot observed vs fitted total meat produced from a log(meat) vs log(gdp) model
ggplot(meat_gdp_log_mod, aes(x = .fitted, y = logmeat)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Observed vs Fitted total meat produced",
       subtitle = "from log(total meat) vs log(gdp) model",
       x = "total meat_predicted",
       y = "total meat_observed") +
  theme_bw()

# plot residuals vs total meat produced from a log(meat) vs log(gdp) model
ggplot(meat_gdp_log_mod, aes(x = logmeat, y = .resid)) + 
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.5) +
  labs(title = "Residuals vs total meat produced",
       subtitle = "from log(total meat) vs log(gdp)",
       x = "total meat_observed",
       y = "residuals")

### make a model of log(meat_norm) as a function of log(pop) --------
# make a model of log(meat_norm) as a function of log(pop)
meat_pop_log_mod <- lm(logmeat ~ logpop, data = meat_gdp_pop_log)
tidy(meat_pop_log_mod)
glance(meat_pop_log_mod)
summary(meat_pop_log_mod)

# plot observed vs fitted total meat produced from a log(meat) vs log(pop) model
ggplot(meat_pop_log_mod, aes(x = .fitted, y = logmeat)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Observed vs Fitted total meat produced",
       subtitle = "from log(total meat) vs log(pop) model",
       x = "total meat_predicted",
       y = "total meat_observed") +
  theme_bw()

# plot residuals vs total meat produced from a log(meat) vs log(pop) model
ggplot(meat_pop_log_mod, aes(x = logmeat, y = .resid)) + 
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.5) +
  labs(title = "Residuals vs total meat produced",
       subtitle = "from log(total meat) vs log(pop)",
       x = "total meat_observed",
       y = "residuals")


##### Which model to you think is best for predicting the meat produced by a country? 
##### Answer: I produced 6 models;
# 1) total meat ~ GDP
# 2) total meat ~ population
# 3) total meat ~ GDP + population
# 4) total meat ~ GDP*population
# 5) log(total meat) ~ log(GDP)
# 6) log(total meat) ~ log(population)
##### From adjusted R squared, model 4 has the highest adjusted R squared (0.9131), which means that this model best fits the data
##### when looking at the plot of predicted vs observed total meat from each model, it is hard to tell which model is the best since points tend to cluster well around the 1-to-1 reference lines in plots from model 1 – 4 except the log models (5 - 6). Model 5 and 6 show more segregated points around the reference line.
##### when looking at the residual plots, residuals from log models (5 and 6) are more segregated from zero than model 1 - 4. For model 1 – 4, residuals from model 4 tend to get closer to zero than model 1 – 3. 
##### Model 4 is the best model because both GPD and population affects meat consumption and GDP and population are also related to each other since GDP is calculated based on population. Thus, there is an interaction between GDP and population that affects total meat produced. Model 4 considers both the effect of individual factors and also the interaction between two factors on total meat produced and this makes this model most fit the data than other models. 






##### Just for fun: mapping total meat produced around the world ------------------
# load package
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)

# get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
str(world)

# join total meat produced to world map
meat_total %>% 
  left_join(world, by = c("country" = "admin")) -> meat_total_world

# check projection of total_meat_world and change dataframe to sf
st_crs(meat_total_world)
meat_total_world <- st_as_sf(meat_total_world)
st_crs(meat_total_world)

# plot total meat produced around the world
meat_total_world %>%
  ggplot() +
  geom_sf(aes(fill = total_meat_produced), color = "black", size = 0.2) +
  scale_fill_viridis_c() +
  theme_minimal()

# change country names in meat_total dataframe
meat_total$country[meat_total$country == "China, mainland"] <- "China"
meat_total$country[meat_total$country == "China, Hong Kong SAR"] <- "China"
meat_total$country[meat_total$country == "China, Macao SAR"] <- "China"
meat_total$country[meat_total$country == "Russian Federation"] <- "Russia"
meat_total$country[meat_total$country == "Bolivia (Plurinational State of)"] <- "Bolivia"

# join total meat produced to world map
meat_total %>% 
  left_join(world, by = c("country" = "admin")) -> meat_total_world

# check projection of total_meat_world and change dataframe to sf
st_crs(meat_total_world)
meat_total_world <- st_as_sf(meat_total_world)
st_crs(meat_total_world)

# plot total meat produced around the world
meat_total_world %>%
  ggplot() +
  geom_sf(aes(fill = total_meat_produced), color = "black", size = 0.2) +
  scale_fill_viridis_c() +
  labs(fill = NULL,
       title = "Meat produced in 2017 (tons)",
       caption = "Data: UN FAO") +
  theme_minimal()
