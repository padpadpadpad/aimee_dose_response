# load in packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(nlme)

# fig path
fig_path <- 'figs'

# write function for determining x intercept for quadratic model
x_at_y0 <- function(xmin, xmax, model, y, colname_x){
  temp = data.frame(x = seq(xmin, xmax, length.out = 100000))
  colnames(temp) <- colname_x
  temp$z <- predict(model, temp)
  return(temp[which.min(abs(temp$z-y)),1])
}

# Analysis of CC low ####
# load in data
d_cclow <- readxl::read_excel('data/raw_data_310717.xlsx', range = 'A2:E47') %>%
  fill(., concentration) %>%
  data.frame()

# quick plot                             
ggplot(d_cclow) +
  geom_point(aes(concentration, coeff)) 

# filter out curve below 500
d_sub <- filter(d_cclow, concentration < 500)

# replot
ggplot(d_sub) +
  geom_point(aes(concentration, coeff)) 

# so this is where you need to make the decision of when to cut the data...
# could be < 50 concentration?

# I shall do an example model and simplification

# linear model only
lin_mod <- lm(coeff ~ concentration, d_sub)
quad_mod <- lm(coeff ~ concentration + I(concentration^2), d_sub)

# compare models
AIC(lin_mod, quad_mod)
anova(lin_mod, quad_mod)

# quadratic term is not significant here

# create a predictions dataset
preds <- data.frame(concentration = seq(min(d_sub$concentration), max(d_sub$concentration), length.out = 50)) %>%
  # predict the linear model with confidence intervals
  mutate(., pred_lin = predict(lin_mod, .),
         # upper confidence intervals
         lin_CI_high = predict(lin_mod, ., interval = 'confidence')[,3],
         # lower confidence intervals
         lin_CI_low = predict(lin_mod, ., interval = 'confidence')[,2],
         # predict the quadratic model
         pred_quad = predict(quad_mod, .),
         # upper confidence intervals
         quad_CI_high = predict(quad_mod, ., interval = 'confidence')[,3],
         # lower confidence intervals
         quad_CI_low = predict(quad_mod, ., interval = 'confidence')[,2])

# fancier plot
ggplot(d_sub) +
  geom_point(aes(concentration, coeff)) +
  geom_line(aes(concentration, pred_quad), col = 'red', preds) +
  geom_ribbon(aes(concentration, ymin = quad_CI_low, ymax = quad_CI_high), fill = 'red', alpha = 0.1, preds) +
  geom_line(aes(concentration, pred_lin), col = 'blue', preds) +
  geom_ribbon(aes(concentration, ymin = lin_CI_low, ymax = lin_CI_high), fill = 'blue', alpha = 0.1, preds) +
  theme_bw() +
  xlab('Concentration') +
  ylab('Response') +
  ggtitle('Models on Concentrations < 500')

ggsave(file.path(fig_path, 'conc_under500.pdf'), last_plot(), height = 8, width = 5)

# neither of these lines go through 0...
# will need to filter to a different point

# work out where a model passes through a y value

# y = 0
# for a linear model ####
# lin mod: y = ax + b
# x = (y - b)/a
x_at_y0_lin <- -coef(lin_mod)[1]/coef(lin_mod)[2]
# this currently does not work as it is a negative concentration

# for a quadratic model
# y = ax^2 + bx + c
x_at_y0_quad <- x_at_y0(-100, 100, quad_mod, 'concentration')
# this also currently does not as it is a negative concentration

# redo analysis byt filter at < 50 ####

# filter out curve below 50
d_sub <- filter(d_cclow, concentration < 50)

# linear model only
lin_mod <- lm(coeff ~ concentration, d_sub)
quad_mod <- lm(coeff ~ concentration + I(concentration^2), d_sub)

# compare models
AIC(lin_mod, quad_mod)
anova(lin_mod, quad_mod)

# quadratic term is not significant here

# create a predictions dataset
preds <- data.frame(concentration = seq(min(d_sub$concentration), max(d_sub$concentration), length.out = 50)) %>%
  # predict the linear model with confidence intervals
  mutate(., pred_lin = predict(lin_mod, .),
         # upper confidence intervals
         lin_CI_high = predict(lin_mod, ., interval = 'confidence')[,3],
         # lower confidence intervals
         lin_CI_low = predict(lin_mod, ., interval = 'confidence')[,2],
         # predict the quadratic model
         pred_quad = predict(quad_mod, .),
         # upper confidence intervals
         quad_CI_high = predict(quad_mod, ., interval = 'confidence')[,3],
         # lower confidence intervals
         quad_CI_low = predict(quad_mod, ., interval = 'confidence')[,2])

# fancier plot
ggplot(d_sub) +
  geom_point(aes(concentration, coeff)) +
  geom_line(aes(concentration, pred_quad), col = 'red', preds) +
  geom_ribbon(aes(concentration, ymin = quad_CI_low, ymax = quad_CI_high), fill = 'red', alpha = 0.1, preds) +
  geom_line(aes(concentration, pred_lin), col = 'blue', preds) +
  geom_ribbon(aes(concentration, ymin = lin_CI_low, ymax = lin_CI_high), fill = 'blue', alpha = 0.1, preds) +
  theme_bw() +
  xlab('Concentration') +
  ylab('Response') +
  ggtitle('Models on Concentrations < 50')

ggsave(file.path(fig_path, 'conc_under50.pdf'), last_plot(), height = 8, width = 5)

# neither of these lines go through 0...
# will need to filter to a different point

# work out where a model passes through a y value

# y = 0
# for a linear model ####
# lin mod: y = ax + b
# x = (y - b)/a
x_at_y0_lin <- -coef(lin_mod)[1]/coef(lin_mod)[2]
# this currently does not work as it is a negative concentration

# for a quadratic model
# y = ax^2 + bx + c
x_at_y0_quad <- x_at_y0(-100, 100, quad_mod, y = 0, 'concentration')
# this also currently does not work as it is only just a positive concentration

# create a means dataset and run the code on that ####
# This is going to be affected by the outliers that cross the x axis into negative numbers

# get means of each concentration
d_means <- group_by(d_cclow, concentration) %>%
  summarise(sd_coeff = sd(coeff),
            coeff = mean(coeff)) %>%
  data.frame()

# quickplot
ggplot(d_means) +
  geom_point(aes(concentration, coeff)) +
  geom_errorbar(aes(x = concentration, ymin = coeff - sd_coeff, ymax = coeff + sd_coeff))

# redo analysis on means ####
# filter out curve below 50
d_sub <- filter(d_means, concentration < 50)

# linear model only
lin_mod <- lm(coeff ~ concentration, d_sub)
quad_mod <- lm(coeff ~ concentration + I(concentration^2), d_sub)

# compare models
AIC(lin_mod, quad_mod)
anova(lin_mod, quad_mod)
# have to use a linear model because there are too few points to fit a quadratic, no degrees of freedom

# quadratic term is not significant here

# create a predictions dataset
preds <- data.frame(concentration = seq(min(d_sub$concentration), max(d_sub$concentration), length.out = 50)) %>%
  # predict the linear model with confidence intervals
  mutate(., pred_lin = predict(lin_mod, .),
         # upper confidence intervals
         lin_CI_high = predict(lin_mod, ., interval = 'confidence')[,3],
         # lower confidence intervals
         lin_CI_low = predict(lin_mod, ., interval = 'confidence')[,2],
         # predict the quadratic model
         pred_quad = predict(quad_mod, .),
         # upper confidence intervals
         quad_CI_high = predict(quad_mod, ., interval = 'confidence')[,3],
         # lower confidence intervals
         quad_CI_low = predict(quad_mod, ., interval = 'confidence')[,2])

# fancier plot
ggplot(d_sub) +
  geom_point(aes(concentration, coeff)) +
  geom_line(aes(concentration, pred_quad), col = 'red', preds) +
  geom_ribbon(aes(concentration, ymin = quad_CI_low, ymax = quad_CI_high), fill = 'red', alpha = 0.1, preds) +
  geom_line(aes(concentration, pred_lin), col = 'blue', preds) +
  geom_ribbon(aes(concentration, ymin = lin_CI_low, ymax = lin_CI_high), fill = 'blue', alpha = 0.1, preds) +
  theme_bw() +
  xlab('Concentration') +
  ylab('Response') +
  ggtitle('Models on Mean Concentrations < 50')

ggsave(file.path(fig_path, 'conc_under50_means.pdf'), last_plot(), height = 8, width = 5)


# However this still does not give you what you would like

# y = 0
x_at_y0_lin <- -coef(lin_mod)[1]/coef(lin_mod)[2]
# this currently does not work as it is a negative concentration

# for a quadratic model
x_at_y0_quad <- x_at_y0(-100, 100, quad_mod, y = 0, 'concentration')
# this also currently does not work as it is only just a positive concentration


