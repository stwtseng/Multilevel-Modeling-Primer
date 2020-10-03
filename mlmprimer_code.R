##### MLM Primer Example: Manager Feedback and Employee Engagement
##### Author: Steven T. Tseng, Ph.D. Candidate, The University of Akron
##### Credit to Michael Freeman, University of Washington, for original source code
##### Source code link: https://github.com/mkfreeman/hierarchical-models/

### Generate random engagement data

# Load requisite libraries
library(dplyr) # data wrangling
library(lme4) # modeling
library(ggplot2, ggthemes) # visualization
library(arm) # standard errors

# Set parameters for generating engagement data
managers <- c("Yuki","Thomas", "Shen", "Mo", "Gretchen")
base.eng <- c(3.00, 3.50, 4.00, 4.50, 5.50)
eng.change <- c(0.10, -0.025, 0.05, 0.20, 0.10)
employees.per.mgr <- 20
total.employees <- employees.per.mgr * length(managers)

# Generate dataframe of employees and feedback interactions
ids <- 1:total.employees
manager <- rep(managers, employees.per.mgr)
ff <- floor(runif(total.employees, 0, 11))
bases <- rep(base.eng, employees.per.mgr) * runif(total.employees, .9, 1.1)
changes <- rep(eng.change, employees.per.mgr) * runif(total.employees, .9, 1.1)
dat <- data.frame(ids, manager, bases, ff, changes)

# Generate engagement outcomes (base + ff * changes)
dat <- dat %>% mutate(engagement = bases + ff * changes)

### Modeling

# Linear Model
m0 <- lm(engagement ~ ff, data = dat)
predict(m0)
dat$simple.model <- predict(m0)

# Model random intercepts only
m1 <- lmer(engagement ~ ff + (1|manager), data = dat)
dat$random.intercept.preds <- predict(m1)

# Model random slopes only
m2 <- lmer(engagement ~ ff + (0 + ff|manager), data = dat,
           control = lmerControl(optimizer ="Nelder_Mead"))
dat$random.slope.preds <- predict(m2)

# Model random slopes and intercepts
m3 <- lmer(engagement ~ ff + (1 + ff|manager), data = dat, REML = FALSE, 
           control = lmerControl(optimizer ="Nelder_Mead"))
dat$random.slope.int.preds <- predict(m3)

### Visualization

# Scatterplot
simple.scatterplot <- ggplot(data = dat, aes(x = ff, y = simple.model)) +
  geom_point(aes(x = ff, y = engagement), size = 4)

simple.scatterplot +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")

# Simple LM
simple.reg <- ggplot(data = dat, aes(x = ff, y = simple.model)) +
  geom_point(aes(x = ff, y = engagement), size = 4) + 
  geom_line(color = "red", size = 1.5)

simple.reg +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")

# Scatterplot Color
color.scatterplot <- ggplot(data = dat, aes(x = ff, y = simple.model, 
                                            group = manager, color = manager)) +
  geom_point(aes(x = ff, y = engagement), size = 5, alpha = 0.8)

color.scatterplot +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.background = element_rect(fill = "gray90")) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_color_discrete(name = "Manager") +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")

# LM Color
color.reg <- ggplot(data = dat, aes(x = ff, y = simple.model, 
                                    group = manager, color = manager)) +
  geom_point(aes(x = ff, y = engagement), size = 5, alpha = 0.8) + 
  geom_line(color = "red", size = 1.5)

color.reg +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.background = element_rect(fill = "gray90")) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_color_discrete(name = "Manager") +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")

# Visualize random intercept
rand.int <- ggplot(data = dat, aes(x = ff, y = random.intercept.preds, 
                                   group = manager, color = manager)) +
  geom_point(aes(x = ff, y = engagement), size = 5, alpha = 0.8) + 
  geom_line(size = 1.5)

rand.int +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.background = element_rect(fill = "gray90")) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_color_discrete(name = "Manager") +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")

# Visualize random slope
rand.slope <- ggplot(data = dat, aes(x = ff, y = random.slope.preds, 
                                     group = manager, color = manager)) +
  geom_point(aes(x = ff, y = engagement), size = 5, alpha = 0.8) + 
  geom_line(size = 1.5)

rand.slope +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.background = element_rect(fill = "gray90")) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_color_discrete(name = "Manager") +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")


# Visualize random slope + intercept
rand.intslope <- ggplot(data = dat, aes(x = ff, y = random.slope.int.preds, 
                                        group = manager, color = manager)) +
  geom_point(aes(x = ff, y = engagement), size = 5, alpha = 0.8) + 
  geom_line(size = 1.5)

rand.intslope +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.background = element_rect(fill = "gray90")) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_color_discrete(name = "Manager") +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")

# Visualize random slope + intercept + OLS line
rand.all <- ggplot(data = dat, aes(x = ff, y = random.slope.int.preds, 
                                        group = manager, color = manager)) +
  geom_point(aes(x = ff, y = engagement), size = 5, alpha = 0.8) + 
  geom_line(size = 1.5)

rand.all +theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.background = element_rect(fill = "gray90")) +
  scale_x_discrete(limits = c(0:10), breaks = c(0:10)) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1:7)) +
  scale_color_discrete(name = "Manager") +
  xlab("Number of Weekly Feedback Interactions") + ylab("Employee Engagement")
