library(haven)
library(clubSandwich)
library(dplyr)
library(ggplot2)
library(gridExtra)

setwd('/Users/nataliasarabiavasquez/Downloads/116335-V1/')
df <- read_dta('Data/20140162_Data.dta')

# Table 4
y <- c('attendpermonth','protestant','religiousness')
results <- as.data.frame(matrix(NA, nrow = 10, ncol = 6))
  
for(i in 1:length(y)){
  m1 <- ivreg(eval(paste0(y[i], '~ score2_1 + moremoneyold + collect_2 | moremoneynew + score2_1 + moremoneyold')) , data=df)
  cov <- vcovCR(m1, df$parroqui, type = "CR0")
  results[2 * (i-1) + 1,1] <- round(m1$coefficients[4],3)
  results[2 * (i-1) + 2,1] <- round(sqrt(diag(cov))[4],3)
  
  m2 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + moremoneyold + collect_2 | moremoneynew + score2_1 + score2_1sq + moremoneyold')) , data=df)
  cov <- vcovCR(m2, df$parroqui, type = "CR0")
  results[2 * (i-1) + 1,2] <- round(m2$coefficients[5],3)
  results[2 * (i-1) + 2,2] <- round(sqrt(diag(cov))[5],3)
  
  m3 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + score2_1cu + moremoneyold + collect_2 | moremoneynew + score2_1 + score2_1sq + score2_1cu + moremoneyold')) , data=df)
  cov <- vcovCR(m3, df$parroqui, type = "CR0")
  results[2 * (i-1) + 1,3] <- round(m3$coefficients[6],3)
  results[2 * (i-1) + 2,3] <- round(sqrt(diag(cov))[6],3)
  
  m4 <- ivreg(eval(paste0(y[i], '~ score2_1 + moremoneyold + collect_2 +  householdsize + ageresponder + schooling_resp| moremoneynew + score2_1 + moremoneyold + householdsize + ageresponder + schooling_resp')), data=df)
  cov <- vcovCR(m4, df$parroqui, type = "CR0")
  results[2 * (i-1) + 1,4] <- round(m4$coefficients[4],3)
  results[2 * (i-1) + 2,4] <- round(sqrt(diag(cov))[4],3)
  
  m5 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + moremoneyold + collect_2 + householdsize + ageresponder + schooling_resp | moremoneynew + score2_1 + score2_1sq + moremoneyold + householdsize + ageresponder + schooling_resp')), data=df)
  cov <- vcovCR(m5, df$parroqui, type = "CR0")
  results[2 * (i-1) + 1,5] <- round(m5$coefficients[5],3)
  results[2 * (i-1) + 2,5] <- round(sqrt(diag(cov))[5],3)
  
  m6 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + score2_1cu + moremoneyold + collect_2 + householdsize + ageresponder + schooling_resp| moremoneynew + score2_1 + score2_1sq + score2_1cu + moremoneyold + householdsize + ageresponder + schooling_resp')), data=df)
  cov <- vcovCR(m6, df$parroqui, type = "CR0")
  results[2 * (i-1) + 1,6] <- round(m6$coefficients[6],3)
  results[2 * (i-1) + 2,6] <- round(sqrt(diag(cov))[6],3)
  
}


names(results) <- c("(1)","(2)","(3)","(4)","(5)","(6)")
results$dep_var <- c('Church attendance','','Being Evangelical','','Self-rated religiousness','','1st-order polynomial','2nd-order polynomial','3rd-order polynomial','Controls')
results$type <- c('estimate','se','estimate','se','estimate','se','','','','')
results$observations <- c(2645,NA,2645,NA,2645,NA,NA,NA,NA,NA)

results <- results %>%
  select(dep_var, type, c("(1)","(2)","(3)","(4)","(5)","(6)"),observations)

results[7,3] <- 1
results[7,6] <- 1
results[8,4] <- 1
results[8,7] <- 1
results[9,5] <- 1
results[9,8] <- 1
results[10,6] <- 1
results[10,7] <- 1
results[10,8] <- 1

# Table 5
y <- c('attendpermonth','protestant')
results <- as.data.frame(matrix(NA, nrow = 12, ncol = 6))
data <- list(df[df$religiousness >= 6.827599,],
             df[df$religiousness < 6.827599,])

k <- 1
for(i in 1:length(y)){
  for(d in 1:length(data)){
    m1 <- ivreg(eval(paste0(y[i], '~ score2_1 + moremoneyold + collect_2 | moremoneynew + score2_1 + moremoneyold')) , data=data[[d]])
    cov <- vcovCR(m1, data[[d]]$parroqui, type = "CR0")
    results[2 * (k-1) + 1,1] <- round(m1$coefficients[4],3)
    results[2 * (k-1) + 2,1] <- round(sqrt(diag(cov))[4],3)
    
    m2 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + moremoneyold + collect_2 | moremoneynew + score2_1 + score2_1sq + moremoneyold')) , data=data[[d]])
    cov <- vcovCR(m2, data[[d]]$parroqui, type = "CR0")
    results[2 * (k-1) + 1,2] <- round(m2$coefficients[5],3)
    results[2 * (k-1) + 2,2] <- round(sqrt(diag(cov))[5],3)
    
    m3 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + score2_1cu + moremoneyold + collect_2 | moremoneynew + score2_1 + score2_1sq + score2_1cu + moremoneyold')) , data=data[[d]])
    cov <- vcovCR(m3, data[[d]]$parroqui, type = "CR0")
    results[2 * (k-1) + 1,3] <- round(m3$coefficients[6],3)
    results[2 * (k-1) + 2,3] <- round(sqrt(diag(cov))[6],3)
    
    m4 <- ivreg(eval(paste0(y[i], '~ score2_1 + moremoneyold + collect_2 +  householdsize + ageresponder + schooling_resp| moremoneynew + score2_1 + moremoneyold + householdsize + ageresponder + schooling_resp')), data=data[[d]])
    cov <- vcovCR(m4, data[[d]]$parroqui, type = "CR0")
    results[2 * (k-1) + 1,4] <- round(m4$coefficients[4],3)
    results[2 * (k-1) + 2,4] <- round(sqrt(diag(cov))[4],3)
    
    m5 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + moremoneyold + collect_2 + householdsize + ageresponder + schooling_resp | moremoneynew + score2_1 + score2_1sq + moremoneyold + householdsize + ageresponder + schooling_resp')), data=data[[d]])
    cov <- vcovCR(m5, data[[d]]$parroqui, type = "CR0")
    results[2 * (k-1) + 1,5] <- round(m5$coefficients[5],3)
    results[2 * (k-1) + 2,5] <- round(sqrt(diag(cov))[5],3)
    
    m6 <- ivreg(eval(paste0(y[i], '~ score2_1 + score2_1sq + score2_1cu + moremoneyold + collect_2 + householdsize + ageresponder + schooling_resp| moremoneynew + score2_1 + score2_1sq + score2_1cu + moremoneyold + householdsize + ageresponder + schooling_resp')), data=data[[d]])
    cov <- vcovCR(m6, data[[d]]$parroqui, type = "CR0")
    results[2 * (k-1) + 1,6] <- round(m6$coefficients[6],3)
    results[2 * (k-1) + 2,6] <- round(sqrt(diag(cov))[6],3)
    
    k <- k + 1
  }  
}

names(results) <- c("(1)","(2)","(3)","(4)","(5)","(6)")
results$dep_var <- c('Church attendance','','','','Being Evangelical','','','','1st-order polynomial','2nd-order polynomial','3rd-order polynomial','Controls')
results$sub <- c('Above average religious','','Below average religious','','Above average religious','','Below average religious','','','','','')
results$type <- c('estimate','se','estimate','se','estimate','se','estimate','se','','','','')
results$observations <- c(1480,NA,1165,NA,1480,NA,1165,NA,NA,NA,NA,NA)

results <- results %>%
  select(dep_var, sub, type, c("(1)","(2)","(3)","(4)","(5)","(6)"),observations)

results[9,4] <- 1
results[9,7] <- 1
results[10,5] <- 1
results[10,8] <- 1
results[11,6] <- 1
results[11,9] <- 1
results[12,7] <- 1
results[12,8] <- 1
results[12,9] <- 1
results


# First set of graphs
# Figure 4

fig <- df %>%
  group_by(bins) %>%
  summarize(protestant = mean(protestant),
            attendpermonth = mean(attendpermonth),
            religiousness = mean(religiousness),
            score2_1 = mean(score2_1),
            n_bins = mean(n_bins))

p1 <- ggplot(fig, aes(x = score2_1, y = attendpermonth)) +
  geom_point() +
  scale_x_continuous(limits = c(-4.6, 4.6)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(data = filter(fig, score2_1 <= 0), method = "lm", se = FALSE, size = .6 ) +
  geom_smooth(data = filter(fig, score2_1 > 0), method = "lm", se = FALSE, size = .6) +
  geom_vline(xintercept = 0) +
  labs(title = 'Panel A: Monthly service attendance', caption = "Discontinuity at threshold: 1.390 (p=0.00)") +
  theme(title=element_text(size=8))

p2 <- ggplot(fig, aes(x = score2_1, y = protestant)) +
  geom_point() +
  scale_x_continuous(limits = c(-4.6, 4.6)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  geom_smooth(data = filter(fig, score2_1 <= 0), method = "lm", se = FALSE, size = .6 ) +
  geom_smooth(data = filter(fig, score2_1 > 0), method = "lm", se = FALSE, size = .6) +
  geom_vline(xintercept = 0) +
  labs(title = 'Panel B: Being Evangelical', caption = "Discontinuity at threshold: 0.051 (p=0.06)") +
  theme(title=element_text(size=8))

p3 <- ggplot(fig, aes(x = score2_1, y = religiousness)) +
  geom_point() +
  scale_x_continuous(limits = c(-4.6, 4.6)) +
  scale_y_continuous(limits = c(4, 10)) +
  geom_smooth(data = filter(fig, score2_1 <= 0), method = "lm", se = FALSE, size = .6 ) +
  geom_smooth(data = filter(fig, score2_1 > 0), method = "lm", se = FALSE, size = .6) +
  geom_vline(xintercept = 0) +
  labs(title = 'Panel C: How religious are you?', caption = "Discontinuity at threshold: 0.203 (p=0.16)") +
  theme(title=element_text(size=8))

grid.arrange(p1, p2, p3, ncol=3)

# Second set of graphs
# Figure 5

fig1 <- df %>%
  filter(religiousness >= 6.827599) %>%
  group_by(bins) %>%
  summarize(protestant = mean(protestant),
            attendpermonth = mean(attendpermonth),
            religiousness = mean(religiousness),
            score2_1 = mean(score2_1),
            n_bins = mean(n_bins))

fig2 <- df %>%
  filter(religiousness < 6.827599) %>%
  group_by(bins) %>%
  summarize(protestant = mean(protestant),
            attendpermonth = mean(attendpermonth),
            religiousness = mean(religiousness),
            score2_1 = mean(score2_1),
            n_bins = mean(n_bins))

p1 <- ggplot(fig1, aes(x = score2_1, y = attendpermonth)) +
  geom_point() +
  scale_x_continuous(limits = c(-4.6, 4.6)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(data = filter(fig1, score2_1 <= 0), method = "lm", se = FALSE, size = .6 ) +
  geom_smooth(data = filter(fig1, score2_1 > 0), method = "lm", se = FALSE, size = .6) +
  geom_vline(xintercept = 0) +
  labs(title = 'Panel A: Monthly service attendance\n(above average religious)', caption = "Discontinuity at threshold: 2.023 (p=0.00)") +
  theme(title=element_text(size=8))

p2 <- ggplot(fig2, aes(x = score2_1, y = attendpermonth)) +
  geom_point() +
  scale_x_continuous(limits = c(-4.6, 4.6)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(data = filter(fig2, score2_1 <= 0), method = "lm", se = FALSE, size = .6 ) +
  geom_smooth(data = filter(fig2, score2_1 > 0), method = "lm", se = FALSE, size = .6) +
  geom_vline(xintercept = 0) +
  labs(title = 'Panel B: Monthly service attendance\n(below average religious)', caption = "Discontinuity at threshold: 0.429 (p=0.44)") +
  theme(title=element_text(size=8))

p3 <- ggplot(fig1, aes(x = score2_1, y = protestant)) +
  geom_point() +
  scale_x_continuous(limits = c(-4.6, 4.6)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  geom_smooth(data = filter(fig1, score2_1 <= 0), method = "lm", se = FALSE, size = .6 ) +
  geom_smooth(data = filter(fig1, score2_1 > 0), method = "lm", se = FALSE, size = .6) +
  geom_vline(xintercept = 0) +
  labs(title = 'Panel C: Being Evangelical\n(above average religious)', caption = "Discontinuity at threshold: 0.105 (p=0.00)")+
  theme(title=element_text(size=8))

p4 <- ggplot(fig2, aes(x = score2_1, y = protestant)) +
  geom_point() +
  scale_x_continuous(limits = c(-4.6, 4.6)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  geom_smooth(data = filter(fig2, score2_1 <= 0), method = "lm", se = FALSE, size = .6 ) +
  geom_smooth(data = filter(fig2, score2_1 > 0), method = "lm", se = FALSE, size = .6) +
  geom_vline(xintercept = 0) +
  labs(title = 'Panel D: Being Evangelical\n(below average religious)', caption = "Discontinuity at threshold: -0.017 (p=0.71)")+
  theme(title=element_text(size=8))

grid.arrange(p1, p2, p3, p4, ncol=2)







