library(haven)
library(dplyr)
library(rdd)
library(rdlocrand)

data <- read_dta(file.path('replication_materials', '20140162_Data.dta'))

#################################
### cross-validated bandwidth ###
#################################
cvBandwidth <- function(bw, outcome, delta = 0.5) {
    lower_bound <- quantile(
        data$score2_1[data$score2_1 < 0],
        probs = delta
    )
    upper_bound <- quantile(
        data$score2_1[data$score2_1 >= 0], 
        probs = 1 - delta
    )
    scores <- data$score2_1[
        (data$score2_1 >= lower_bound) & 
            (data$score2_1 <= upper_bound)
    ]
    outcomes <- data[[outcome]][
        (data$score2_1 >= lower_bound) & 
            (data$score2_1 <= upper_bound)
    ]
    cv_resids <- rep(NA, length(scores)) 
    for(i in 1:length(scores)) {
        x <- scores[i] 
        y <- outcomes[i]
        if(x < 0) {
            data_temp <- data %>% 
                filter(score2_1 < x, score2_1 > x - bw) %>% 
                mutate(score_mod = score2_1 - x) 
            if(nrow(data_temp) == 0) {
                cv_resids[i] <- 0
            }
            else {
                local_mod <- lm(
                    as.formula(paste0(outcome, ' ~ score_mod')), 
                    data = data_temp
                )
                cv_resids[i] <- y - coef(local_mod)[1]
            }
        }
        else {
            data_temp <- data %>% 
                filter(score2_1 < x + bw, score2_1 > x) %>% 
                mutate(score_mod = score2_1 - x)
            if(nrow(data_temp) == 0) {
                cv_resids[i] <- 0
            }
            else {
                local_mod <- lm(
                    as.formula(paste0(outcome, ' ~ score_mod')), 
                    data = data_temp 
                )
                cv_resids[i] <- y - coef(local_mod)[1]
            }
        }
    }
    return(mean(cv_resids^2))
}

bw_vec <- seq(3.5, 4.5, 0.05)
cv1 <- sapply(bw_vec, cvBandwidth, outcome = 'attendpermonth')
cv2 <- sapply(bw_vec, cvBandwidth, outcome = 'protestant') 
cv3 <- sapply(bw_vec, cvBandwidth, outcome = 'religiousness')
cv4 <- sapply(bw_vec, cvBandwidth, outcome = 'collect_2')

# re-compute iv regressions with new bandwidths 
attendbw <- min(bw_vec[which.min(cv1)], bw_vec[which.min(cv4)])
attendreg <- RDestimate(
    attendpermonth ~ score2_1 + collect_2,
    data = data,
    cutpoint = 0,
    bw = attendbw,
    kernel = 'rectangular',
    cluster = data$parroqui
)
attendbw
attendreg$est
attendreg$se
attendreg$p

protestantbw <- min(bw_vec[which.min(cv2)], bw_vec[which.min(cv4)])
protestantreg <- RDestimate(
    protestant ~ score2_1 + collect_2, 
    data = data, 
    cutpoint = 0,
    bw = protestantbw, 
    kernel = 'rectangular', 
    cluster = data$parroqui
)
protestantbw
protestantreg$est 
protestantreg$se
protestantreg$p

religiousbw <- min(bw_vec[which.min(cv3)], bw_vec[which.min(cv4)])
religiousreg <- RDestimate(
    religiousness ~ score2_1 + collect_2, 
    data = data, 
    cutpoint = 0,
    bw = religiousbw, 
    kernel = 'rectangular', 
    cluster = data$parroqui
)
religiousbw
religiousreg$est 
religiousreg$se
religiousreg$p

###########################
### local randomization ###
###########################
window_select <- rdwinselect(
    data$score2_1,
    data[c('householdsize', 'ageresponder', 'schooling_resp')],
    cutoff = 0, 
    level = 0.1, 
    nwindows = 40, 
    wobs = 2, 
    dropmissing = TRUE, 
    reps = 1000, 
    seed = 101, 
    plot = TRUE
)

attendrandinf <- rdrandinf(
    data$attendpermonth, 
    data$score2_1, 
    cutoff = 0, 
    wl = window_select$w_left,
    wr = window_select$w_right,
    fuzzy = 1 - data$collect_2, 
    ci = c(0.1, seq(-15, 15, 0.1)), 
)

protestantrandinf <- rdrandinf(
    data$protestant, 
    data$score2_1, 
    cutoff = 0, 
    wl = window_select$w_left,
    wr = window_select$w_right,
    fuzzy = 1 - data$collect_2, 
    ci = c(0.1, seq(-1, 1, 0.01)), 
)

religiousrandinf <- rdrandinf(
    data$religiousness, 
    data$score2_1, 
    cutoff = 0,
    wl = window_select$w_left,
    wr = window_select$w_right,
    fuzzy = 1 - data$collect_2, 
    ci = c(0.1, seq(-10, 10, 0.1))
)
