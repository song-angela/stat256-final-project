library(haven)
library(dplyr)
library(rdd)

data <- read_dta(file.path('replication_materials', '20140162_Data.dta'))
databw <- max(abs(min(data$score2_1)), abs(max(data$score2_1)))

householdreg <- RDestimate(
    householdsize ~ score2_1 + collect_2,
    data = data,
    cutpoint = 0,
    bw = databw,
    kernel = 'rectangular',
    cluster = data$parroqui
)
householdreg$est 
householdreg$se 
householdreg$p

agereg <- RDestimate(
    ageresponder ~ score2_1 + collect_2,
    data = data,
    cutpoint = 0,
    bw = databw,
    kernel = 'rectangular',
    cluster = data$parroqui
)
agereg$est 
agereg$se 
agereg$p

schoolreg <- RDestimate(
    schooling_resp ~ score2_1 + collect_2,
    data = data,
    cutpoint = 0,
    bw = databw,
    kernel = 'rectangular',
    cluster = data$parroqui
)
schoolreg$est 
schoolreg$se 
schoolreg$p
