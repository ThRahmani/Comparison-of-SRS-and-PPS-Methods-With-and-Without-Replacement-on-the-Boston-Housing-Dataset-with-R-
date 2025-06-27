#install.packages("dplyr")
#install.packages("Metrics")
#install.packages("readr")

library(dplyr)
library(Metrics)
library(readr)

set.seed(123)

# ------------------- بخش 1: بارگذاری داده و تعریف جامعه ------------------- #

# خواندن دیتاست Boston Housing از لینک
url <- "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv"
data1 <- read_csv(url)

# انتخاب ستون MEDV به عنوان جامعه آماری
data2 <- data1$medv

# محاسبه پارامترهای جامعه
mean_pop <- mean(data2)
var_pop <- var(data2)
median_pop <- median(data2)
midrange_pop <- (min(data2) + max(data2)) / 2

# نمایش نتایج پارامترهای جامعه
print("پارامترهای جامعه:")
print(paste("میانگین =", mean_pop))
print(paste("واریانس =", var_pop))
print(paste("میانه =", median_pop))
print(paste("میانگین حدی (midrange) =", midrange_pop))

# آزمون نرمال بودن جامعه
print("نتیجه آزمون نرمال بودن جامعه:")
print(shapiro.test(data2))


# ------------------- بخش 2: نمونه‌گیری تصادفی ساده ------------------- #

# نمونه‌ها
samp1 <- sample(data2, size = 15, replace = TRUE)
samp2 <- sample(data2, size = 15, replace = FALSE)
samp3 <- sample(data2, size = 80, replace = TRUE)
samp4 <- sample(data2, size = 80, replace = FALSE)

# تابع محاسبه آماره‌ها
sample_stats <- function(x) {
  list(
    mean = mean(x),
    var = var(x),
    bias = mean(x) - mean_pop,
    mse = (mean(x) - mean_pop)^2 + var(x)
  )
}

# محاسبه آماره‌ها
res1 <- sample_stats(samp1)
res2 <- sample_stats(samp2)
res3 <- sample_stats(samp3)
res4 <- sample_stats(samp4)

# نمایش نتایج نمونه‌گیری تصادفی ساده
print("نتایج نمونه‌گیری تصادفی ساده:")
print("نمونه 1 (با جایگذاری، n=15):")
print(res1)
print("نمونه 2 (بدون جایگذاری، n=15):")
print(res2)
print("نمونه 3 (با جایگذاری، n=80):")
print(res3)
print("نمونه 4 (بدون جایگذاری، n=80):")
print(res4)

# آزمون نرمال بودن نمونه‌ها
print("آزمون نرمال بودن نمونه‌ها:")
print(shapiro.test(samp1))
print(shapiro.test(samp2))
print(shapiro.test(samp3))
print(shapiro.test(samp4))

# رسم هیستوگرام‌ها
par(mfrow = c(2, 2))
hist(samp1, main = "SRS WR (n=15)")
hist(samp2, main = "SRS WOR (n=15)")
hist(samp3, main = "SRS WR (n=80)")
hist(samp4, main = "SRS WOR (n=80)")


# ------------------- بخش 3: نمونه‌گیری با احتمال متغیر (PPS) ------------------- #

# وزن‌دهی بر اساس نسبت مقدار هر داده به کل
weights <- data2 / sum(data2)

# نمونه‌گیری با جایگذاری و بدون جایگذاری
samp_pps_wr <- sample_n(data.frame(x = data2), size = 40, replace = TRUE, weight = weights)
samp_pps_wor <- sample_n(data.frame(x = data2), size = 40, replace = FALSE, weight = weights)

# محاسبه آماره‌های PPS
res_pps_wr <- sample_stats(samp_pps_wr$x)
res_pps_wor <- sample_stats(samp_pps_wor$x)

# نمایش نتایج PPS
print("نتایج نمونه‌گیری با احتمال متغیر (PPS):")
print("نمونه PPS با جایگذاری:")
print(res_pps_wr)
print("نمونه PPS بدون جایگذاری:")
print(res_pps_wor)
