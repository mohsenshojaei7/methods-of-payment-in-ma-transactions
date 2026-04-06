#   Methods of Payment in M&A       

# Load necessary packages

install.packages("DescTools")   
install.packages("knitr")     
install.packages("tidyverse")  
install.packages("stargazer")   
install.packages("lmtest")     
install.packages("car")         
install.packages("vtable")      
require(DescTools)   
require(knitr)      
require(tidyverse)      
require(stargazer)      
require(lmtest)        
require(car)            
require(vtable)       

# Load data

load("C:/Users/asmam/OneDrive/SEMESTER TWO/FIE401/1. Assignment One/.RData")

# First look at the data

summary(CAR_MA)

# Winsorize continuous variables at 0.5%
# from both tails to deal with outliers

winsorize <- function(x, low = 0.005, high = 0.995) {
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

for (cn in c("deal.value", "bidder.car", "bidder.size",
             "bidder.sigma", "bidder.runup", "deal.relsize",
             "bidder.mtb", "bidder.fcf", "bidder.lev")) {
  CAR_MA[[cn]] <- winsorize(CAR_MA[[cn]])
}

# DESCRIPTIVE TABLE 1
# Number of deals, average deal size,
# share of private targets, share of
# all-stock deals — by year
# Count the number of deals per year

DescTable1 <- aggregate(CAR_MA$deal.value, list(CAR_MA$yyyy), FUN = length)

# Compute averages of deal.value, private, deal.allstock per year
Avg <- aggregate(CAR_MA[, c("deal.value", "private", "deal.allstock")],
                 list(CAR_MA$yyyy), FUN = mean)

# Merge count and averages into one table
DescTable1[, 3:5] <- Avg[, 2:4]

# Print with nice column names
kable(DescTable1,
      digits = 2,
      col.names = c("Year", "N", "Mean Deal Value ($ M)",
                    "Fraction of Private Targets",
                    "Fraction of All-Stock Deals"))
# DESCRIPTIVE TABLE 2
# Summary statistics for regression variables:
# N, mean, SD, and percentiles (10/25/50/75/90)
# Variables included in the regressions

required_var <- c("bidder.car", "deal.allstock", "public", "deal.relsize",
                  "bidder.size", "bidder.runup", "bidder.fcf", "bidder.lev",
                  "bidder.sigma", "hostile", "deal.tenderoffer")

sumtable(CAR_MA[, required_var],
         summ  = c('notNA(x)', 'mean(x)', 'sd(x)',
                   'pctile(x)[10]', 'pctile(x)[25]', 'pctile(x)[50]',
                   'pctile(x)[75]', 'pctile(x)[90]'),
         summ.names = c('N', 'Mean', 'SD', 'p10', 'p25', 'p50', 'p75', 'p90'),
         out = 'return')

# REGRESSION TABLE 1
# Bivariate regressions of bidder CAR
# on all-stock deal dummy
# Model 1: public targets only
fit.1 <- lm(bidder.car ~ deal.allstock,
            data = CAR_MA[CAR_MA$public == 1, ])

# Model 2: private targets only
fit.2 <- lm(bidder.car ~ deal.allstock,
            data = CAR_MA[CAR_MA$public == 0, ])

# Model 3: full sample with interaction term (deal.allstock x public)
fit.3 <- lm(bidder.car ~ deal.allstock * public,
            data = CAR_MA)

# Heteroscedasticity-robust standard errors (HC)
se.1 <- coeftest(fit.1, vcov = hccm)
se.2 <- coeftest(fit.2, vcov = hccm)
se.3 <- coeftest(fit.3, vcov = hccm)

# Regression table 1 (text output)
stargazer(fit.1, fit.2, fit.3,
          se           = list(se.1[, 2], se.2[, 2], se.3[, 2]),
          keep.stat    = c("n", "rsq", "adj.rsq"),
          report       = "vc*t",           # show coefficient, robust SE, t-stat
          type         = "text",
          title        = "Regression Table 1: Effect of Stock Payment on Bidder CAR (No Controls)",
          dep.var.labels = "Bidder's CAR",
          covariate.labels = c("All-stock deal",
                               "Public target",
                               "All-stock deal \u00D7 Public target"))

# REGRESSION TABLE 2
# Regressions of bidder CAR on all-stock
# deal dummy with controls
# Controls used in all four models (same set throughout):
#   deal.relsize    – scaled deal size (relative to bidder market cap)
#   log(bidder.size)– bidder size (log-transformed)
#   bidder.runup    – pre-announcement stock run-up (info leakage / overvaluation proxy)
#   bidder.fcf      – free cash flow (agency / financing constraint)
#   bidder.lev      – leverage (discipline / financing constraint)
#   bidder.sigma    – stock return volatility (noise in bidder price)
#   hostile         – hostile takeover dummy
#   deal.tenderoffer– tender offer dummy

# Model 1: public targets only, with controls
fit.1 <- lm(bidder.car ~ deal.allstock + deal.relsize + log(bidder.size) +
              bidder.runup + bidder.fcf + bidder.lev + bidder.sigma +
              hostile + deal.tenderoffer,
            data = CAR_MA[CAR_MA$public == 1, ])

# Model 2: private targets only, with controls
fit.2 <- lm(bidder.car ~ deal.allstock + deal.relsize + log(bidder.size) +
              bidder.runup + bidder.fcf + bidder.lev + bidder.sigma +
              hostile + deal.tenderoffer,
            data = CAR_MA[CAR_MA$public == 0, ])

# Model 3: full sample, interaction of deal.allstock with public, plus controls
fit.3 <- lm(bidder.car ~ deal.allstock * public + deal.relsize + log(bidder.size) +
              bidder.runup + bidder.fcf + bidder.lev + bidder.sigma +
              hostile + deal.tenderoffer,
            data = CAR_MA)

# Model 4: full sample, all controls interacted with public
# (allows every slope to differ between public and private sub-samples)
fit.4 <- lm(bidder.car ~ deal.allstock * public +
              deal.relsize    * public +
              log(bidder.size)* public +
              bidder.runup    * public +
              bidder.fcf      * public +
              bidder.lev      * public +
              bidder.sigma    * public +
              hostile         * public +
              deal.tenderoffer* public,
            data = CAR_MA)

# Heteroscedasticity-robust standard errors
se.1 <- coeftest(fit.1, vcov = hccm)
se.2 <- coeftest(fit.2, vcov = hccm)
se.3 <- coeftest(fit.3, vcov = hccm)
se.4 <- coeftest(fit.4, vcov = hccm)

# Regression table 2 (text output)
stargazer(fit.1, fit.2, fit.3, fit.4,
          se           = list(se.1[, 2], se.2[, 2], se.3[, 2], se.4[, 2]),
          keep.stat    = c("n", "rsq", "adj.rsq"),
          report       = "vc*t",
          type         = "text",
          title        = "Regression Table 2: Effect of Stock Payment on Bidder CAR (With Controls)",
          dep.var.labels = "Bidder's CAR",
          covariate.labels = c(
            "All-stock deal",
            "Public target",
            "Relative deal size",
            "Log bidder's size",
            "Bidder's run-up",
            "Bidder's free cash flow",
            "Bidder's leverage",
            "Bidder's stock volatility",
            "Hostile takeover",
            "Tender offer",
            "Public target \u00D7 All-stock deal",
            "Public target \u00D7 Relative deal size",
            "Public target \u00D7 Log bidder's size",
            "Public target \u00D7 Bidder's run-up",
            "Public target \u00D7 Bidder's free cash flow",
            "Public target \u00D7 Bidder's leverage",
            "Public target \u00D7 Bidder's stock volatility",
            "Public target \u00D7 Hostile takeover",
            "Public target \u00D7 Tender offer"
          ))
