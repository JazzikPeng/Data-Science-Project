rm(list = setdiff(ls(), lsf.str()))

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

require(xgboost)
library(readr)

# Import 9 sectors datasets 
XLE <- read_csv("~/Desktop/XGBoost_Long-Short/XLE.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLB <- read_csv("~/Desktop/XGBoost_Long-Short/XLB.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLK <- read_csv("~/Desktop/XGBoost_Long-Short/XLK.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLY <- read_csv("~/Desktop/XGBoost_Long-Short/XLY.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLF <- read_csv("~/Desktop/XGBoost_Long-Short/XLF.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLI <- read_csv("~/Desktop/XGBoost_Long-Short/XLI.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLV <- read_csv("~/Desktop/XGBoost_Long-Short/XLV.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLU <- read_csv("~/Desktop/XGBoost_Long-Short/XLU.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))
XLP <- read_csv("~/Desktop/XGBoost_Long-Short/XLP.csv", 
                col_types = cols_only(`Adj Close` = col_guess(), Date = col_guess()))

# initialize logreturn
XLB$logreturn = 0
XLE$logreturn = 0
XLF$logreturn = 0
XLI$logreturn = 0
XLK$logreturn = 0
XLP$logreturn = 0
XLU$logreturn = 0
XLV$logreturn = 0
XLY$logreturn = 0

# calculate daily log return for each ETF
for(i in 2:nrow(XLB)){
  XLB$logreturn[i] = log(XLB$`Adj Close`[i] / XLB$`Adj Close`[i-1])
  XLE$logreturn[i] = log(XLE$`Adj Close`[i] / XLE$`Adj Close`[i-1])
  XLF$logreturn[i] = log(XLF$`Adj Close`[i] / XLF$`Adj Close`[i-1])
  XLI$logreturn[i] = log(XLI$`Adj Close`[i] / XLI$`Adj Close`[i-1])
  XLK$logreturn[i] = log(XLK$`Adj Close`[i] / XLK$`Adj Close`[i-1])
  XLP$logreturn[i] = log(XLP$`Adj Close`[i] / XLP$`Adj Close`[i-1])
  XLU$logreturn[i] = log(XLU$`Adj Close`[i] / XLU$`Adj Close`[i-1])
  XLV$logreturn[i] = log(XLV$`Adj Close`[i] / XLV$`Adj Close`[i-1])
  XLY$logreturn[i] = log(XLY$`Adj Close`[i] / XLY$`Adj Close`[i-1])
}
  

# Read other 8 Macro Factor Data
DTWEXB <- read_csv("~/Desktop/XGBoost_Long-Short/DTWEXB.csv")
OIL <- read_csv("~/Desktop/XGBoost_Long-Short/OIL.csv")
GOLD <- read_csv("~/Desktop/XGBoost_Long-Short/GOLD.csv")
CESIUSD <- read_csv("~/Desktop/XGBoost_Long-Short/CESIUSD.csv")
YIELD_SPRD <- read_csv("~/Desktop/XGBoost_Long-Short/Yield_Spread.csv")
CREDIT_SPRD <- read_csv("~/Desktop/XGBoost_Long-Short/Credit_Spread.csv")
colnames(DTWEXB)[1] = 'Dates'

# Calculate Macro Log Return
# CESIUSD should not be logreturn, it's stationary, we should just normalize it
CESIUSD$CESI_lr = 0
plot(CESIUSD$CESIUSD, type = 'l')
CESIUSD$CESI_lr = scale(CESIUSD$CESIUSD) # Scale to mean 0, sd = 1
#for(i in 2:nrow(CESIUSD))
#{
#  CESIUSD$CESI_lr[i] = log(CESIUSD$CESIUSD[i] / CESIUSD$CESIUSD[i-1])
#}

CREDIT_SPRD$IG_lr = 0
CREDIT_SPRD$HY_lr = 0
CREDIT_SPRD$`CDX IG CDSI GEN 5Y Corp` = as.numeric(CREDIT_SPRD$`CDX IG CDSI GEN 5Y Corp`)
CREDIT_SPRD$`CDX HY CDSI GEN 5Y PRC Corp` = as.numeric(CREDIT_SPRD$`CDX HY CDSI GEN 5Y PRC Corp`)
# 1745 is the first Non NAN Data!
for(i in 1746:nrow(CREDIT_SPRD))
{
  CREDIT_SPRD$IG_lr[i] = log(CREDIT_SPRD$`CDX IG CDSI GEN 5Y Corp`[i] / CREDIT_SPRD$`CDX IG CDSI GEN 5Y Corp`[i-1])
  CREDIT_SPRD$HY_lr[i] = log(CREDIT_SPRD$`CDX HY CDSI GEN 5Y PRC Corp`[i] / CREDIT_SPRD$`CDX HY CDSI GEN 5Y PRC Corp`[i-1])
}

DTWEXB$D_lr = 0
for(i in 2:nrow(DTWEXB))
{
  DTWEXB$D_lr[i] = log(DTWEXB$DTWEXB[i] / DTWEXB$DTWEXB[i-1])
}

GOLD$GOLD_lr = 0
GOLD$PX_MID = as.numeric(GOLD$PX_MID)
for(i in 2:nrow(GOLD))
{
  GOLD$GOLD_lr[i] = log(GOLD$PX_MID[i] / GOLD$PX_MID[i-1])
}

OIL$OIL_lr = 0
for(i in 2:nrow(OIL))
{
  OIL$OIL_lr[i] = log(OIL$PX_MID[i] / OIL$PX_MID[i-1])
}

YIELD_SPRD$YIELD_lr = 0
for(i in 2:nrow(YIELD_SPRD))
{
  YIELD_SPRD$YIELD_lr[i] = log(YIELD_SPRD$`Y10-Y2 Spread`[i] / YIELD_SPRD$`Y10-Y2 Spread`[i-1])
}

# Merge Data Frame 
colnames(XLB)[3] = 'XLB_lr'
colnames(XLE)[3] = 'XLE_lr'
colnames(XLF)[3] = 'XLF_lr'
colnames(XLI)[3] = 'XLI_lr'
colnames(XLK)[3] = 'XLK_lr'
colnames(XLP)[3] = 'XLP_lr'
colnames(XLU)[3] = 'XLU_lr'
colnames(XLV)[3] = 'XLV_lr'
colnames(XLY)[3] = 'XLY_lr'

df = data.frame(XLB$Date, XLB$XLB_lr, XLE$XLE_lr, XLF$XLF_lr, XLI$XLI_lr, XLK$XLK_lr, XLP$XLP_lr,
                XLU$XLU_lr, XLV$XLV_lr, XLY$XLY_lr)

colnames(df)[1] = 'Dates'

# Clean up data, by deleting unnecessary rows
CESIUSD = CESIUSD[,-2]
CREDIT_SPRD = CREDIT_SPRD[,-c(2,3)]
DTWEXB = DTWEXB[,-2]
GOLD = GOLD[,-2]
OIL = OIL[,-2]
YIELD_SPRD = YIELD_SPRD[,-c(2,3,4)]

# Merge Macro Data's logreturn by ETF Index Date:
df = merge(df, CESIUSD, by = 'Dates')
df = merge(df, CREDIT_SPRD, by= 'Dates')
df = merge(df, DTWEXB, by = 'Dates')
df = merge(df, GOLD, by = 'Dates')
df = merge(df, OIL, by = 'Dates')
df = merge(df, YIELD_SPRD, by = 'Dates')

# Now adjust the dataframe so that the previous day of input match against the output
# c(2:10) are dependen variables, c(11:17) are independed variables
df2 <- df
df2[1:nrow(df2)-1,c(2:10)] = df[2:nrow(df2), c(2:10)]
df2 = df2[-nrow(df2),]
# Now Perform xgb algorithm to predict 
xgb <- function(col)
{
  param =list('objective' = "reg:linear", 'max.depth' = sample(c(1:7),1), 'num_class' = 1, "eval_metric" = "rmse")
  n = 2608
  xgtrain = data.matrix(df2[1:n, 11:17])
  num_rounds = 150
  
  # Cross validation
  model2 = xgb.cv(params = param, xgtrain,
                  nrounds = num_rounds,nfold=5,
                  label = data.matrix(df2[1:n,col]),
                  verbose = FALSE, early_stopping_rounds=25, maximize=FALSE)
  
  plot(log(model2$evaluation_log$test_rmse_mean), type="l")
  
  
  model <- xgboost(xgtrain,label = data.matrix(df2[1:n,col]), params=param,
                   nrounds=num_rounds, nthread=10,verbose = FALSE)
  preds = predict(model, data.matrix(df2[(n+1):nrow(df2),11:17]))
  plot(log(model$evaluation_log$train_rmse), type="l")
  return(preds)
}
# Predict each of nine sectors outcome
XLB_Pred = xgb(which( colnames(df)=="XLB.XLB_lr"))
XLE_Pred = xgb(which( colnames(df)=='XLE.XLE_lr'))
XLF_Pred = xgb(which( colnames(df)=="XLF.XLF_lr"))
XLI_Pred = xgb(which( colnames(df)=="XLI.XLI_lr"))
XLK_Pred = xgb(which( colnames(df)=="XLK.XLK_lr"))
XLP_Pred = xgb(which( colnames(df)=="XLP.XLP_lr"))
XLU_Pred = xgb(which( colnames(df)=="XLU.XLU_lr"))
XLV_Pred = xgb(which( colnames(df)=="XLV.XLV_lr"))
XLY_Pred = xgb(which( colnames(df)=="XLY.XLY_lr"))

# Now we have all the predictions lets construct the long-short strategy
# Select the top expected return stocks and bottom 3 expected return stocks
n = 2608
Dates=df2$Dates[(n+1):nrow(df2)]
pred_return = data.frame(Dates, XLB_Pred, XLE_Pred, XLF_Pred, XLI_Pred, XLK_Pred, XLP_Pred, 
                         XLU_Pred, XLV_Pred, XLY_Pred)
long_short = data.frame(Dates, XLB_Pred, XLE_Pred, XLF_Pred, XLI_Pred, XLK_Pred, XLP_Pred, 
                        XLU_Pred, XLV_Pred, XLY_Pred)
long_short[c(2,3,4,5,6,7,8,9,10)] = 0
# label 1 to long， -1 is short, 0 is do not purchase.
for(i in 1:651)
{
  temp = sort(pred_return[i,2:ncol(pred_return)])
  # Botton 3 Short
  long_short[i, c(colnames(temp[1]),colnames(temp[2]), colnames(temp[3]))] = -1
  # Top 3 long
  long_short[i, c(colnames(temp[7]), colnames(temp[8]), colnames(temp[9]))] = 1

}

# Now long_short is our strategy, lets put it back to test our strategy
# Assume the long side and short side have the same Weight. We will nto introduce ETF price in our 
# Calculation. We only consider returns. Thus, 1 represent weight instead of number of shares
total_return = 1
pnl = c()
daily_lr = c()
for(i in 1:nrow(long_short))
{
  # divide by 6 because you long 3 and short 3. With equal weight, each return have weight 1/6
  r = sum(long_short[i,2:10] * df[n+i, 2:10]) / 6
  daily_lr[i] = r
  #cat('========',r,"\n")
  pnl[i] = total_return
  total_return = total_return*exp(r)
}

# Read SP500 returns and calculate covariance
SP500 <- read_csv("~/Desktop/XGBoost_Long-Short/SP500.csv", 
                  col_types = cols_only(`Adj Close` = col_guess(), 
                                        Date = col_guess()))
SP500$lr = 0
for(i in 2:nrow(SP500))
{
  SP500$lr[i] = log(SP500$`Adj Close`[i] / SP500$`Adj Close`[i-1])
}
rho = cor(daily_lr, SP500$lr[2:nrow(SP500)])

cat("Annualized return is ", (total_return)^(252/651) -1)
# Multiply volatility return by sqrt(252) to anualized it 
cat("Annualized Volatiltiy is: ", sd(daily_lr)*sqrt(252))
cat("Sharp Ratio is ", ((total_return)^(252/651) -1 )/(sd(daily_lr)*sqrt(252)))
cat("Correlation with SP500 is ", rho)

plot(SP500$Date[2:652],pnl*100, type = 'l',xlab = "Date", ylab = "pnl")
  
  

