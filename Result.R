# %% [code] {"_execution_state":"idle","execution":{"iopub.status.busy":"2022-04-20T13:26:59.702186Z","iopub.execute_input":"2022-04-20T13:26:59.744354Z","iopub.status.idle":"2022-04-20T13:27:01.422829Z"},"jupyter":{"outputs_hidden":false}}
# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load


# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "D:/UB/EAS 509 Stats2/Project")

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:30.156809Z","iopub.execute_input":"2022-04-20T00:01:30.187404Z","iopub.status.idle":"2022-04-20T00:01:36.91092Z"},"jupyter":{"outputs_hidden":false}}
packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot','ggfortify', 'gridExtra', 'forecast', 'tseries', 'TSA','tsibble' ,'tibble', 'TTR','tidyverse','zoo','readr')

my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T13:27:02.589465Z","iopub.execute_input":"2022-04-20T13:27:02.591314Z","iopub.status.idle":"2022-04-20T13:27:02.619355Z"},"jupyter":{"outputs_hidden":false}}
df_currency <- read.csv(file ="D:/UB/EAS 509 Stats2/Project/compiled.csv")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T13:58:12.603633Z","iopub.execute_input":"2022-04-20T13:58:12.604966Z","iopub.status.idle":"2022-04-20T13:58:12.716095Z"},"jupyter":{"outputs_hidden":false}}
M<-cor(df_currency[,8:12])
library(corrplot)
corrplot(M, method="circle")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:36.951382Z","iopub.execute_input":"2022-04-20T00:01:36.9535Z","iopub.status.idle":"2022-04-20T00:01:37.001929Z"},"jupyter":{"outputs_hidden":false}}
summary(df_currency)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.005395Z","iopub.execute_input":"2022-04-20T00:01:37.007015Z","iopub.status.idle":"2022-04-20T00:01:37.038321Z"},"jupyter":{"outputs_hidden":false}}
str(df_currency)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.0421Z","iopub.execute_input":"2022-04-20T00:01:37.044232Z","iopub.status.idle":"2022-04-20T00:01:37.074624Z"},"jupyter":{"outputs_hidden":false}}
df_currency$month <- as.Date(as.yearmon(df_currency$month))
summary(df_currency)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.078087Z","iopub.execute_input":"2022-04-20T00:01:37.079699Z","iopub.status.idle":"2022-04-20T00:01:37.107265Z"},"jupyter":{"outputs_hidden":false}}
str(df_currency)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.110797Z","iopub.execute_input":"2022-04-20T00:01:37.112436Z","iopub.status.idle":"2022-04-20T00:01:37.638319Z"},"jupyter":{"outputs_hidden":false}}

ggplot(df_currency, aes(USDCAD)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.640742Z","iopub.execute_input":"2022-04-20T00:01:37.642239Z","iopub.status.idle":"2022-04-20T00:01:37.772475Z"},"jupyter":{"outputs_hidden":false}}
inds <- seq(as.Date("2010-01-01"), as.Date("2021-12-01"), by = "month")

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(df_currency[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2010, as.numeric(format(inds[1], "%j"))),
       frequency = 12)
  return(i_ts)
}
i_ts = create_ts(which(colnames(df_currency) == "USDCAD"))
plot.ts(i_ts, xlab = "Time", ylab = "USDCAD value", main = "Time Series", col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.775227Z","iopub.execute_input":"2022-04-20T00:01:37.776694Z","iopub.status.idle":"2022-04-20T00:01:37.805261Z"},"jupyter":{"outputs_hidden":false}}
adf.test(df_currency[,which(colnames(df_currency) == "USDCAD")], alternative = "stationary", k = 0)
#not stationary

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.807716Z","iopub.execute_input":"2022-04-20T00:01:37.809129Z","iopub.status.idle":"2022-04-20T00:01:37.922626Z"},"jupyter":{"outputs_hidden":false}}
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:37.925144Z","iopub.execute_input":"2022-04-20T00:01:37.926624Z","iopub.status.idle":"2022-04-20T00:01:38.023195Z"},"jupyter":{"outputs_hidden":false}}
i_tsdiff1 <- diff(i_ts, differences=1)
plot.ts(i_tsdiff1, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:38.026056Z","iopub.execute_input":"2022-04-20T00:01:38.027801Z","iopub.status.idle":"2022-04-20T00:01:38.106639Z"},"jupyter":{"outputs_hidden":false}}
acf(i_tsdiff1, lag.max=60)             # plot a correlogram

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:38.10925Z","iopub.execute_input":"2022-04-20T00:01:38.110761Z","iopub.status.idle":"2022-04-20T00:01:38.127043Z"},"jupyter":{"outputs_hidden":false}}
acf(i_tsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:38.129518Z","iopub.execute_input":"2022-04-20T00:01:38.130959Z","iopub.status.idle":"2022-04-20T00:01:38.208484Z"},"jupyter":{"outputs_hidden":false}}
pacf(i_tsdiff1, lag.max=60)             # plot a partial correlogram

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:38.211088Z","iopub.execute_input":"2022-04-20T00:01:38.212715Z","iopub.status.idle":"2022-04-20T00:01:38.22952Z"},"jupyter":{"outputs_hidden":false}}
pacf(i_tsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:38.232039Z","iopub.execute_input":"2022-04-20T00:01:38.233536Z","iopub.status.idle":"2022-04-20T00:01:40.110257Z"},"jupyter":{"outputs_hidden":false}}
i_tsarima <- auto.arima(i_ts, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:40.114667Z","iopub.execute_input":"2022-04-20T00:01:40.117358Z","iopub.status.idle":"2022-04-20T00:01:40.259251Z"},"jupyter":{"outputs_hidden":false}}
i_tsforecasts <- forecast(i_tsarima, h = 12)
plot(i_tsforecasts, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:40.261765Z","iopub.execute_input":"2022-04-20T00:01:40.263375Z","iopub.status.idle":"2022-04-20T00:01:40.356455Z"},"jupyter":{"outputs_hidden":false}}
plot.ts(i_tsforecasts$residuals)            # make time plot of forecast errors

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:40.359049Z","iopub.execute_input":"2022-04-20T00:01:40.360467Z","iopub.status.idle":"2022-04-20T00:01:40.705093Z"},"jupyter":{"outputs_hidden":false}}
ggplot(data.frame(residuals = i_tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram


write_csv(fortify(i_tsforecasts, ts.connect = TRUE), "USDCAD_forecast.csv")


# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:40.707608Z","iopub.execute_input":"2022-04-20T00:01:40.709103Z","iopub.status.idle":"2022-04-20T00:01:40.974749Z"},"jupyter":{"outputs_hidden":false}}
ggplot(df_currency, aes(USDJPY)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:40.977351Z","iopub.execute_input":"2022-04-20T00:01:40.978816Z","iopub.status.idle":"2022-04-20T00:01:41.073464Z"},"jupyter":{"outputs_hidden":false}}
inds <- seq(as.Date("2010-01-01"), as.Date("2021-12-01"), by = "month")

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(df_currency[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2010, as.numeric(format(inds[1], "%j"))),
       frequency = 12)
  return(i_ts)
}
i_ts = create_ts(which(colnames(df_currency) == "USDJPY"))
plot.ts(i_ts, xlab = "Time", ylab = "USDJPY value", main = "Time Series", col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:41.075842Z","iopub.execute_input":"2022-04-20T00:01:41.077257Z","iopub.status.idle":"2022-04-20T00:01:41.094495Z"},"jupyter":{"outputs_hidden":false}}
adf.test(df_currency[,which(colnames(df_currency) == "USDJPY")], alternative = "stationary", k = 0)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:41.097364Z","iopub.execute_input":"2022-04-20T00:01:41.098936Z","iopub.status.idle":"2022-04-20T00:01:41.204556Z"},"jupyter":{"outputs_hidden":false}}
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:41.206836Z","iopub.execute_input":"2022-04-20T00:01:41.208214Z","iopub.status.idle":"2022-04-20T00:01:45.605298Z"},"jupyter":{"outputs_hidden":false}}
i_tsarima <- auto.arima(i_ts, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:45.60964Z","iopub.execute_input":"2022-04-20T00:01:45.612186Z","iopub.status.idle":"2022-04-20T00:01:45.748256Z"},"jupyter":{"outputs_hidden":false}}
i_tsforecasts <- forecast(i_tsarima, h = 12)
plot(i_tsforecasts, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:45.750599Z","iopub.execute_input":"2022-04-20T00:01:45.75206Z","iopub.status.idle":"2022-04-20T00:01:45.842355Z"},"jupyter":{"outputs_hidden":false}}
plot.ts(i_tsforecasts$residuals)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:45.844659Z","iopub.execute_input":"2022-04-20T00:01:45.84598Z","iopub.status.idle":"2022-04-20T00:01:46.103648Z"},"jupyter":{"outputs_hidden":false}}
ggplot(data.frame(residuals = i_tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram


write_csv(fortify(i_tsforecasts, ts.connect = TRUE), "USDJPY_forecast.csv")


# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:46.105955Z","iopub.execute_input":"2022-04-20T00:01:46.107336Z","iopub.status.idle":"2022-04-20T00:01:46.367668Z"},"jupyter":{"outputs_hidden":false}}
ggplot(df_currency, aes(USDMXN)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:46.369839Z","iopub.execute_input":"2022-04-20T00:01:46.37113Z","iopub.status.idle":"2022-04-20T00:01:46.457433Z"},"jupyter":{"outputs_hidden":false}}
inds <- seq(as.Date("2010-01-01"), as.Date("2021-12-01"), by = "month")

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(df_currency[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2010, as.numeric(format(inds[1], "%j"))),
       frequency = 12)
  return(i_ts)
}
i_ts = create_ts(which(colnames(df_currency) == "USDMXN"))
plot.ts(i_ts, xlab = "Time", ylab = "USDMXN value", main = "Time Series", col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:46.459571Z","iopub.execute_input":"2022-04-20T00:01:46.460898Z","iopub.status.idle":"2022-04-20T00:01:46.477214Z"},"jupyter":{"outputs_hidden":false}}
adf.test(df_currency[,which(colnames(df_currency) == "USDMXN")], alternative = "stationary", k = 0)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:46.479397Z","iopub.execute_input":"2022-04-20T00:01:46.480756Z","iopub.status.idle":"2022-04-20T00:01:46.588996Z"},"jupyter":{"outputs_hidden":false}}
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:46.591247Z","iopub.execute_input":"2022-04-20T00:01:46.592606Z","iopub.status.idle":"2022-04-20T00:01:47.862384Z"},"jupyter":{"outputs_hidden":false}}
i_tsarima <- auto.arima(i_ts, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:47.866618Z","iopub.execute_input":"2022-04-20T00:01:47.869212Z","iopub.status.idle":"2022-04-20T00:01:47.998994Z"},"jupyter":{"outputs_hidden":false}}
i_tsforecasts <- forecast(i_tsarima, h = 12)
plot(i_tsforecasts, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:48.001378Z","iopub.execute_input":"2022-04-20T00:01:48.002817Z","iopub.status.idle":"2022-04-20T00:01:48.081141Z"},"jupyter":{"outputs_hidden":false}}
plot.ts(i_tsforecasts$residuals)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:48.083394Z","iopub.execute_input":"2022-04-20T00:01:48.084822Z","iopub.status.idle":"2022-04-20T00:01:48.344043Z"},"jupyter":{"outputs_hidden":false}}
ggplot(data.frame(residuals = i_tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram


write_csv(fortify(i_tsforecasts, ts.connect = TRUE), "USDMXN_forecast.csv")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:48.346352Z","iopub.execute_input":"2022-04-20T00:01:48.347726Z","iopub.status.idle":"2022-04-20T00:01:48.599022Z"},"jupyter":{"outputs_hidden":false}}
ggplot(df_currency, aes(EURUSD)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:48.60203Z","iopub.execute_input":"2022-04-20T00:01:48.603594Z","iopub.status.idle":"2022-04-20T00:01:48.693277Z"},"jupyter":{"outputs_hidden":false}}
inds <- seq(as.Date("2010-01-01"), as.Date("2021-12-01"), by = "month")

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(df_currency[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2010, as.numeric(format(inds[1], "%j"))),
       frequency = 12)
  return(i_ts)
}
i_ts = create_ts(which(colnames(df_currency) == "EURUSD"))
plot.ts(i_ts, xlab = "Time", ylab = "EURUSD value", main = "Time Series", col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:48.695822Z","iopub.execute_input":"2022-04-20T00:01:48.69726Z","iopub.status.idle":"2022-04-20T00:01:48.714622Z"},"jupyter":{"outputs_hidden":false}}
adf.test(df_currency[,which(colnames(df_currency) == "EURUSD")], alternative = "stationary", k = 0)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:48.717615Z","iopub.execute_input":"2022-04-20T00:01:48.719238Z","iopub.status.idle":"2022-04-20T00:01:48.830696Z"},"jupyter":{"outputs_hidden":false}}
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:48.833173Z","iopub.execute_input":"2022-04-20T00:01:48.834649Z","iopub.status.idle":"2022-04-20T00:01:49.723905Z"},"jupyter":{"outputs_hidden":false}}
i_tsarima <- auto.arima(i_ts, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:49.728237Z","iopub.execute_input":"2022-04-20T00:01:49.73088Z","iopub.status.idle":"2022-04-20T00:01:49.866092Z"},"jupyter":{"outputs_hidden":false}}
i_tsforecasts <- forecast(i_tsarima, h = 12)
plot(i_tsforecasts, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:49.868538Z","iopub.execute_input":"2022-04-20T00:01:49.870046Z","iopub.status.idle":"2022-04-20T00:01:49.974162Z"},"jupyter":{"outputs_hidden":false}}
plot.ts(i_tsforecasts$residuals)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:49.976437Z","iopub.execute_input":"2022-04-20T00:01:49.9778Z","iopub.status.idle":"2022-04-20T00:01:50.237897Z"},"jupyter":{"outputs_hidden":false}}
ggplot(data.frame(residuals = i_tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram


write_csv(fortify(i_tsforecasts, ts.connect = TRUE), "EURUSD_forecast.csv")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:50.240197Z","iopub.execute_input":"2022-04-20T00:01:50.241546Z","iopub.status.idle":"2022-04-20T00:01:50.506073Z"},"jupyter":{"outputs_hidden":false}}
ggplot(df_currency, aes(USDKRW)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:50.508417Z","iopub.execute_input":"2022-04-20T00:01:50.509797Z","iopub.status.idle":"2022-04-20T00:01:50.602242Z"},"jupyter":{"outputs_hidden":false}}
inds <- seq(as.Date("2010-01-01"), as.Date("2021-12-01"), by = "month")

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(df_currency[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2010, as.numeric(format(inds[1], "%j"))),
       frequency = 12)
  return(i_ts)
}
i_ts = create_ts(which(colnames(df_currency) == "USDKRW"))
plot.ts(i_ts, xlab = "Time", ylab = "USDKRW value", main = "Time Series", col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:50.60459Z","iopub.execute_input":"2022-04-20T00:01:50.606023Z","iopub.status.idle":"2022-04-20T00:01:50.624276Z"},"jupyter":{"outputs_hidden":false}}
adf.test(df_currency[,which(colnames(df_currency) == "USDKRW")], alternative = "stationary", k = 0)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:50.626628Z","iopub.execute_input":"2022-04-20T00:01:50.628023Z","iopub.status.idle":"2022-04-20T00:01:50.743588Z"},"jupyter":{"outputs_hidden":false}}
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:50.747335Z","iopub.execute_input":"2022-04-20T00:01:50.749685Z","iopub.status.idle":"2022-04-20T00:01:52.116524Z"},"jupyter":{"outputs_hidden":false}}
i_tsarima <- auto.arima(i_ts, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:52.121035Z","iopub.execute_input":"2022-04-20T00:01:52.123804Z","iopub.status.idle":"2022-04-20T00:01:52.263959Z"},"jupyter":{"outputs_hidden":false}}
i_tsforecasts <- forecast(i_tsarima, h = 12)
plot(i_tsforecasts, col = "red")

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:52.266252Z","iopub.execute_input":"2022-04-20T00:01:52.267636Z","iopub.status.idle":"2022-04-20T00:01:52.362732Z"},"jupyter":{"outputs_hidden":false}}
plot.ts(i_tsforecasts$residuals)

# %% [code] {"execution":{"iopub.status.busy":"2022-04-20T00:01:52.364936Z","iopub.execute_input":"2022-04-20T00:01:52.366232Z","iopub.status.idle":"2022-04-20T00:01:52.620311Z"},"jupyter":{"outputs_hidden":false}}
ggplot(data.frame(residuals = i_tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram


write_csv(fortify(i_tsforecasts, ts.connect = TRUE), "USDKRW_forecast.csv")


