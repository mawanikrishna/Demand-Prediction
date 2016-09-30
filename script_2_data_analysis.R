setwd("~/Personal/R-Work/Demand Prediction")

#load the libraries
#install.packages("data.table")
#install.packages("DataCombine")
library(data.table)
library(DataCombine)

##########################################################################
#this function takes a vector and  convert it into numeric vector 
#by trimming white spaces and removing COmma
##########################################################################
remove_comma <- function(x = NULL){
  x <- trimws(x)
  out <-  as.numeric(gsub(pattern = ",",replacement = "",x = x))
  out
}

##########################################################################
#this function takes a dataframe and the list of columns 
# that it has to create lag on and creates new comuns with lag 
##########################################################################
calculate_lag_columns <- function(x, columns_to_lag, lag_try = -3){
  for(i in columns_to_lag){
    for(j in lag_try:-1){
      x <- slide(data = x,Var = i,slideBy = j)
    }
  }
  x
}

##########################################################################
#this function takes Y variable and a dataframe of x variables 
# and computes the max correlating x variables against y variable.
#this returns the column names of the x variables which is highly correlated 
##########################################################################
best_correlation <- function(y, x_i_i)
{
  corr_df = as.data.frame(t(cor(y,x_i_i,use = "complete.obs")))
  max_corr=max(abs(corr_df))
  max_corr_col_name <-row.names(corr_df)[abs(corr_df$V1) == max_corr]
  max_corr_col_name
}

###Script starts here..
xy <- fread(input = "xy_cleaned.csv")
xy <- as.data.frame(xy)


#convert data to numeric
for(i in 2:ncol(xy)){
  xy[,i] <- remove_comma(xy[,i])
}

#create a data frame with all new lag columns
columns_to_lag <- names(xy)[3:length(names(xy))]
lag_try = -3
xy_lag <- calculate_lag_columns(x = xy,columns_to_lag = columns_to_lag,lag_try = lag_try)
write.csv(x = xy_lag,file = "xy_lag.csv",row.names = F)


## Krishna.. see if you can remove hard coding from the position on the variables.. 
## take hint from other function calls above..
## we need to do this because if we remove any columns from the dataset then we will have to change the function...
## i think we will be removing few columns from the dataset from visual inspection
#find the best correlating variables
xy_cleaned=read.csv("xy_cleaned.csv")
col_count=ncol(xy_cleaned)
col_count

x_i=3
df=data.frame()
max_corr_names <- NULL
m=x_i+col_count-1

for(i in 1:col_count-2)
{
  df = NULL
  df[1]<- xy_lag[x_i]
  n=abs(lag_try)

    for(j in 1:n)
    {   
      df[j+1] =(xy_lag[m])
      m=m+1
    }
  #max_corr_names[i] <- best_correlation(y = xy_lag$y,x_i_i = df) 
  x_i=x_i+1
}
#subset the xy_lag dataframe with the columns with max correlation
xy_best <- xy_lag[c("y",max_corr_names)]
write.csv(x = xy_best,file = "xy_best.csv",row.names = F)

