# Principal Components Analysis
wages<-read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
#subwages <-wages[,c("IQ","KWW","educ")]
# exclude variable which is not from data 
Data1 <- wages

# missing value handling
a=NULL
b=NULL
c=NULL

for(i in 1:ncol(Data1)){
  a[i]=sum(is.na(Data1[,i]))
  b[i]=((a[i]/nrow(Data1))*100)
  c[i]=class(Data1[,i])
}

# In below data freame a column gives how many number of NA is present int perticular column
# In column b give percetage of NA in perticuar column
# In column C give data type of specific column
df=data.frame(names(Data1),a,b,c)

#There are only 3 column which have null values so we nremove it intsed of replace null with it's mean/median

# Create a subset of wages with just a few columns
principal.components <- prcomp(wages[,c(1:13,17)], retx=T, center=T, scale=T)
# Do a principal components analysis on just the columns in subwages
print(principal.components$rotation) # The weights on each variable
summary(principal.components) # The proportion of variance explained
plot(principal.components) # Graphical depiction of proportion of variance
sw <- cbind(wages,data.frame(principal.components$x))
# Add the new principal components as columns to a new data frame, sw
# (New so we don't muck up going back and running other stuff on wages now)
one.pc <- lm(wage ~ PC1, data= sw)
all.variables <- lm(wage ~ ., data= sw)
summary(one.pc)
summary(all.variables)
# If you see one principal component linear regression R^2 value is 0.1636 and now it 0.5913
# and with all values it's 1  
