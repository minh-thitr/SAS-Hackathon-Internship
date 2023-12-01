
library(tinytex)
library(lattice)
library("readxl")
library(rio)
library(corrplot)

iyv <- import_list("/Users/thitruong/Desktop/Illinois Tech/Internship/Demos-0311/Invest+Your+Values+shareclass+results+20221216.xlsx")


# returns variables
return1y <- iyv$Shareclasses$`Returns and fees: Month end trailing returns, 1 year`
return5y <- iyv$Shareclasses$`Returns and fees: Month end trailing returns, 5 year`
return10y <- iyv$Shareclasses$`Returns and fees: Month end trailing returns, 10 year`
return20y <- iyv$Shareclasses$`Returns and fees: Month end trailing returns, 20 year`
returnInception <- iyv$Shareclasses$`Returns and fees: Month end trailing returns, since inception`

# esg variables
fossil_fuel <- as.numeric(as.factor(iyv$Shareclasses$`Fossil Free Funds: Fossil fuel grade`))
fossil_fuel_fin <- as.numeric(as.factor(iyv$Shareclasses$`Fossil Free Funds: Fossil fuel finance grade`))
fossil_fuel_ins <- as.numeric(as.factor(iyv$Shareclasses$`Fossil Free Funds: Fossil fuel insurance grade`))
deforestation <- as.numeric(as.factor(iyv$Shareclasses$`Deforestation Free Funds: Deforestation grade`))
gender <- as.numeric(as.factor(iyv$Shareclasses$`Gender Equality Funds: Gender equality grade`))
gunfree_civillian <- as.numeric(as.factor(iyv$Shareclasses$`Gun Free Funds: Civilian firearm grade`))
prison_indus <- as.numeric(as.factor(iyv$Shareclasses$`Prison Free Funds: Prison industrial complex grade`))
weapon <- as.numeric(as.factor(iyv$Shareclasses$`Weapon Free Funds: Military weapon grade`))
tobacco <- as.numeric(as.factor(iyv$Shareclasses$`Tobacco Free Funds: Tobacco grade`))

ticker <- iyv$Shareclasses$`Fund profile: Ticker`


esg_rating_table <- data.frame(fossil_fuel,
                               fossil_fuel_fin,
                               fossil_fuel_ins,
                               deforestation,
                               gender,
                               gunfree_civillian,
                               prison_indus,
                               weapon,
                               tobacco)

returns_table <- data.frame(return1y,return5y,return10y,return20y)

# compute correlations
corr_esg <- cor(esg_rating_table,use="complete.obs") # corr between esg
corr_returns <- cor(returns_table,use="complete.obs") #corr between returns
corr_esg_returns <- cor(esg_rating_table,returns_table,use="complete.obs")

# making a plot of esg correlation
corrplot(corr_esg,
         method = "color",
         addCoef.col = "#7F0000",
         number.cex = 0.55,
         number.digits = 2,
         title = " ESG Correlations",
         mar = c(1,1,1,1),
         outline = "black")

# making a plot of returns correlation
corrplot(corr_returns,
         method = "color",
         addCoef.col = "red",
         number.cex = 1,
         number.digits = 3,
         title = " Returns Correlations",
         mar = c(1,1,1,1),
         outline = "black")

# marking a plot correlations esg and returns
corrplot(corr_esg_returns,
         method = "color",
         addCoef.col = "red",
         number.cex = 0.55,
         number.digits = 3,
         title = "Correlations Between ESG and Returns",
         outline = "black")

