setwd("../case3")
credit <- read.csv("creditcard.csv")

head(credit)
tail(credit)
str(credit)
summary(credit)

# Check outcome
credit$default.payment.next.month <- ifelse(credit$default.payment.next.month == 1, "YES", "NO")

table(credit$default.payment.next.month)
# inblanced data
barplot(table(credit$default.payment.next.month))

# Delete ID
work.df <- credit[, -c(1,2)]
summary(work.df)

# for easier use
colnames(work.df)[24] <- "STATUS"
work.df

library(caret)
fitControl <- trainControl(method = "cv",
                           number = 10,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE, savePredictions = T, sampling = "smote")

set.seed(1)
c.model.1 <- train(STATUS ~., data=work.df, method="rf", trControl=fitControl)
c.model.1

# Use domain knowledge
# define a new feature find a group of people
work.df$gfs <- ifelse(work.df$LIMIT_BAL > 400000 & work.df$AGE < 35, 1, 0)
sum(work.df$gfs)

work.df$limit1 <- (work.df$BILL_AMT1-work.df$PAY_AMT1)/work.df$LIMIT_BAL
work.df$limit2 <- (work.df$BILL_AMT2-work.df$PAY_AMT2)/work.df$LIMIT_BAL
work.df$limit3 <- (work.df$BILL_AMT3-work.df$PAY_AMT3)/work.df$LIMIT_BAL
work.df$limit4 <- (work.df$BILL_AMT4-work.df$PAY_AMT4)/work.df$LIMIT_BAL
work.df$limit5 <- (work.df$BILL_AMT5-work.df$PAY_AMT5)/work.df$LIMIT_BAL
work.df$limit6 <- (work.df$BILL_AMT6-work.df$PAY_AMT6)/work.df$LIMIT_BAL

set.seed(1)
c.model.2 <- train(STATUS ~., data=work.df, method="rf", trControl=fitControl)
c.model.2
