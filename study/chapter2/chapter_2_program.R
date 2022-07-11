getwd()
setwd("/Users/yooseungli/Downloads/데청캠/오프라인 수업/220711/chapter2")
library(rstudioapi)
curPosition <- rstudioapi::getActiveDocumentContext()$path
curDir <- dirname(curPosition)
setwd(curDir)

#Predictive Model for LA Dodgers Promotion and Attendance

library(car) #special functions for linear regression
library(lattice) #graphics package

#read in data and create a data frame called dodgers
dodgers <-read.csv("/Users/yooseungli/Downloads/데청캠/오프라인 수업/220711/chapter2/dodgers.csv")
print(str(dodgers)) #check the structure of the data frame

# Set Plot margin
# http://blog.naver.com/padosori60/220803108652
par("mar")
par("mfrow")
par("mfcol")
par(mar = c(5, 4, 3, 2) + 0.1) #margin default
par(mar = c(7, 8, 7, 7))
a<-runif(29, 0.4)
hist(a, border = F, col = rgb(0.2, 0.2, 0.8, 0.7), main = "testMain", ylab = "testY", xlab = "testX")
mtext(c("Margin1", "Margin2", "Margin3", "Margin4"), at = c(0.7, 0.22, 0.7, 1.1),
      line = c(-12, -4, 2, -4), col = "darkblue")

par(mfrow = c(2,2))
par(mar = c(1, 2, 2, 1))
plot(0:5, 0:5, main = "mar = c(1, 2, 2, 1)")
par(mar = c(1, 1, 1, 1))
plot(0:5, 0:5, main = "mar = c(1, 1, 1, 1)")
par(mar = c(3, 2, 2, 1))
plot(0:5, 0:5, main = "mar = c(3, 2, 2, 1)")
par(mar = c(4, 3, 2, 1))
plot(0:5, 0:5, main = "mar = c(4, 3, 2, 1)")
par(mfrow = c(1, 1))

# EDA
head(dodgers)
dodgers[which(dodgers$attend == max(dodgers$attend)),]
dodgers[which(dodgers$cpa == "YES"),]
dodgers[which(dodgers$shirt == "YES"),]
dodgers[which(dodgers$fireworks == "YES"),]
dodgers[which(dodgers$bobblehead == "YES"),]

max(dodgers[which(dodgers$day_of_week == "Monday"),]$attend)

#define an ordered day-of-week variable
# for plots and data summaries
dodgers$ordered_day_of_week <- with(data = dodgers,
                                    ifelse ((day_of_week == "Monday"),1,
                                    ifelse ((day_of_week == "Tuesday"),2,
                                    ifelse ((day_of_week == "Wednesday"),3,
                                    ifelse ((day_of_week == "Thursday"),4,
                                    ifelse ((day_of_week == "Friday"),5,
                                    ifelse ((day_of_week == "Saturday"),6,7)))))))
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels = 1:7,
                                      labels = c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

# exploratory data analysis with standard graphics: attendance by day of week
with(data = dodgers, plot(ordered_day_of_week, attend/1000,
                          xlab = "Day of Week", ylab = "Attendance (thousands)",
                          col = "violet", las = 1))

# when do the Dodgers use bobblehead promotions
with(dodgers, table(bobblehead, ordered_day_of_week)) # bobbleheads on Tuesday

# define an ordered month variable
# for plots and data summaries
dodgers$ordered_month <- with(data = dodgers,
                              ifelse ((month == "APR"), 4,
                              ifelse ((month == "MAY"), 5,
                              ifelse ((month == "JUN"), 6,
                              ifelse ((month == "JUL"), 7,
                              ifelse ((month == "AUG"), 8,
                              ifelse ((month == "SEP"), 9, 10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels = 4:10,
                                labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

# EDA with standard R graphics: attendance by month
with(data = dodgers, plot(ordered_month, attend/1000, xlab = "Month",
                          ylab = "Attendance", col = "light blue", las = 1))

# EDA displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
library(lattice) #used for plotting
# let us prepare a graphical summary of the dodgers data
group.labels <- c("No Fireworks", "Fireworks")
group.symbols <- c(21, 24)
group.colors <- c("black", "red")
group.fill <- c("black", "red")
xyplot(attend/1000 ~ temp | skies + day_night,
       data = dodgers, groups = fireworks, pch = group.symbols,
       aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
       layout = c(2,2), type = c("p", "g"),
       strip = strip.custom(strip.levels = TRUE, strip.names = FALSE, style = 1),
       xlab = "Temperature (Degrees Fahernheit)",
       ylab = "Attendance (thousands)",
       key = list(space= "top",
                  text = list(rev(group.labels), col = rev(group.colors),
                              fill = rev(group.fill))))
# https://homerhanumat.github.io/tigerstats/xyplot.html

# attendance by opponent and day/night game
group.labels <- c("Day", "Night")
group.symbols <- c(1, 20)
group.symbols.size <- c(2, 2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night,
       xlab = "Attendance (thousands)", 
       panel = function(x, y, groups, subscripts,...)
         {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
         panel.stripplot(x, y, groups = groups, subscripts = subscripts,
         cex = group.symbols.size, pch = group.symbols, col = "darkblue")
         },
       key = list(space = "top",
                  text = list(group.labels, col = "black"),
                  points = list(pch = group.symbols, cex = group.symbols.size,
                                col = "darkblue")))
# https:/homerhanumat.github.io/tigerstats/bwbplot.html

# example for xyplot, bwplot
install.packages("tigerstats", dependencies = T)
library(tigerstats)
xyplot(fastest ~ GPA, data = m111survey,
       groups = sex,
       auto.key = TRUE,
       par.settings = list(superpose.symbol = list(col = c("blue", "red"),
                                                   pch = 19),
                           superpose.line = list(col = c("blue", "red"),
                                                 lwd = 2)),
       xlab = "grade point averge",
       ylab = "speed (mph)",
       main = "Fastest Speed Ever Driven, ₩nby Grade Point Average",
       type = c("p", "smooth"))
bwplot(~fastest, data = m111survey,
       xlab = "speed(mph)",
       main = "Fastest Speed Ever Driven")
Height <- equal.count(m111survey$height, number = 2, overlap = 0.1)
bwplot(sex ~ fastest | Height * seat,
       data= m111survey,
       layout = c(2, 3))

# specify a simple model with bobblehed entered last
my.model <- {attend ~ ordered_month + ordered_day_of_week+ bobblehead}

#employ a training-and-test regimen
set.seed(1234) # set seed for repeatability of training-and-test split
training_test <- c(rep(1, length = trunc((2/3)*nrow(dodgers))),
                  rep(2, length = (nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))
dodgers$training_test <- sample(training_test) #random permutation
dodgers$training_test <- factor(dodgers$training_test,
                                levels = c(1, 2), labels = c("TRAIN", "TEST"))
dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) #check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test))

# fit the model to the training set
train.model.fit <- lm(my.model, data = dodgers.train)
#obtain predictions from the training set
dodgers.train$predict_attend <- predict(train.model.fit)

# evaluate the fitted model on the test set
dodgers.test$predict_attend <- predict(train.model.fit,
                                       newdata = dodgers.test)

# compute the proportion of response variance
# accounted for when predicting out-of-sample
cat("₩n", "Proportion of Test Set Variance Accounted for: ",
    round((with(dodgers.test, cor(attend, predict_attend)^2)),
          digits = 3), "₩n", sep = "")

# cor, cov, var : package "Matrices"
cov(dodgers.test$attend, dodgers.test$predict_attend)
var(dodgers.test$attend, dodgers.test$predict_attend)
cor(dodgers.test$attend, (dodgers.test$predict_attend)^2, method = "pearson")
cor(dodgers.test$attend, (dodgers.test$predict_attend)^2, method = "spearman")

# correlation - pearson, spearman, kendall : package "Hmisc"
install.packages("Hmisc")
library(Hmisc)
m<-matrix(c(1:10, (1:10)^2), ncol = 2)
rcorr(m, type = "pearson")$r
rcorr(m, type = "spearman")$r

a <- c(1:5)
b <- c(1, 0, 3, 4, 5)
cor.test(a, b, method = "pearson")
cor.test(a, b, method = "spearman")
cor.test(a, b, method = "kendall")

# merge the training and test sets for plotting
dodgers.plotting.frame <- rbind(dodgers.train, dodgers.test)

# Export Dataframe to excel
install.packages("writexl", dependencies = T)
library(writexl)
write_xlsx(dodgers.plotting.frame, "dodgers.plotting.frame.xlsx")
# Test : 27 /Train : 54

#generate predictive modeling visual for management
group.labels <- c("No Bobbleheads", "Bobbleheads")
group.symbols <- c("black", "black")
group.colors <- c("black", "red")
xyplot(predict_attend/1000 ~ attend/1000 | training_test,
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill,
       layout = c(2, 1), xlim = c(20, 65), ylim = c(20, 65),
       aspect = 1, type = c("p", "g"),
       panel = function(x, y, ...)
         {panel.xyplot(x, y, ...)
         panel.segments(25, 25, 60, 60, col = "black", cex = 2)
         },
       strip = function(...) strip.default(..., style = 1),
       xlab = "Actual Attendance (thousands)",
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top",
                  text = list(rev(group.labels), col = rev(group.colors)),
                  points = list(pch = rev(group.symbols),
                                col = rev(group.colors),
                                fill = rev(group.fill))))

# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors
my.model.fit <- lm(my.model, data = dodgers) # use all available data
print(summary(my.model.fit))
# tests statistics significance of the bobblehead promotion
# type | anova computes sums of squares for sequential test
print(anova(my.model.fit))
summary(aov(attend ~ bobblehead, dodgers))
anova(lm(attend ~ bobblehead, dodgers))

cat("₩n", "Estimated Effect of Bobblehead Promotion on Attendance: ",
    round(my,model.fit$coefficients[length(my.model.fit$coefficients)],
          digits = 0), "₩n", sep = "")

par(mfrow = c(2, 2))
# standard graphics provide diagnostic plots
plot(my.model.fit)
par(mfrow = c(1, 1))
# SE QQ plot check - Quantile-Quantile Plot ()
cooks.distance(my.model.fit)
cooks.distance(my.model.fit)[cooks.distance(my.model.fit) > 4 / length(dodgers$bobblehead)]
#
durbinWatsonTest(my.model.fit)
#shapiro wilk
shapiro.test((dodgers.plotting.frame$attend - dodgers.plotting.frame$predict_attend)^2)
#
leveneTest(dodgers$attend ~ dodgers$bobblehead, dodgers)

par("mar")
par(mar = c(1, 1, 1, 1))

par(mfrow = c(1, 2), mar = c(1, 3, 3, 1))
par(mar = c(1, 1, 1, 1))

# additional model diagnostics drawn from the car packages
library(car)
residualPlots(my.model.fit)
marginalModelPlot(my.model.fit, sd = F)
# marginalModelPlot(my.model.fit, sd = T)
# 등분산성 확인이 필요함
print(outlierTest(my.model.fit))

