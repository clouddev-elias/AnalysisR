day1 <- read.delim("SolData/2015-08-31.txt", header = TRUE, sep = "\t", dec = "." )
day2 <- read.delim("SolData/2015-09-01.txt", header = TRUE, sep = "\t", dec = "." )
day3 <- read.delim("SolData/2015-09-02.txt", header = TRUE, sep = "\t", dec = "." )
day4 <- read.delim("SolData/2015-09-03.txt", header = TRUE, sep = "\t", dec = "." )
day5 <- read.delim("SolData/2015-09-04.txt", header = TRUE, sep = "\t", dec = "." )
day6 <- read.delim("SolData/2015-09-05.txt", header = TRUE, sep = "\t", dec = "." )
day7 <- read.delim("SolData/2015-09-06.txt", header = TRUE, sep = "\t", dec = "." )
# -----------------------
# SolData used as y-param - avg per day
SolData <- data.frame(mean(day1$SolData),mean(day2$SolData),mean(day3$SolData),
                        mean(day4$SolData),mean(day5$SolData),mean(day6$SolData),
                       mean(day7$SolData))
# Average soldata
sol_avg <- mean(as.vector(t(SolData[1,])))
# Standard deviation x for for continues data
std_sol <- sd(as.vector(t(SolData[1,])))*(sqrt((length(SolData)-1)/length(SolData)))
# -----------------------
# Average Pmax for 7d, for panel 1 # RS Solar # mono-Si
p1_days <- data.frame(mean(day1$Pmax.a.Si),mean(day2$Pmax.a.Si),mean(day3$Pmax.a.Si),
                           mean(day4$Pmax.a.Si),mean(day5$Pmax.a.Si),mean(day6$Pmax.a.Si),
                           mean(day7$Pmax.a.Si)) 
# avg pmax for panel
p1_avg <- mean(as.vector(t(p1_days[1,])))
# Correlation between x and y
correlation_p1 <- cor(as.vector(t(p1_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p1 <- sd(as.vector(t(p1_days[1,])))*(sqrt((length(p1_days)-1)/length(p1_days)))
# bx slope for linear 
slope1 <- correlation_p1 * (std_p1/std_sol)
# a intercept1 for linear
intercept1 <- p1_avg - (slope1 * sol_avg)
eq_p1 = function(x){intercept1+slope1*x}
print(intercept1+slope1*800)
print(intercept1+slope1*400)
#curve(eq, from=sol_avg-0.5, to=sol_avg, xlab="x", ylab="y")
# -----------------------
p2_days <- data.frame(mean(day1$Pmax.459),mean(day2$Pmax.459),mean(day3$Pmax.459),
                           mean(day4$Pmax.459),mean(day5$Pmax.459),mean(day6$Pmax.459),
                           mean(day7$Pmax.459))
# avg pmax for panel
p2_avg <- mean(as.vector(t(p2_days[1,])))
# Correlation between x and y
correlation_p2 <- cor(as.vector(t(p2_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p2 <- sd(as.vector(t(p2_days[1,])))*(sqrt((length(p2_days)-1)/length(p2_days)))
# bx slope for linear 
slope2 <- correlation_p2 * (std_p2/std_sol)
# a intercept2 for linear
intercept2 <- p2_avg - (slope2 * sol_avg)
eq_p2 = function(x){intercept2+slope2*x}
print(intercept2+slope2*800)
print(intercept2+slope2*400)
# ----------------------
p3_days <- data.frame(mean(day1$Pmax.422),mean(day2$Pmax.422),mean(day3$Pmax.422),
                           mean(day4$Pmax.422),mean(day5$Pmax.422),mean(day6$Pmax.422),
                           mean(day7$Pmax.422))
# avg pmax for panel
p3_avg <- mean(as.vector(t(p3_days[1,])))
# Correlation between x and y
correlation_p3 <- cor(as.vector(t(p3_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p3 <- sd(as.vector(t(p3_days[1,])))*(sqrt((length(p3_days)-1)/length(p3_days)))
# bx slope3 for linear 
slope3 <- correlation_p3 * (std_p3/std_sol)
# a intercept3 for linear
intercept3 <- p3_avg - (slope3 * sol_avg)
eq_p3 = function(x){intercept3+slope3*x}
print(intercept3+slope3*800)
# -----------------------
p4_days <- data.frame(mean(day1$Pmax.423),mean(day2$Pmax.423),mean(day3$Pmax.423),
                           mean(day4$Pmax.423),mean(day5$Pmax.423),mean(day6$Pmax.423),
                           mean(day7$Pmax.423))
# avg pmax for panel
p4_avg <- mean(as.vector(t(p4_days[1,])))
# Correlation between x and y
correlation_p4 <- cor(as.vector(t(p4_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p4 <- sd(as.vector(t(p4_days[1,])))*(sqrt((length(p4_days)-1)/length(p4_days)))
# bx slope4 for linear 
slope4 <- correlation_p4 * (std_p4/std_sol)
# a intercept4 for linear
intercept4 <- p4_avg - (slope4 * sol_avg)
eq_p4 = function(x){intercept4+slope4*x}
print(intercept4+slope4*800)
# -----------------------
p5_days <- data.frame(mean(day1$Pmax.433),mean(day2$Pmax.433),mean(day3$Pmax.433),
                           mean(day4$Pmax.433),mean(day5$Pmax.433),mean(day6$Pmax.433),
                           mean(day7$Pmax.433))
# avg pmax for panel
p5_avg <- mean(as.vector(t(p5_days[1,])))
# Correlation between x and y
correlation_p5 <- cor(as.vector(t(p5_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p5 <- sd(as.vector(t(p5_days[1,])))*(sqrt((length(p5_days)-1)/length(p5_days)))
# bx slope5 for linear 
slope5 <- correlation_p5 * (std_p5/std_sol)
# a intercept5 for linear
intercept5 <- p5_avg - (slope5 * sol_avg)
eq_p5 = function(x){intercept5+slope5*x}
print(intercept5+slope5*800)
print(intercept5+slope5*400)
# -----------------------
p6_days <- data.frame(mean(day1$Pmax.GPV),mean(day2$Pmax.GPV),mean(day3$Pmax.GPV),
                           mean(day4$Pmax.GPV),mean(day5$Pmax.GPV),mean(day6$Pmax.GPV),
                           mean(day7$Pmax.GPV))
# avg pmax for panel
p6_avg <- mean(as.vector(t(p6_days[1,])))
# Correlation between x and y
correlation_p6 <- cor(as.vector(t(p6_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p6 <- sd(as.vector(t(p6_days[1,])))*(sqrt((length(p6_days)-1)/length(p6_days)))
# bx slope6 for linear 
slope6 <- correlation_p6 * (std_p6/std_sol)
# a intercept6 for linear
intercept6 <- p6_avg - (slope6 * sol_avg)
eq_p6 = function(x){intercept6+slope6*x}
print(intercept6+slope6*800)
# -----------------------
p7_days <- data.frame(mean(day1$Pmax.A10156),mean(day2$Pmax.A10156),mean(day3$Pmax.A10156),
                           mean(day4$Pmax.A10156),mean(day5$Pmax.A10156),mean(day6$Pmax.A10156),
                           mean(day7$Pmax.A10156))
# avg pmax for panel
p7_avg <- mean(as.vector(t(p7_days[1,])))
# Correlation between x and y
correlation_p7 <- cor(as.vector(t(p7_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p7 <- sd(as.vector(t(p7_days[1,])))*(sqrt((length(p7_days)-1)/length(p7_days)))
# bx slope7 for linear 
slope7 <- correlation_p7 * (std_p7/std_sol)
# a intercept7 for linear
intercept7 <- p7_avg - (slope7 * sol_avg)
eq_p7 = function(x){intercept7+slope7*x}
print(intercept7+slope7*800)
# -----------------------
p8_days <- data.frame(mean(day1$Pmax.A10160),mean(day2$Pmax.A10160),mean(day3$Pmax.A10160),
                           mean(day4$Pmax.A10160),mean(day5$Pmax.A10160),mean(day6$Pmax.A10160),
                           mean(day7$Pmax.A10160))
# avg pmax for panel
p8_avg <- mean(as.vector(t(p8_days[1,])))
# Correlation between x and y
correlation_p8 <- cor(as.vector(t(p8_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p8 <- sd(as.vector(t(p8_days[1,])))*(sqrt((length(p8_days)-1)/length(p8_days)))
# bx slope8 for linear 
slope8 <- correlation_p8 * (std_p8/std_sol)
# a intercept8 for linear
intercept8 <- p8_avg - (slope8 * sol_avg)
eq_p8 = function(x){intercept8+slope8*x}
print(intercept8+slope8*800)
# -----------------------
p9_days <- data.frame(mean(day1$Pmax.CIS),mean(day2$Pmax.CIS),mean(day3$Pmax.CIS),
                           mean(day4$Pmax.CIS),mean(day5$Pmax.CIS),mean(day6$Pmax.CIS),
                           mean(day7$Pmax.CIS))
# avg pmax for panel
p9_avg <- mean(as.vector(t(p9_days[1,])))
# Correlation between x and y
correlation_p9 <- cor(as.vector(t(p9_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p9 <- sd(as.vector(t(p9_days[1,])))*(sqrt((length(p9_days)-1)/length(p9_days)))
# bx slope9 for linear 
slope9 <- correlation_p9 * (std_p9/std_sol)
# a intercept9 for linear
intercept9 <- p9_avg - (slope9 * sol_avg)
print(intercept9+slope9*800)
eq_p9 = function(x){intercept9+slope9*x}
# -----------------------
p10_days <- data.frame(mean(day1$Pmax.NESTE),mean(day2$Pmax.NESTE),mean(day3$Pmax.NESTE),
                      mean(day4$Pmax.NESTE),mean(day5$Pmax.NESTE),mean(day6$Pmax.NESTE),
                      mean(day7$Pmax.NESTE))
# avg pmax for panel
p10_avg <- mean(as.vector(t(p10_days[1,])))
# Correlation between x and y
correlation_p10 <- cor(as.vector(t(p10_days[1,])),as.vector(t(SolData[1,])))
# Standard deviation y for for continues data
std_p10 <- sd(as.vector(t(p10_days[1,])))*(sqrt((length(p10_days)-1)/length(p10_days)))
# bx slope9 for linear 
slope10 <- correlation_p10 * (std_p10/std_sol)
# a intercept9 for linear
intercept10 <- p10_avg - (slope10 * sol_avg)
print(intercept10+slope10*800)
eq_p10 = function(x){intercept10+slope10*x}
# -----------------------
fun1<-eq_p1
fun2<-eq_p2
fun3<-eq_p3
fun4<-eq_p4
fun5<-eq_p5
fun6<-eq_p6
fun7<-eq_p7
fun8<-eq_p8
fun9<-eq_p9
fun10<-eq_p10
x<-seq(sol_avg-std_sol,sol_avg+std_sol)
matplot(x,cbind(fun1(x),fun2(x),fun3(x),fun4(x),fun5(x),fun6(x),fun7(x),fun8(x),fun9(x),fun10(x)),type="l",col=c("blue","red","green","grey","black","purple","darkgrey","darkgreen","orange","black"),lty = 1:90, lwd = 1)


