#Durchlaufreihenfolge ist wichtig
library(qualityTools)

#2 Grafiken
windows(14, 7)
fracChoose()  #diese Grafik
rsmChoose() #diese Grafik


#3 Grafiken
windows(7,7)
example(paretoPlot) diese Grafiken
paretoPlot(vp, abs = F) #diese Grafik

#4 Grafiken
example(normalPlot) # diese Grafik

#1 Grafik
example(effectPlot) #diese nicht
effectPlot(fdo, classic = TRUE, col = "red") #diese Grafik

#2Grafiken
example(wirePlot)
#window schlieﬂen
wirePlot(A,B,y, data = fdo, form = "full") #diese Grafik
contourPlot(A,B,y, data = fdo, form = "full") #diese Grafik

#1 Grafik
example(desirability)
plot(d, col = "red") #diese Grafik

#1 Text wie in Grafik
example(optimum) #hier den output


#Text und Outpufdt wie in Grafik
windows(14,7)
set.seed(666)
example(gageRR)

#2 Grafiken
windows(7,7)
set.seed(6454654)
x = rnorm(25, 15)
pcr(x)

y = rweibull(25, 18, 3)
pcr(y, "weibull")

y = rexp(25, 18)
pcr(y, "exponential")


#4 ppPlot Grafiken
set.seed(542342)
x = rweibull(20, 8, 2)
ppPlot(x, "log-normal")
ppPlot(x, "weibull")
ppPlot(x, "exponential")
ppPlot(x, "normal")

#4 qqPlot Grafiken
set.seed(542342)
x = rweibull(20, 8, 2)
qqPlot(x, "log-normal")
qqPlot(x, "weibull")
qqPlot(x, "exponential")
qqPlot(x, "normal")

#1 Grafik
example(paretoChart) #diese Grafik

#1 Grafik
example(mvPlot)

#2 Grafiken
example(mixDesign)
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic")
wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic", theta = 190)
