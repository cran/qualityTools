#line 171 "qualityTools.rnw"
library(qualityTools)


#line 176 "qualityTools.rnw"
#create artificial defect data set
defects = c(rep("E", 62), rep("B", 15), rep("F", 3), rep("A", 10),
rep("C",20), rep("D", 10))
paretoChart(defects)


#line 257 "qualityTools.rnw"
x = c(9.991, 10.013, 10.001, 10.007, 10.010, 10.013, 10.008, 10.017, 10.005,
10.005, 10.002, 10.017, 10.005, 10.002,  9.996, 10.011, 10.009, 10.006,
10.008, 10.003, 10.002, 10.006, 10.010, 9.992, 10.013)
cg(x, target = 10.003, tolerance = c(9.903, 10.103))


#line 283 "qualityTools.rnw"
#create a gage RnR design
design = gageRRDesign(Operators=3, Parts=10, Measurements=2, randomize=FALSE)
#set the response	
response(design) = c(23,22,22,22,22,25,23,22,23,22,20,22,22,22,24,25,27,28,
23,24,23,24,24,22,22,22,24,23,22,24,20,20,25,24,22,24,21,20,21,22,21,22,21,
21,24,27,25,27,23,22,25,23,23,22,22,23,25,21,24,23)
#perform Gage RnR
gdo = gageRR(design)


#line 295 "qualityTools.rnw"
#visualization of Gage RnR
plot(gdo)


#line 301 "qualityTools.rnw"
#line 295 "qualityTools.rnw#from line#301#starts at#294#"
#visualization of Gage RnR
plot(gdo)
#line 296 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 302 "qualityTools.rnw"


#line 382 "qualityTools.rnw"
set.seed(1234)
#generate some data
norm = rnorm(20, mean = 20)
#generate some data
weib = rweibull(20, shape = 2, scale = 8)
#process capability
pcr(norm, "normal", lsl = 17, usl = 23)


#line 391 "qualityTools.rnw"
#process cabapility
pcr(weib, "weibull", usl = 20)


#line 401 "qualityTools.rnw"
#line 382 "qualityTools.rnw#from line#401#starts at#381#"
set.seed(1234)
#generate some data
norm = rnorm(20, mean = 20)
#generate some data
weib = rweibull(20, shape = 2, scale = 8)
#process capability
pcr(norm, "normal", lsl = 17, usl = 23)
#line 388 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 402 "qualityTools.rnw"


#line 407 "qualityTools.rnw"
#line 391 "qualityTools.rnw#from line#407#starts at#390#"
#process cabapility
pcr(weib, "weibull", usl = 20)
#line 392 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 408 "qualityTools.rnw"


#line 420 "qualityTools.rnw"
pcr(weib, "weibull", usl = 20)


#line 426 "qualityTools.rnw"
par(mfrow = c(1,2))
qqPlot(weib, "weibull"); qqPlot(weib, "normal")


#line 432 "qualityTools.rnw"
#line 426 "qualityTools.rnw#from line#432#starts at#425#"
par(mfrow = c(1,2))
qqPlot(weib, "weibull"); qqPlot(weib, "normal")
#line 427 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 433 "qualityTools.rnw"


#line 441 "qualityTools.rnw"
par(mfrow = c(1,2))
ppPlot(norm, "weibull"); ppPlot(norm, "normal")


#line 447 "qualityTools.rnw"
#line 441 "qualityTools.rnw#from line#447#starts at#440#"
par(mfrow = c(1,2))
ppPlot(norm, "weibull"); ppPlot(norm, "normal")
#line 442 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 448 "qualityTools.rnw"


#line 486 "qualityTools.rnw"
set.seed(1234)
fdo = facDesign(k = 3, centerCube = 4) #fdo - factorial design object
names(fdo) = c("Factor 1", "Factor 2", "Factor 3") #optional
lows(fdo) = c(80, 120, 1) #optional
highs(fdo) = c(120, 140, 2) #optional
summary(fdo) #information about the factorial design


#line 498 "qualityTools.rnw"
#set first value
yield = simProc(x1 = 80, x2 = 120, x3 = 1)


#line 505 "qualityTools.rnw"
yield = c(simProc(80,120, 1),simProc(120,120, 2),simProc(120,140, 2),
simProc(80,140, 1),simProc(120,120, 1),simProc(120,140, 1),
simProc(80,120, 2),simProc(80,140, 2), simProc(90,130, 1.5),
simProc(90,130, 1.5),simProc(90,130, 1.5), simProc(90,130, 1.5))


#line 514 "qualityTools.rnw"
response(fdo) = yield	#assign yield to the factorial design object


#line 520 "qualityTools.rnw"
effectPlot(fdo, classic = TRUE)


#line 523 "qualityTools.rnw"
interactionPlot(fdo)


#line 532 "qualityTools.rnw"
#line 520 "qualityTools.rnw#from line#532#starts at#519#"
effectPlot(fdo, classic = TRUE)
#line 520 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 533 "qualityTools.rnw"


#line 538 "qualityTools.rnw"
#line 523 "qualityTools.rnw#from line#538#starts at#522#"
interactionPlot(fdo)
#line 523 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 539 "qualityTools.rnw"


#line 551 "qualityTools.rnw"
lm.1 = lm(yield ~ A*B*C, data = fdo)
summary(lm.1)


#line 559 "qualityTools.rnw"
paretoPlot(fdo)


#line 564 "qualityTools.rnw"
#line 559 "qualityTools.rnw#from line#564#starts at#558#"
paretoPlot(fdo)
#line 559 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 565 "qualityTools.rnw"


#line 573 "qualityTools.rnw"
par(mfrow = c(1,2))
wirePlot(A, B, yield, data = fdo)
contourPlot(A, B, yield, data = fdo)


#line 580 "qualityTools.rnw"
#line 573 "qualityTools.rnw#from line#580#starts at#572#"
par(mfrow = c(1,2))
wirePlot(A, B, yield, data = fdo)
contourPlot(A, B, yield, data = fdo)
#line 575 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 581 "qualityTools.rnw"


#line 597 "qualityTools.rnw"
fdo.frac = fracDesign(k = 3, gen = "C = AB", centerCube = 4)


#line 603 "qualityTools.rnw"
summary(fdo.frac)


#line 617 "qualityTools.rnw"
aliasTable(fdo.frac)


#line 623 "qualityTools.rnw"
fracChoose()


#line 628 "qualityTools.rnw"
#line 623 "qualityTools.rnw#from line#628#starts at#622#"
fracChoose()
#line 623 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 629 "qualityTools.rnw"


#line 639 "qualityTools.rnw"
fdo1 = facDesign(k = 3, centerCube = 2, replicates = 2)


#line 647 "qualityTools.rnw"
set.seed(1234)
y2 = rnorm(12, mean = 20)
response(fdo) = data.frame(yield, y2) 


#line 655 "qualityTools.rnw"
par(mfrow = c(1,2))
wirePlot(A, B, yield, data = fdo, form = "yield~A+B+C+A*B")
contourPlot(A, B, y2, data = fdo, form = "y2~A+B+C+A*B")


#line 662 "qualityTools.rnw"
#line 655 "qualityTools.rnw#from line#662#starts at#654#"
par(mfrow = c(1,2))
wirePlot(A, B, yield, data = fdo, form = "yield~A+B+C+A*B")
contourPlot(A, B, y2, data = fdo, form = "y2~A+B+C+A*B")
#line 657 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 663 "qualityTools.rnw"


#line 671 "qualityTools.rnw"
par(mfrow = c(1,2))
wirePlot(A,B,y2, data = fdo, factors = list(C=-1), form = "y2~A*B*C")
wirePlot(A,B,y2, data = fdo, factors = list(C=1), form = "y2~A*B*C")


#line 678 "qualityTools.rnw"
#line 671 "qualityTools.rnw#from line#678#starts at#670#"
par(mfrow = c(1,2))
wirePlot(A,B,y2, data = fdo, factors = list(C=-1), form = "y2~A*B*C")
wirePlot(A,B,y2, data = fdo, factors = list(C=1), form = "y2~A*B*C")
#line 673 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 679 "qualityTools.rnw"


#line 687 "qualityTools.rnw"
fits(fdo) = lm(yield ~ A+B, data = fdo)
fits(fdo) = lm(y2 ~ A*B*C, data = fdo)
fits(fdo)


#line 697 "qualityTools.rnw"
sao =steepAscent(factors=c("A","B"),response="yield",data=fdo,steps=20)
sao


#line 704 "qualityTools.rnw"
predicted = simProc(sao[,5], sao[,6])
response(sao) = predicted 
plot(sao, type = "b", col = 2)


#line 711 "qualityTools.rnw"
#line 704 "qualityTools.rnw#from line#711#starts at#703#"
predicted = simProc(sao[,5], sao[,6])
response(sao) = predicted 
plot(sao, type = "b", col = 2)
#line 706 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 712 "qualityTools.rnw"


#line 726 "qualityTools.rnw"
#set the seed for randomization of the runs
set.seed(1234)
fdo2 = facDesign(k = 2, centerCube = 3)
names(fdo2) = c("Factor 1", "Factor 2") 
lows(fdo2) = c(134, 155)
highs(fdo2) = c(155, 175)


#line 737 "qualityTools.rnw"
yield = c(simProc(134,155), simProc(155,155), simProc(134,175),
simProc(155,175),simProc(144,165), simProc(144,165), simProc(144,165))
response(fdo2) = yield


#line 747 "qualityTools.rnw"
rsdo = starDesign(data = fdo2)


#line 753 "qualityTools.rnw"
yield2 = c(yield, simProc(130,165), simProc(149,165), simProc(144,151),
simProc(144,179),simProc(144,165),simProc(144,165),simProc(144,165))
response(rsdo) = yield2


#line 761 "qualityTools.rnw"
lm.3 = lm(yield2 ~ A*B + I(A^2) + I(B^2), data = rsdo)


#line 767 "qualityTools.rnw"
par(mfrow=c(1,2))
wirePlot(A,B,yield2,form="yield2~A*B+I(A^2)+I(B^2)",data=rsdo,theta=-70)
contourPlot(A,B,yield2,form="yield2~A*B+I(A^2)+I(B^2)",data=rsdo)


#line 774 "qualityTools.rnw"
#line 767 "qualityTools.rnw#from line#774#starts at#766#"
par(mfrow=c(1,2))
wirePlot(A,B,yield2,form="yield2~A*B+I(A^2)+I(B^2)",data=rsdo,theta=-70)
contourPlot(A,B,yield2,form="yield2~A*B+I(A^2)+I(B^2)",data=rsdo)
#line 769 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 775 "qualityTools.rnw"


#line 784 "qualityTools.rnw"
A = seq(40, 210, length = 100)
B = seq(90, 190, length = 100)
C = seq(90, 190, length = 100)
filled.contour(A, B,outer(A,B, simProc, noise = FALSE), xlab = "Factor 1", ylab = "Factor 2", color.palette = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))) 


#line 798 "qualityTools.rnw"
fdo = rsmDesign(k = 3, alpha = 1.633, cc = 0, cs = 6)


#line 804 "qualityTools.rnw"
fdo = randomize(fdo, so = TRUE)


#line 810 "qualityTools.rnw"
rsdo = rsmChoose()


#line 815 "qualityTools.rnw"
#line 810 "qualityTools.rnw#from line#815#starts at#809#"
rsdo = rsmChoose()
#line 810 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 816 "qualityTools.rnw"


#line 826 "qualityTools.rnw"
fdo3 = facDesign(k = 6)
rsdo = starDesign(alpha = "orthogonal", data = fdo3)


#line 837 "qualityTools.rnw"
randomize(fdo, random.seed = 123)


#line 875 "qualityTools.rnw"
d1 = desirability(y1, 120, 170, scale = c(1,1), target = "max")
d3 = desirability(y3, 400, 600, target = 500)
d1


#line 883 "qualityTools.rnw"
par(mfrow = c(1,2))
plot(d1, col = 2); plot(d3, col = 2)


#line 889 "qualityTools.rnw"
#line 883 "qualityTools.rnw#from line#889#starts at#882#"
par(mfrow = c(1,2))
plot(d1, col = 2); plot(d3, col = 2)
#line 884 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 890 "qualityTools.rnw"


#line 903 "qualityTools.rnw"
ddo = rsmDesign(k = 3, alpha = 1.633, cc = 0, cs = 6)
ddo = randomize(ddo, so = TRUE)
#optional
names(ddo) = c("silica", "silan", "sulfur")
#optional
highs(ddo) = c(1.7, 60, 2.8)
#optional
lows(ddo) = c(0.7, 40, 1.8)


#line 916 "qualityTools.rnw"
y1 = c(102, 120, 117, 198, 103, 132, 132, 139, 102, 154, 96, 163, 116,
 153, 133, 133, 140, 142, 145, 142)
y2 = c(900, 860, 800, 2294, 490, 1289, 1270, 1090, 770, 1690, 700, 1540,
 2184, 1784, 1300, 1300, 1145, 1090, 1260, 1344)
y3 = c(470, 410, 570, 240, 640, 270, 410, 380, 590, 260, 520, 380, 520,
 290, 380, 380, 430, 430, 390, 390)
y4 = c(67.5, 65, 77.5, 74.5, 62.5, 67, 78, 70, 76, 70, 63, 75, 65, 71,
 70, 68.5, 68, 68, 69, 70)


#line 929 "qualityTools.rnw"
response(ddo) = data.frame(y1, y2, y3, y4)[c(5,2,3,8,1,6,7,4,9:20),]


#line 935 "qualityTools.rnw"
d2 = desirability(y2, 1000, 1300, target = "max")
d4 = desirability(y4, 60, 75, target = 67.5)


#line 942 "qualityTools.rnw"
desires(ddo)=d1; desires(ddo)=d2; desires(ddo)=d3; desires(ddo)=d4


#line 948 "qualityTools.rnw"
fits(ddo) = lm(y1 ~ A+B+C+A:B+A:C+B:C+I(A^2)+I(B^2)+I(C^2), data = ddo)
fits(ddo) = lm(y2 ~ A+B+C+A:B+A:C+B:C+I(A^2)+I(B^2)+I(C^2), data = ddo)
fits(ddo) = lm(y3 ~ A+B+C+A:B+A:C+B:C+I(A^2)+I(B^2)+I(C^2), data = ddo)
fits(ddo) = lm(y4 ~ A+B+C+A:B+A:C+B:C+I(A^2)+I(B^2)+I(C^2), data = ddo)


#line 957 "qualityTools.rnw"
optimum(ddo, type = "optim")


#line 967 "qualityTools.rnw"
mdo = mixDesign(3,2, center = FALSE, axial = FALSE, randomize = FALSE,
replicates  = c(1,1,2,3))
names(mdo) = c("polyethylene", "polystyrene", "polypropylene")

#set response (i.e. yarn elongation)
elongation = c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0,
10.0, 9.7, 11.8, 16.8, 16.0)  
response(mdo) = elongation


#line 980 "qualityTools.rnw"
mdo


#line 986 "qualityTools.rnw"
par(mfrow=c(1,2))
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic")
wirePlot3(A, B, C, elongation, data=mdo, form="quadratic", theta=-170)


#line 993 "qualityTools.rnw"
#line 986 "qualityTools.rnw#from line#993#starts at#985#"
par(mfrow=c(1,2))
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic")
wirePlot3(A, B, C, elongation, data=mdo, form="quadratic", theta=-170)
#line 988 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 994 "qualityTools.rnw"


#line 1012 "qualityTools.rnw"
set.seed(1234)
tdo = taguchiDesign("L9_3")
values(tdo) = list(A  = c(20, 40, 60), B = c("mateial 1", "material 2",
"material 3"), C = c(1,2,3))
names(tdo) = c("Factor 1", "Factor 2", "Factor 3", "Factor 4")
summary(tdo)


#line 1023 "qualityTools.rnw"
response(tdo) = rnorm(9)
effectPlot(tdo, col = 2)


#line 1029 "qualityTools.rnw"
#line 1023 "qualityTools.rnw#from line#1029#starts at#1022#"
response(tdo) = rnorm(9)
effectPlot(tdo, col = 2)
#line 1024 "#end named chunk#"
invisible(.Last.value) # End of chunk marker
#line 1030 "qualityTools.rnw"


#line 1038 "qualityTools.rnw"
sessionInfo()


