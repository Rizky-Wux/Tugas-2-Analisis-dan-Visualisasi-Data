install.packages("ggplot2") 
install.packages("dplyr") 
install.packages("broom") 
install.packages("ggpubr") 
library(ggplot2) 
library(dplyr) 
library(broom) 
library(ggpubr)
data<-read.csv("D:/Tugas UT/RIZKY/Analisis dan Visualisasi Data/data.pendapatan- tugas tutorial 2.csv", header = TRUE)
View(data)
#a.
hist(data$kebahagiaan,
     main="Histogram kebahagiaan",
     xlab="Tingkat kebahagiaan",
     ylab="Frekuensi",
     col="lightgreen",
     border="black")

#b.
plot(data$pendapatan, data$kebahagiaan,
     main = "Scatter Plot: Hubungan Pendapatan dan Kebahagiaan",
     xlab = "Pendapatan",
     ylab = "Kebahagiaan",
     col = "red",
     pch = 19)

abline(lm(kebahagiaan ~ pendapatan, data = data),
       col = "black",
       lwd = 2)

#c.
model <- lm(kebahagiaan ~ pendapatan, data = data)
summary_model <- summary(model)
print(summary_model)

#d.
par(mfrow=c(2, 2))
plot(model)
#e.
par(mfrow=c(1, 1))
plot(data$pendapatan, data$kebahagiaan,
     main="Regresi Linear: Pengaruh Pendapatan terhadap Kebahagiaan",
     xlab = "Pendapatan",
     ylab="Kebahagiaan",
     col="red",
     pch=19)
abline(model, col="black", lwd=2)
coef_model<-coef(model)
eq<-paste("y =", round(coef_model[1], 3), "+", round(coef_model[2], 3), "x")
r2<-paste("R2 =", round(summary_model$r.squared, 3))
text(x=min(data$pendapatan),
     y=max(data$kebahagiaan),
     labels=r2,
     pos=4,
     col="red")
text(x=min(data$pendapatan),
     y=max(data$kebahagiaan)-0.5,
     labels=r2,
     pos=4,
     col="red")