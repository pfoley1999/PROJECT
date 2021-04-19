df <- Business.Analytics.Data...Sheet1
df$isDoc <- as.integer(df$Ownership.Model == "Doc")
View(df)
summary(df)

cor(df[2],df[3])
cor(df[2],df[4])
cor(df[2],df[6])
cor(df[2],df[7])
cor(df[2],df[9])

cov(df[2],df[3])
cov(df[2],df[4])
cov(df[2],df[6])
cov(df[2],df[7])
cov(df[2],df[9])

hist(df$Total.Income, prob = TRUE, main=" ", xlab=" ", ylab=" ")
curve(dnorm(x, mean = mean(df$Total.Income), sd = sd(df$Total.Income)), col = "darkblue", lwd = 2, add = TRUE)
title(main = "Histogram of Total Income", 
      xlab = "Total Income", ylab = "Density")

install.packages('sm')
library(sm)
options(scipen=5)
sm.density.compare(df$Total.Income, df$Ownership.Model,main=" ", xlab=" ", ylab=" ") 
legend("topright", legend=c("Owned by Doctor", "Owned by Investor", "Owned by Company"), 
       col=c("red", "green","blue"), lty=1:2, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black")
title(main = "Total Income Density", 
      xlab = "Total Income", ylab = "Density")

plot(df$Total.Income~df$Chiropractic.Visits, main=" ", xlab=" ", ylab=" ",
     col=factor(df$Ownership.Model))
legend("topleft", legend=c("Owned by Doctor", "Owned by Investor", "Owned by Company"),
       col=c("red", "green", "black"), pch=1, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black")
abline(lm(df$Total.Income~df$Chiropractic.Visits), col="blue")
title(main = "Total Income vs. Chiropractic Visits", 
      xlab = "Chiropractic Visits", ylab = "Total Income")

library(ggplot2)
p <- ggplot(df, aes(Total.Income)) +
   geom_boxplot() +
   facet_wrap(~isDoc)
theme_update(plot.title = element_text(hjust = 0.5))
p + ggtitle("Income by Ownership") +
   xlab("Total Income($)")

plot(df$Chiropractic.Visits~df$Patient.Referrals, main=" ", xlab=" ", ylab=" ", xlim=c(0,80))
abline(lm(df$Chiropractic.Visits~df$Patient.Referrals), col="red")
title(main = "Chiropractic Visits vs. Patient Referrals", 
      xlab = "Patient Referrals", ylab = "Chiropractic Visits")    

hist(df$Ownership.Model~df$Chiropractic.Visits, prob = TRUE, main=" ", xlab=" ", ylab=" ")
title(main = "Ownership Model vs Chiropractic Visits", 
      xlab = "Ownership Model ", ylab = "Chiropractic Visits")

