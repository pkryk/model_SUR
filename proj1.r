wig_banki_m <- read_excel("wig_banki_m.xlsx")
wig_budow_m <- read_excel("wig_budow_m.xlsx")
wig_chemia_m <- read_excel("wig_chemia_m.xlsx")
wig_info_m <- read_excel("wig_info_m.xlsx")
wig_media_m <- read_excel("wig_media_m.xlsx")
wig_nrchom_m <- read_excel("wig_nrchom_m.xlsx")
wig_paliwa_m <- read_excel("wig_paliwa_m.xlsx")
wig_spozyw_m <- read_excel("wig_spozyw_m.xlsx")
wig_telkom_m <- read_excel("wig_telkom_m.xlsx")
wig_m <- read_excel("wig_m.xlsx")

wig<-c()
wig<-cbind(wig_banki_m[,1], wig_m[,5], wig_banki_m[,5],wig_budow_m[,5],wig_chemia_m[,5], wig_info_m[,5], wig_media_m[,5], wig_nrchom_m[,5], wig_paliwa_m[,5], wig_spozyw_m[,5], wig_telkom_m[,5])
colnames(wig)<-c("Data","WIG", "Banki", "Budow", "Chemia", "Info", "Media", "Nrchom", "Paliwa", "Spozyw", "Telkom")

daty<-wig[-1,1]
stopy <- data.frame(daty)
for (j in 2:ncol(wig)){
  stopy_zwrotu <- c()
  for (i in 2:nrow(wig)){
    stopy_zwrotu[i-1] <- (wig[i,j]-wig[i-1,j])/(wig[i-1,j])
  }
  stopy <- cbind(stopy,stopy_zwrotu)
}
colnames(stopy) <- colnames(wig)

stopa_proc_wolna_od_ryzyka <- 0.01/12
stopy[,-1] <- stopy[,-1] - stopa_proc_wolna_od_ryzyka

model_banki <- lm(stopy[,3]~stopy[,2])
model_budow <- lm(stopy[,4]~stopy[,2])
model_chemia <- lm(stopy[,5]~stopy[,2])
model_Info <- lm(stopy[,6]~stopy[,2])
model_Media <- lm(stopy[,7]~stopy[,2])
model_Nrchom <- lm(stopy[,8]~stopy[,2])
model_Paliwa <- lm(stopy[,9]~stopy[,2])
model_Spozyw <- lm(stopy[,10]~stopy[,2])
model_Telkom <- lm(stopy[,11]~stopy[,2])

reszty <- cbind(model_banki$residuals,model_budow$residuals,model_chemia$residuals,model_Info$residuals,
                model_Media$residuals,model_Nrchom$residuals,model_Paliwa$residuals,model_Spozyw$residuals,
                model_Telkom$residuals)  
colnames(reszty) <- c("Banki", "Budow", "Chemia", "Info", "Media", "Nrchom", "Paliwa", "Spozyw", "Telkom")

Alfa_0 <- c(summary(model_banki)$coefficients[1,1],summary(model_budow)$coefficients[1,1],
           summary(model_chemia)$coefficients[1,1],summary(model_Info)$coefficients[1,1],
           summary(model_Media)$coefficients[1,1],summary(model_Nrchom)$coefficients[1,1],
           summary(model_Paliwa)$coefficients[1,1],summary(model_Spozyw)$coefficients[1,1],
           summary(model_Telkom)$coefficients[1,1])

kowariancja <- cov(reszty)

T <- nrow(stopy)
N <- ncol(stopy) - 2
k <-1


Alfa_0_t <- t(Alfa_0)
kowariancja1 <- solve(kowariancja)

licznik <- Alfa_0_t%*%kowariancja1%*%t(t(Alfa_0))
mianownik <- ((mean(stopy[,2]))^2)/((sd(stopy[,2]))^2)+1

GRS <- (T/N) * ((T-N-k)/(T-k-1)) *(licznik/mianownik)

F_kryt <- qf(p=0.05,df1=N,df2=T-N-k, lower.tail = FALSE)


