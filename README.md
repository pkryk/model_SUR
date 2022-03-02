---
title: "Model CAPM"
author: "Piotr Kryk"
---

## Wstęp
Celem projektu jest zbadanie i przetestowanie zależności pomiędzy ryzykiem a stopą zwrotu z indeksów branżowych za pomocą modelu wyceny aktywów kapitałowych CAPM.

W niniejszym opracowaniu zweryfikowano hipotezę, że stopy zwrotu z portfeli odpowiednich indeksów branżowych zależne są wyłącznie od portfela rynkowego, ktorego odzwierciedla Warszawski Indeks Giełdowy (WIG), co daje się opisać poniższym równaniem regresji.

$$(R_{it}-r_{ft}) = \alpha_{i} + \beta{i}(R_{Mt}-r_{ft}) + \varepsilon_{it}$$
Gdzie: <br>
$R_{it}$ - stopa zwrotu i-tego portfela w czasie t;<br>
$r_{ft}$ - stopa zwrotu wolna od ryzyka;<br>
$R_{Mt}$ - stopa zwrotu portfela rynkowego - WIG.
<br>

Do badania wykorzystano miesięczne wartości indeksu WIG oraz indeksy branżowe obejmujace okres od marca 2009 roku do pazdziernika 2021 roku. Dane historyczne pochodzą z serwisu stooq.pl.

```{r, echo=FALSE}
library("readxl")


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

```

```{r,echo=FALSE}
options(width=250)
```
W tabeli przedstawiono wartości indeksu WIG oraz poszczególnych indeksów branżowych dla pierwszych 10 miesięcy badanego okresu:
```{r, echo=FALSE}
wig<-c()
wig<-cbind(wig_banki_m[,1], wig_m[,5], wig_banki_m[,5],wig_budow_m[,5],wig_chemia_m[,5], wig_info_m[,5], wig_media_m[,5], wig_nrchom_m[,5], wig_paliwa_m[,5], wig_spozyw_m[,5], wig_telkom_m[,5])
colnames(wig)<-c("Data","WIG", "Banki", "Budow", "Chemia", "Info", "Media", "Nrchom", "Paliwa", "Spozyw", "Telkom")

wig[1:10, ]
```
## Stopy zwrotu
Dla uzyskanych wartości wyznaczono proste stopy zwrotu za pomocą wzoru:  

$$L_1(t)=100*\frac{x_t - x_{t-1}}{x_{t-1}}$$
<br>
```{r}
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
```
W tabeli przedstawiono proste stopy zwrotu z portfela rynkowego WIG oraz poszczególnych indeksów branżowych dla pierwszych 10 miesięcy badanego okresu:
```{r,echo=FALSE}
options(width=100)
```

```{r, echo=FALSE}
stopy[1:10, ]
```
Za stopę wolną od ryzyka przyjęto wartość - 0.01
```{r}
stopa_proc_wolna_od_ryzyka <- 0.01/12
stopy[,-1] <- stopy[,-1] - stopa_proc_wolna_od_ryzyka
```
## Budowa modelu CAPM
Z uwagi na to, że układy równań wyjaśniane są przez te samą zmienną objaśniającą (WIG) modele estymowano w sposoób niezależny za pomocą Metody Najmniejszych Kwadratów.
```{r}
model_banki <- lm(stopy[,3]~stopy[,2])
model_budow <- lm(stopy[,4]~stopy[,2])
model_chemia <- lm(stopy[,5]~stopy[,2])
model_Info <- lm(stopy[,6]~stopy[,2])
model_Media <- lm(stopy[,7]~stopy[,2])
model_Nrchom <- lm(stopy[,8]~stopy[,2])
model_Paliwa <- lm(stopy[,9]~stopy[,2])
model_Spozyw <- lm(stopy[,10]~stopy[,2])
model_Telkom <- lm(stopy[,11]~stopy[,2])
```
Następnie wyznaczono wartości reszt oraz wyrazy wolne poszczególnych modeli.
```{r}
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
```
## Statystyka GRS
W celu sprawdzenia, czy uzyskane modele są odpowiednie, należy sprawdzić, czy wyrazy wolne modeli sa statystycznie różne od zera w ramach grupy portfeli.<br>

Hipoteza zerowa testu ma postać:
$$H_0: \alpha = 0 $$
<br>
Posłużono się statystyką testową GRS, która w tym przypadku przyjmuje postać;

$$GRS = (\frac{T}{N})(\frac{T-N-K}{T-K-1})[\frac{\alpha^T\Sigma^{-1}\hat{\alpha}}{1+\frac{\bar{x}^2_{wig}}{\sigma^2_{wig}}}]$$
Gdzie: <br>
$T$ - liczebność próby;<br>
$N$ - liczba portfeli objaśnianych w badanej próbie;<br>
$K$ - liczba czynników objaśniających;<br>
$\hat{\alpha}$ - wektor wyrazów wolnych z regresji;<br>
$\Sigma$ - macierz kowariancji składników losowych.
<br>


```{r}
T <- nrow(stopy)
N <- ncol(stopy) - 2
k <-1


Alfa_0_t <- t(Alfa_0)
kowariancja1 <- solve(kowariancja)

licznik <- Alfa_0_t%*%kowariancja1%*%t(t(Alfa_0))
mianownik <- ((mean(stopy[,2]))^2)/((sd(stopy[,2]))^2)+1

GRS <- (T/N) * ((T-N-k)/(T-k-1)) *(licznik/mianownik)

GRS
```
Statystyka GRS ma rozkład F z N oraz T-N-K stopniami swobody.<br><br>
Wyznaczono wartość krytyczną na poziomie istotności 0.05.

```{r}
F_kryt <- qf(p=0.05,df1=N,df2=T-N-k, lower.tail = FALSE)
F_kryt
```
## Wnioski
Wyznaczona wartość statytsyki GRS  jest mniejsza od wartości krytycznej rozkładu F, nie ma więc podstaw do odrzucenia hipotezy zerowej.<br>

Można zatem stwierdzić, iż model CAPM jest odpowiednim narzędziem do objaśniania stóp zwrotu. Brak podstaw do odrzucenia hipotezy sugeruje, iż łącznie wyrazy wolne modeli są nieistotne, zatem można stwierdzić, że żadne czynniki zewnętrzne nie wpływają na wartości stóp zwrotu z portfeli odpowiednich indeksów branżowych - zależą one wyłącznie od portfela rynkowego.

