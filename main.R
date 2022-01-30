# install.packages("expm")
# install.packages("stringr")
library(expm)
library(stringr)
library(pracma)
library(Ryacas)

# Exercice 1
cat("\n -= Exercice 1 =-\n")

n <- as.numeric(readline("Saisir n : "))

M <- rbind(c(1, 0, 2), c(0, -1, 0), c(2, 0, 1))
D_M <- diag(round(eigen(M)$values))
P_M <- eigen(M)$vectors
inv_P_M <- solve(eigen(M)$vectors)

u0 <- 1
v0 <- 2
w0 <- 3
X0 <- c(u0, v0, w0)

# P * D^n * P^-1 * X0
Xn <- P_M %*% D_M%^%n %*% inv_P_M %*% X0
un <- Xn[1]
vn <- Xn[2]
wn <- Xn[3]
message("u",n," = ",un)
message("v",n," = ",vn)
message("w",n," = ",wn)

# Exercice 2
cat("\n -= Exercice 2 =-\n")



# Exercice 4
cat("\n -= Exercice 4 =-\n")
# https://www.youtube.com/watch?v=sS1ePuiy0pq

# P(X>75) = 0.10 -> P(X<75) = 0.9
# P(X<50) = 0.25
t <- qnorm(0.90, 0, 1)
t_prim <- qnorm(0.25, 0, 1)

# 75 - mu = t * sigma
# 50 - mu = t_prim * sigma

A <- rbind(c(t, 1), c(-0.67, 1))
b <- c(75, 50)
result <- solve(A) %*% b
sigma <- result[1]
mu <- result[2]
message("mu(moyenne) = ",mu)
message("sigma(écart-type) = ",sigma)

# y <- rnorm(1000,mu,sigma)
# hist(y,freq=TRUE)

plot(function(x) dnorm(x, mu, sigma), mu-4*sigma, mu+4*sigma, main = "", lwd=3,xlab="Distance des javelots",ylab="Frequence")
abline (v =c(mu-sigma,mu+sigma), col="red",lwd=2,lty=3)

# Exercice 5
cat("\n -= Exercice 5 =-\n")
# https://sites.google.com/site/rgraphiques/home/les-donnees-de-type-texte
# https://data.hypotheses.org/564
Unaccent <- function(text) {
  text <- gsub("Ç","C",text); text <- gsub("ç","c",text); text <- gsub("é","e",text); text <- gsub("È","e",text);
  text <- gsub("î","i",text); text <- gsub("ï","i",text); text <- gsub("è","e",text); text <- gsub("ê","e",text);
  text <- gsub(" ","_",text); text <- gsub("-","_",text);   text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE");   text <- gsub("['`^~\"]", "", text)
  return(text)
}

mot <- Unaccent("RADAR")
mots <- c("radar", "bonne année", "sept", "kayak", "la mariée ira mal", "statistiques", "engage le jeu que je le gagne", "espoe reste ici et se repose")

palindrome <- function (mot) {
  # print(mot)
  test <- setequal(str_sub(mot,1,1), str_sub(mot,-1,-1))
  if ( str_length(str_sub(mot,2,-2)) > 2 & test) {
    test <- palindrome(str_sub(mot,2,-2))
  }
  return (test)
}

if (palindrome(str_replace_all(Unaccent(mot), "_", ""))) {
  print(paste0(mot,  ", est un palindrome"))
} else {
  print(paste0(mot, ", n'est pas un palindrome"))
}

for (m in mots) {
  if (palindrome(str_replace_all(Unaccent(m), "_", ""))) {
    print(paste0(m,  ", est un palindrome"))
  } else {
    print(paste0(m, ", n'est pas un palindrome"))
  }
}

# Exercice 6
cat("\n -= Exercice 6 =-\n")

for (n in 1:10) {
  print(paste0("I",n))
  print(integral(function(x) x^2*(log(x))^n, 1, exp(1)))
  print(integrate(x^2*(log(x))^n)
}

# 2) La limite tent vers 0

n <- ysym("n")
d <- "n*(integral(x^2*(log(x))^n, 1, exp(1)))"
lim(yac_symbol(yac_str(d)), n, Inf)
# yac_symbol(yac_str("lim(n*(integral(x^2*(log(x))^n, 1, exp(1))), n, Inf)"))
