# RP rozw

library("prob")
rolldie()
cards()
subset(cards(), suit == "Club" & rank == 6)
logsum <- function(x1, x2, x3) {
  (x1 == "H") + (x2 == "H") + (x3 == "H")
}
subset(tosscoin(3), (toss1 == "H" + toss2 == "H" + toss3 == "H") == 2)
subset(tosscoin(3), logsum(toss1, toss2, toss3) == 2)


library("prob")

#Ćwiczenie 1.1
wyniki <- tosscoin(8)
nrow(wyniki)

#Ćwiczenie 1.2
rolldie(2, nsides = 4)

#Ćwiczenie 2.2
subset(rolldie(3, nsides = 2) - 1, X1 + X2 + X3 == 2) / nrow(rolldie(3, nsides = 1))
1 - subset(rolldie(3, nsides = 2) - 1, X1 + X2 + X3 == 0) / nrow(rolldie(3, nsides = 1))

#Ćwiczenie 2.3
S <- cards(makespace = TRUE)
A <- subset(S, suit == "Heart")
B <- subset(S, rank %in% 7:9)

prob(union(A,B)) - (prob(A) + prob(B) - prob(intersect(A, B)))

#Ćwiczenie 3.6
plot(1:4, rep(0.25, 4))

#Zadanie 3.2
1:6 %>%
  map(~ 10 ^ .) %>%
  map(~ sample(0:1, size = ., replace = TRUE)) %>%
  map(~ (sum(.)/length(.)-0.5)/0.5)

#Rozkłąd bernoulliego

n <- 15
p <- 0.6
# Wektor atomów:
x <- 0:n
# Wektor prawdopodobieństw:
f <- dbinom(x, size = n, prob = p)

mu1 <- sum(x * f)
mu2 <- n*p

#Ćwiczenie 3.8
n <- 100
x <- 0:n
p <- 0.2
y <- dbinom(x, size = n, prob = p)

plot(x, y)

#w ggplot
dane <- data.frame(x = x, y = y)
library(ggplot2)

ggplot(dane, aes(x = x, y = y)) + geom_point()

# Zadanie 3.3
rbinom(10000, size = 10, prob = 0.7) %>% table()/10000

dbinom(0:10, size = 10, prob = 0.7)

# Zadanie 3.4
n <- 2 * 1:9 - 1
sum(dbinom(n, size = max(n), prob = 0.5))

n <- 2 * 1:10 - 2
sum(dbinom(n, size = max(n), prob = 0.5))


# Zadanie 3.5
dbinom(6:8, size=12, prob = 1/6) %>% sum()

dbinom(0:12, size=12, prob = 1/6) %>% sum()

#Ćwiczenie 3.9
p <- 0.3
k <- 0:20
(1-p)^k*p
dgeom(k, prob = 0.3)

#Zadanie 3.6
1:10000 %>% map(~ sample(0:1, size = 20, prob=c(0.7,0.3), replace = TRUE) %>%
                  as.logical() %>%
                  which() %>%
                  dplyr::first() - 1) %>%
  unlist %>%
  table()/10000

dgeom(0:19, prob = 0.3)

porazki <- 1:10000 %>% map(~ rbinom(20, size = 1, prob = 0.3)) %>% map(~ . == 1) %>% map_int(~ first(which(.))) - 1 %>% as.integer()
table(porazki)/10000

dgeom(0:20, prob=0.3)

#Zadanie 3.7
1-sum(pgeom(0:4, 0.1))

#Zadanie 3.8
#a)
binom(size = 31, prob = 0.447)
#b)
plot(function(x) {dbinom(x, size = 31, prob = 0.447)})

#c)
plot(function(x) {pbinom(x, size = 31, prob = 0.447)})

#d)
dbinom(17, size = 31, prob = 0.447)
pbinom(13, size = 31, prob = 0.447)
1 - pbinom(11, size = 31, prob = 0.447)
1 - pbinom(19, size = 31, prob = 0.447)
pbinom(17:20, size = 31, prob = 0.447)

#Zadanie 3.9
rpois(1000, lambda = 5) %>% mean
rpois(1000, lambda = 10) %>% mean

#zadanie 3.10
(dbinom(0:100, size = 1000, prob = 0.02) - dpois(0:100, lambda=20)) %>% round(10)
(dbinom(0:100, size = 10000, prob = 0.0005) - dpois(0:100, lambda=5)) %>% round(10)

#Zadanie 3.12
probka <- rbinom(10000, size=20, prob=0.3)
x <- seq(0, 20, 0.01)
plot(ecdf(probka))
lines(x, pbinom(x, size=20, prob=0.3), col="red")

#Ćwiczenie 3.13
probka <- runif(1000)
(probka < 0.5) %>% sum

#Zadanie 3.14
pnorm(115, 100, 15) - pnorm(85, 100, 15)

#Zadanie 3.15
x <- seq(120, 160, 0.01)
pnorm(x, 100, 15) %>% map_lgl(~ . >= 0.99) %>% which() %>% first() %>% x[.]
plot(function(x) {pnorm(x, 100, 15)}, 100, 200)

qnorm(0.99, 100, 15)

#Zadanie 3.17
ile_kl <- 1:10000 %>% map(~ rexp(20, 4)) %>% map(cumsum) %>% map_int(~ sum(. < 1))

table(ile_kl)/10000

dpois(0:13, lambda = 4)

#Zadanie 3.18
x <- seq(50,150, 0.01)
1:1000 %>% map(~ sum(rnorm(100)^2)) %>% unlist %>% hist(prob=TRUE)
lines(x, dchisq(x, df=100), col="red")

#Zadanie 3.19
ar <- seq(-500,500,0.01)
x <- rnorm(1000)
y <- rnorm(1000)
z <- x/sqrt(y^2) * sqrt(2)
z %>% hist(prob=TRUE, breaks = 100)
lines(ar, dt(ar, df=2), col="red")

#Zadanie 3.20
ar <- seq(0, 400, 0.1)
x <- rchisq(1000, df=3)
y <- rchisq(1000, df=2)
z <- (x/5)/(y/2)
z %>% hist(prob=TRUE, breaks = 100)
lines(ar, df(ar, df1=3, df2 = 2), col="red")

#Zadanie 4.6
S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = max, invars = c("X1", "X2"), name = "U")
S <- addrv(S, FUN = sum, invars = c("X1", "X2"), name = "V")

UV <- marginal(S, vars = c("U", "V"))

U <- marginal(UV, vars = "U")
V <- marginal(UV, vars = "V")

Eu <- U$U * U$probs
Ev <- V$V * V$probs

#Zadanie 4.8
library("MASS")
xnorm <- mvrnorm(10000, mu=c(4,8), Sigma = matrix(c(2,1,1,5), 2, 2, byrow=TRUE)) %>%
  .[,1] %>%
  as.vector()

hist(xnorm, prob=TRUE)
x <- seq(0,8,0.01)
lines(x, dnorm(x, 4, sqrt(2)), col="red")


#Ćwiczenie 6.1

rexp(10, 2) %>% ecdf() %>% plot
x <- seq(0,1,0.01)
lines(x, pexp(x, 2), col="red")

rexp(100, 2) %>% ecdf() %>% plot
x <- seq(0,3,0.01)
lines(x, pexp(x, 2), col="red")

rexp(1000, 2) %>% ecdf() %>% plot
x <- seq(0,5,0.01)
lines(x, pexp(x, 2), col="red")

#Ćwiczenie 6.2

rexp(10, 2) %>% hist(prob=TRUE)
x <- seq(0,2,0.01)
lines(x, dexp(x, 2), col="red")

rexp(100, 2) %>% hist(prob=TRUE)
x <- seq(0,3,0.01)
lines(x, dexp(x, 2), col="red")

rexp(1000, 2) %>% hist(prob=TRUE, breaks = 50)
x <- seq(0,5,0.01)
lines(x, dexp(x, 2), col="red")

# Zadanie 6.1
mu <- 1/2
sigma_sq <- 1/12
probka <- 1:10000 %>% map(~ mean(runif(100))) %>% unlist
mean(probka) - mu
var(probka) - sigma_sq/100

# Zadanie 6.2
x <- seq(8,12,0.01)
mu <- 10
sigma <- 2
probka <- 1:10000 %>% map(~ mean(rnorm(100, mu, sigma))) %>% unlist
hist(probka, prob=TRUE, breaks = 50)
lines(x, dnorm(x, mu, sd = sigma/sqrt(100)), col="red")

x <- seq(60,160,0.01)
probka <- 1:10000 %>% map(~ rnorm(100, mu, sigma)) %>%
  map(~ sum((. - mean(.))^2)) %>% unlist()/sigma^2
probka %>% hist(prob=T)
lines(x, dchisq(x, df=99), col="red")

#Zadanie 6.3
mu <- 10
sigma <- 2
probki <- 1:10000 %>% map(~ rnorm(100, mu, sigma))
probka_srednich <- probki %>% map(~mean(.)) %>% unlist()
probka_odchylen <- probki %>% map(~ sum((. - mean(.))^2)) %>%
  unlist()/99

x <- seq(-2,2,0.01)
((probka_srednich-mu)/(sqrt(probka_odchylen/99))) %>% hist(prob=T, breaks=50)
lines(x, dt(x, 99), col="red")

#Zadanie 6.4
probka <- 1:10000 %>% map(~ rexp(100,2)) %>%
  map(~ (sum(.) - 100*0.5)/sqrt(100*0.25)) %>% unlist
probka %>% hist(prob=TRUE)
x <- seq(-5,5,0.01)
lines(x, dnorm(x), col="red")

#Zadanie 6.5
#Średnia ma rozkład N(19, 1/26) stąd już łatwo.

# Zadanie 6.6
x <- runif(100000, min = 0, max= 1)
y <- -log(x)/10

y %>% ecdf() %>% plot()
z <- seq(0,1.5,0.01)
lines(z, pexp(z, 10), col="red")




#Zadanie 7.1

#Próbka z zadania"
x <- c(0.191699821, 0.224444400, 0.068581114, 0.197560308, 0.059592823,
       0.418550081, 0.075697666, 0.014439019, 0.028238374, 0.372759289,
       0.195084562, 0.136200808, 0.014555697, 0.158762977, 0.020223529,
       0.163496164, 0.547601560, 0.005772022, 0.186990635, 0.040101805)

#Wystarczy zapisać jedno równanie.

mu <- mean(x)

#Wartość oczekiwana rozkładu Exp(lambda) to 1/lambda, stąd estymatorem lambdy jest:

1/mu

#Ćwiczenie 7.1
x = c(0.8613860, 1.3584671, 1.6807666, 2.1343041, 2.0676056, 1.7174934, 2.1649500,
      2.4817035, 2.3765367, 2.1067326, 2.3619455, 2.2268371, 1.3793406, 2.0983811,
      1.9972017, 1.8204626, 2.2233658, 1.7847471, 2.0645107, 1.4970086, 1.9185137,
      1.8246692, 1.6753086, 1.9806044, 1.9994241, 0.7223032, 2.2063435, 0.7156948,
      1.5408702, 1.6691013, 2.0507178, 2.0192896, 2.0609326, 2.0367355, 2.0393775,
      1.3901765, 1.7117209, 1.6996659, 1.6338920, 1.9890302, 1.6031829, 2.0670333,
      1.5727281, 1.6499007, 1.5827539, 1.3763549, 1.4520779, 2.3014766, 1.4800752,
      2.3454455, 1.8752410, 1.2570965, 1.5012425, 2.1032645, 1.5289727, 2.0545819,
      1.7035962, 1.4161743, 1.9722288, 1.8486425, 1.6994698, 1.8635498, 2.3129328,
      1.9344945, 1.9335145, 1.8549529, 2.4586943, 1.2438746, 1.5412629, 2.1358845,
      1.5105158, 2.0013740, 1.8427912, 1.4682519, 1.6043895, 1.8410004, 1.6901282,
      1.1609868, 1.3293011, 0.9598889, 1.4081033, 1.7908027, 2.0916427, 2.0217946,
      1.9763032, 2.1016848, 2.0506948, 1.9667649, 1.5994718, 1.7598423, 1.2616686,
      1.7017247, 1.6513898, 1.5017221, 2.4568206, 1.5495427, 2.2166571, 1.9582805,
      2.0737279, 2.0349916)


n <- length(x)
#kwantyl rzędu 1/4:
x_1_4 <- sort(x) %>% .[ceiling(n*0.25)]

#kwantyl rzędu 3/4:
x_3_4 <- sort(x) %>% .[ceiling(n*0.75)]

#oszacowanie parametrów:
b <- log(3)/(log(x_3_4)-log(x_1_4))
c <- exp(log(4) - b * log(x_1_4))

# Zadanie 7.2

#odrobina obliczeń
# http://mathworld.wolfram.com/MaximumLikelihood.html

#Zadanie 7.3

#Z zadania 6.6, kropka druga wynika, że ten rozkład będzie Exp(5).

library(MASS)
x <- -log(runif(10000, 0, 1))/5

fitdistr(x, "exponential")

