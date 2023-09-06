library(effectsize)
library(faux)
library(scales)

n5 <- rnorm_multi(
  n = 5, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n10 <- rnorm_multi(
  n = 10, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n50 <- rnorm_multi(
  n = 50, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n100 <- rnorm_multi(
  n = 100, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n500 <- rnorm_multi(
  n = 500, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n1000 <- rnorm_multi(
  n = 1000, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n5000 <- rnorm_multi(
  n = 5000, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n10000 <- rnorm_multi(
  n = 10000, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n50000 <- rnorm_multi(
  n = 50000, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

n100000 <- rnorm_multi(
  n = 100000, 
  vars = 2, 
  r = 0, 
  mu = c(12, 10), 
  sd = 4, 
  varnames = c("A", "B"),
  empirical = TRUE)  |> 
  pivot_longer(cols = A:B,
               names_to = "category",
               values_to = "value")

d5 <- cohens_d(value ~ category, data = n5)
g5 <- hedges_g(value ~ category, data = n5)

d10 <- cohens_d(value ~ category, data = n10)
g10 <- hedges_g(value ~ category, data = n10)

d50 <- cohens_d(value ~ category, data = n50)
g50 <- hedges_g(value ~ category, data = n50)

d100 <- cohens_d(value ~ category, data = n100)
g100 <- hedges_g(value ~ category, data = n100)

d500 <- cohens_d(value ~ category, data = n500)
g500 <- hedges_g(value ~ category, data = n500)

d1000 <- cohens_d(value ~ category, data = n1000)
g1000 <- hedges_g(value ~ category, data = n1000)

d5000 <- cohens_d(value ~ category, data = n5000)
g5000 <- hedges_g(value ~ category, data = n5000)

d10000 <- cohens_d(value ~ category, data = n10000)
g10000 <- hedges_g(value ~ category, data = n10000)

d50000 <- cohens_d(value ~ category, data = n50000)
g50000 <- hedges_g(value ~ category, data = n50000)

d100000 <- cohens_d(value ~ category, data = n100000)
g100000 <- hedges_g(value ~ category, data = n100000)


tabdtg <- data.frame("n" = c(5, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000),
                     "d" = c(d5$Cohens_d, d10$Cohens_d, d50$Cohens_d, d100$Cohens_d, d500$Cohens_d, d1000$Cohens_d, d5000$Cohens_d, d10000$Cohens_d, d50000$Cohens_d, d100000$Cohens_d),
                     "g" = c(g5$Hedges_g, g10$Hedges_g, g50$Hedges_g, g100$Hedges_g, g500$Hedges_g, g1000$Hedges_g, g5000$Hedges_g, g10000$Hedges_g, g50000$Hedges_g, g100000$Hedges_g))

ggplot(tabdtg, aes(x = n, y = d - g, color = d - g)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line() +
  geom_point() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = label_number_auto()) +
  annotation_logticks(sides = "b") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Tamaño de muestra", 
       y = expression(paste("Diferencia entre ", italic("d"), " de Cohen y ", italic("g"), " de Hedges"))) +
  theme_classic()
