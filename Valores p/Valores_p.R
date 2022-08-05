#########################################################################################################
# Código usado para las simulaciones y gráficas del video                                               #
# Qué es un valor p? Te lo puedo mostrar gráficamente (https://youtu.be/X0At_roDnHc)                    #
# del canal de YouTube Investigación Abierta (https://www.youtube.com/channel/UCs-6iLG3cnZwtGFcvyObPpA) #
# Juan David Leongómez, 2020                                                                            #
#########################################################################################################

#Cargar paquetes
library(tidyverse)
library(plyr)
library(latex2exp)
library(reshape2)

###### Ejemplo correlación / regresión simple ######

# Generar datos
# medias
m1 <- 165
m2 <- 65
# Varianzas
s1 <- 12
s2 <- 6
# Correlaciones
X1 <- 0

set.seed(1234)
dat <- MASS::mvrnorm(10000, mu = c(m1, m2),
                     Sigma = matrix(c(s1, X1,
                                      X1, s2),
                                    ncol = 2, byrow = TRUE),
                     empirical = TRUE)

colnames(dat) <- c("Estatura", "Peso")
dat <- as_tibble(dat)
dat$Sample <- rep(1:100, each = 100)

# Correlación por muestra
corfun <- function(x, y) {
  corr = (cor.test(x, y,
                 alternative = "two.sided", method = "pearson"))
}

# Base de datos
cors <- ddply(dat, .(Sample), summarise,
      p = round(corfun(Estatura, Peso)$p.value, 3),
      r = round(corfun(Estatura, Peso)$estimate, 2),
      IC = paste0("[", round(corfun(Estatura, Peso)$conf.int[1], 2), ", ", round(corfun(Estatura, Peso)$conf.int[2], 2), "]"),
      IClow = round(corfun(Estatura, Peso)$conf.int[1], 2),
      IChi = round(corfun(Estatura, Peso)$conf.int[2], 2))

# Agregar * a resultados significativos
cors$Sig <- ifelse(cors$p < 0.05, paste0("p = ", round(cors$p, 3), "*"), 
    ifelse(cors$p < 0.01, paste0("p = ", round(cors$p, 3), "**"), 
           ifelse(cors$p < 0.001, paste0("p = ", round(cors$p, 3), "***"), paste0("p = ", round(cors$p, 3)))))

# Agregar valores r (coeficientes de correlación)
dat <- merge(dat, cors, by = "Sample", all = T)

# Figuras (todas quedarán guardadas en el directorio de trabajo)
cornull = cor.test(dat$Estatura, dat$Peso,
                 alternative = "two.sided", method = "pearson")

# Figura hipótesis nula
p1 <- ggplot(dat, aes(x = Estatura, y = Peso)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm", color = "black") + 
  geom_rug() + 
  annotate(geom = "text", label = paste0("r = ", round(cornull$estimate, 2), 
                                         ", 95% IC ", paste0("[", round(cornull$conf.int[1], 2), ", ", round(cornull$conf.int[2], 2), "]"),
                                         ", p = ", round(cornull$p.value, 3)),
           x = 0.5*(min(dat$Estatura) + max(dat$Estatura)), y = max(dat$Peso), vjust = 1, colour = "black") +
  ggsave("p1.png", width=5.1, height=5.1, dpi=400)

#Figuras muestras seleccionadas
set.seed(123)
nu <- sample(1:100, 5)
nu <- c(nu, 90,  61, 86)

plot_list = list()
for (i in 1:length(nu)) {
  p =  ggplot(dat, aes(x = Estatura, y = Peso)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(method = "lm", color = "black") + 
    geom_rug() +
    geom_point(data = subset(dat, dat$Sample == nu[i]), color = "red") + 
    geom_smooth(data = subset(dat, dat$Sample == nu[i]), method = "lm", color = "red", fill = "red") +
    annotate(geom = "text", label = paste0("r = ", round(cors$r[nu[i]], 3), ", 95% IC ", cors$IC[nu[i]], ", ", cors$Sig[nu[i]]),
             x = 0.5*(min(dat$Estatura) + max(dat$Estatura)), y = max(dat$Peso), vjust = 1, colour = "red") 
  ggsave(paste("corsample", i, ".png", sep=""), width=4, height=4, dpi=400)
}

# Figura pendientes todas las primeras 100 muestras
p1a <- ggplot(dat, aes(x = Estatura, y = Peso, color = r)) + 
  geom_point(alpha = 0.1, color = "black")+
  geom_line(aes(group = r), stat="smooth", method = "lm", alpha = 0.8, size = 1) + 
  geom_rug() +
  labs(fill = "r") +
  scale_colour_gradient2(low = "red", mid = "grey", high = "blue") +
  ggsave("p1a.png", width=5.1, height=5.1, dpi=400)

# Distribución de valores r
N <- nrow(dat)
R <- 2500

data <- dat[,2:3]
cor.orig <- cor(data)
cor.boot <- NULL

for (i in 1:R) {
  idx <- sample.int(N, 100, replace = TRUE) 
  cor.boot[i] <- cor(data[idx, ])[1,2] 
}

cor.boot <- as.data.frame(cor.boot)
colnames(cor.boot) <- "r"

rect1 <- data.frame(xmin=-Inf, xmax=-0.2, ymin=0, ymax=Inf)
rect2 <- data.frame(xmin=0.2, xmax=Inf, ymin=0, ymax=Inf)

rdist <- ggplot(cor.boot, aes(x = r, y = ..density..)) +
  geom_histogram(bins = 80, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  geom_vline(xintercept = mean(cor.boot$r), color = "red", linetype = "dashed", size = 1) +
  ylim(c(0, 7)) +
  xlim(c(-0.4,0.4)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  labs(x = expression(italic(r)~" (Coeficiente de correlación de Pearson)"), 
       y = "Densidad") +
  annotate(geom = "text", label = paste0("Media ~ ", round(mean(cor.boot$r), 3)),
           x = -0.05, y = 6, vjust = 1, colour = "red", size = 5, angle = 90) +
  annotate(geom = "text", label = "Significativo (p < 0.05)",
           x = -0.3, y = 3, vjust = 1, colour = "blue", size = 4, angle = 90) +
  annotate(geom = "text", label = "Significativo (p < 0.05)",
           x = 0.3, y = 3, vjust = 1, colour = "blue", size = 4, angle = 90) +
  ggsave("rdist.png", width=5.1, height=5.1, dpi=400)

# Resultado hipotético (falso positivo)
rdistH1 <- ggplot(cor.boot, aes(x = r, y = ..density..)) +
  geom_histogram(bins = 80, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  geom_vline(xintercept = mean(cor.boot$r), color = "red", linetype = "dashed", size = 1) +
  ylim(c(0, 7)) +
  xlim(c(-0.4,0.4)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_vline(xintercept = -0.25, color = "blue", linetype = "dashed", size = 1) +
    labs(x = expression(italic(r)~" (Coeficiente de correlación de Pearson)"), 
       y = "Densidad") +
  ggsave("rdistH1.png", width=5.1, height=5.1, dpi=400)

# Distribución esperada del resultado hipotético
rdistH2 <- ggplot(cor.boot, aes(x = r, y = ..density..)) +
  geom_histogram(bins = 80, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  geom_vline(xintercept = mean(cor.boot$r), color = "red", linetype = "dashed", size = 1) +
  ylim(c(0, 7)) +
  xlim(c(-0.4,0.4)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_vline(xintercept = -0.25, color = "blue", linetype = "dashed", size = 1) +
  labs(x = expression(italic(r)~" (Coeficiente de correlación de Pearson)"), 
       y = "Densidad") +
  stat_function(fun = dnorm, n = 1000, args = list(mean = -0.25, sd = 0.1), inherit.aes = FALSE) +
  ggsave("rdistH2.png", width=5.1, height=5.1, dpi=400)


###### Ejemplo Pruebas-t ######

# Generar datos
# Medias
m1 <- 1.25*12
m2 <- 1.25*12
# Varianza
s1 <- 0.5*12
s2 <- 0.5*12
# Correlaciones
X1 <- 0

set.seed(12345)
hair <- MASS::mvrnorm(10000, mu = c(m1, m2),
                     Sigma = matrix(c(s1, X1,
                                      X1, s2),
                                    ncol = 2, byrow = TRUE),
                     empirical = TRUE)

colnames(hair) <- c("Control", "Experimental")
hair <- as_tibble(hair)
hair <- melt(hair)
hair$Sample <- rep(1:100, each = 100)
colnames(hair) <- c("Grupo", "Velocidad de Crecimiento de Cabello (cm por año)", "Sample")

tfun <- function(x, y) {
  corr = (t.test(x ~ y,
                   alternative = "two.sided"))
}

tt <- ddply(hair, .(Sample), summarise,
              p = round(tfun(`Velocidad de Crecimiento de Cabello (cm por año)`, Grupo)$p.value, 3),
              t = round(tfun(`Velocidad de Crecimiento de Cabello (cm por año)`, Grupo)$statistic, 2),
              IC = paste0("[", round(tfun(`Velocidad de Crecimiento de Cabello (cm por año)`, Grupo)$conf.int[1], 2), ", ", round(tfun(`Velocidad de Crecimiento de Cabello (cm por año)`, Grupo)$conf.int[2], 2), "]"),
              IClow = round(tfun(`Velocidad de Crecimiento de Cabello (cm por año)`, Grupo)$conf.int[1], 2),
              IChi = round(tfun(`Velocidad de Crecimiento de Cabello (cm por año)`, Grupo)$conf.int[2], 2))

tt$p <- ifelse(tt$p < 0.05, paste0("p = ", round(tt$p, 3), "*"), 
                   ifelse(tt$p < 0.01, paste0("p = ", round(tt$p, 3), "**"), 
                          ifelse(tt$p < 0.001, paste0("p = ", round(tt$p, 3), "***"), paste0("p = ", round(tt$p, 3)))))

# Agregar valores t
hair <- merge(hair, tt, by = "Sample", all = T)

# Figuras
tnull = t.test(hair[,3] ~ hair[,2], alternative = "two.sided")

h1 <- ggplot(hair, aes(x = Grupo, y = `Velocidad de Crecimiento de Cabello (cm por año)`)) +
  geom_jitter(alpha = 0.1, width = 0.2, aes(color = Grupo)) +
  stat_summary(aes(color = Grupo), fun.data = mean_cl_normal, 
               geom = "errorbar", color = "black", width = 0.2) +
  stat_summary(fun = mean, geom = "point", color = "black") +
  geom_smooth(method = "lm", color = "black", aes(x = as.numeric(Grupo))) +
  annotate(geom = "text", label = paste0("t = ", round(tnull$statistic, 2), 
                                         ", 95% IC ", paste0("[", round(tnull$conf.int[1], 2), ", ", round(tnull$conf.int[2], 2), "]"), 
                                         ", p = ", round(tnull$p.value, 3)),
           x = 1.5, y = Inf, vjust = 1, colour = "black") +
  scale_colour_brewer(palette = "Set2") +
  theme(legend.position="none") +
  ggsave("tnull.png", width=5.1, height=5.1, dpi=400)

set.seed(1278)
nu <- sample(1:100, 5)
nu <- c(nu, 99)

plot_list = list()
for (i in 1:length(nu)) {
  p =  ggplot(hair, aes(x = Grupo, y = `Velocidad de Crecimiento de Cabello (cm por año)`)) +
    stat_summary(aes(color = Grupo), fun.data = mean_cl_normal, 
                 geom = "errorbar", color = "black", width = 0.2) +
    stat_summary(fun.y = mean, geom = "point", color = "black") +
    geom_jitter(alpha = 0.1, width = 0.2, aes(color = Grupo)) +
    geom_smooth(method = "lm", color = "black", aes(x = as.numeric(Grupo))) +
    geom_jitter(data = subset(hair, hair$Sample == nu[i]), width = 0.1, color = "red", alpha = 0.5, size = 1) +
    stat_summary(data = subset(hair, hair$Sample == nu[i]), 
                 fun.data = mean_cl_normal, 
                 geom = "errorbar", color = "red", width = 0.2) +
    geom_smooth(data = subset(hair, hair$Sample == nu[i]), method = "lm", color = "red", aes(x = as.numeric(Grupo))) +
    annotate(geom = "text", label = paste0("t = ", tt$t[nu[i]], 
                                           ", 95% IC ", tt$IC[nu[i]],
                                           ", ", tt$p[nu[i]]), 
             x = 1.5, y = Inf, vjust = 1, colour = "red") +
    scale_colour_brewer(palette = "Set2") +
    theme(legend.position="none") +
    ggsave(paste("tsample", i, ".png", sep=""), width=5.1, height=5.1, dpi=400)
}

set.seed(12345)
hair2 <- MASS::mvrnorm(10000, mu = c(m1, m2),
                      Sigma = matrix(c(s1, X1,
                                       X1, s2),
                                     ncol = 2, byrow = TRUE),
                      empirical = TRUE)

colnames(hair2) <- c("Control", "Experimental")
hair2 <- as_tibble(hair2)

set.seed(1315)
B <- 2500
t.vect <- vector(length=B)
p.vect <- vector(length=B)
for(i in 1:B){
  boot.c <- sample(hair2$Control, size=100, replace=T)
  boot.p <- sample(hair2$Experimental, size=100, replace=T)
  ttest  <- t.test(boot.c, boot.p)
  t.vect[i] <- ttest$statistic
  p.vect[i] <- ttest$p.value
}

tall <- as.data.frame(t.vect)
colnames(tall) <- "t"
tall$p <- p.vect

rect1 <- data.frame(xmin=-Inf, xmax=-1.96, ymin=0, ymax=Inf)
rect2 <- data.frame(xmin=1.96, xmax=3.5, ymin=0, ymax=Inf)

tdist <- ggplot(tall, aes(x = t, y = ..density..)) +
  geom_histogram(bins = 40, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  ylim(c(0, 0.65)) +
  geom_vline(xintercept = mean(tall$t), color = "red", linetype = "dashed") +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  annotate(geom = "text", label = paste0("Media ~ ", round(mean(tall$t), 3)),
           x = 0.05, y = 0.55, vjust = 1, colour = "red", size = 6, angle = 90) +
  annotate(geom = "text", label = "Significativo (p < 0.05)",
           x = -2.8, y = 0.3, vjust = 1, colour = "blue", size = 4, angle = 90) +
  annotate(geom = "text", label = "Significativo (p < 0.05)",
           x = 2.8, y = 0.3, vjust = 1, colour = "blue", size = 4, angle = 90) +
  labs(x = "Valor t", y = "Densidad") +
  ggsave("tdist.png", width=5.1, height=5.1, dpi=400)

tdistH1 <- ggplot(tall, aes(x = t, y = ..density..)) +
  geom_histogram(bins = 40, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  ylim(c(0, 0.65)) +
  geom_vline(xintercept = mean(tall$t), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 2.8, color = "blue", linetype = "dashed") +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  labs(x = "Valor t", y = "Densidad") +
  ggsave("tdistH1.png", width=5.1, height=5.1, dpi=400)

tdistH2 <- ggplot(tall, aes(x = t, y = ..density..)) +
  geom_histogram(bins = 40, alpha = .5, position="identity") +
  geom_density(fill = "red", alpha=.3) +
  ylim(c(0, 0.65)) +
  geom_vline(xintercept = mean(tall$t), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 2.8, color = "blue", linetype = "dashed") +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE) +
  stat_function(fun = dnorm, n = 1000, args = list(mean = 2.8, sd = 1), inherit.aes = FALSE) +
  labs(x = "Valor t", y = "Densidad") +
  ggsave("tdistH2.png", width=5.1, height=5.1, dpi=400)
