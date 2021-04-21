#########################################################################################################
# Código usado para las simulaciones y gráficas del video                                               #
# Qué es un valor p? Te lo puedo mostrar gráficamente (https://youtu.be/X0At_roDnHc)                    #
# del canal de YouTube Investigación Abierta (https://www.youtube.com/channel/UCs-6iLG3cnZwtGFcvyObPpA)  #
# Juan David Leongómez, 2020                                                                            #
#########################################################################################################

#Cargar paquetes
library(tidyverse)
library(MASS)
library(magick)

###### Ejemplo correlación / regresión simple ######

# Generar datos
# medias
m1 <- 165
m2 <- 65
# Varianza
s1 <- 12
s2 <- 6
# Correlaciones
X1 <- 0.9

set.seed(1234)
dat <- mvrnorm(10000, mu = c(m1, m2),
               Sigma = matrix(c(s1, X1,
                                X1, s2),
                              ncol = 2, byrow = TRUE),
               empirical = TRUE)

dat <- as_tibble(dat)
colnames(dat) <- c("Estatura", "Peso")

#Figuras aleatorias por tamaño de muestra
set.seed(17)

## n = 10
plot_list = list()
for (i in 1:20) {
  p =  ggplot(dat, aes(x = Estatura, y = Peso)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(method = "lm", color = "black") + 
    ylim(c(50,80)) +
    geom_rug() +
    geom_point(data = dat[sample(nrow(dat), 10),], color = "red", alpha = 0.5) + 
    geom_smooth(data = dat[sample(nrow(dat), 10),], method = "lm", color = "red", fill = "red", fullrange = TRUE) +
    labs(title = "n = 10") +
    theme(plot.title = element_text(color="red")) +
    ggsave(paste("./n10/n10-", i, ".png", sep=""), width=4, height=4, dpi=200)
}
### Crear GIF
imgs10 <- list.files(path = "./n10", full.names = TRUE)
img_list10 <- lapply(imgs10, image_read)
img_joined10 <- image_join(img_list10)
img_animated10 <- image_animate(img_joined10, fps = 2)
image_write(image = img_animated10,
            path = "n10.gif")

## n = 100
plot_list = list()
for (i in 1:20) {
  p =  ggplot(dat, aes(x = Estatura, y = Peso)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(method = "lm", color = "black") +
    ylim(c(50,80)) +
    geom_rug() +
    geom_point(data = dat[sample(nrow(dat), 100),], color = "red", alpha = 0.5) + 
    geom_smooth(data = dat[sample(nrow(dat), 100),], method = "lm", color = "red", fill = "red", fullrange = TRUE) +
    labs(title = "n = 100") +
    theme(plot.title = element_text(color="red")) +
    ggsave(paste("./n100/n100-", i, ".png", sep=""), width=4, height=4, dpi=200)
}
### Crear GIF
imgs100 <- list.files(path = "./n100", full.names = TRUE)
img_list100 <- lapply(imgs100, image_read)
img_joined100 <- image_join(img_list100)
img_animated100 <- image_animate(img_joined100, fps = 2)
image_write(image = img_animated100,
            path = "n100.gif")

# n = 1000
plot_list = list()
for (i in 1:20) {
  p =  ggplot(dat, aes(x = Estatura, y = Peso)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(method = "lm", color = "black") + 
    ylim(c(50,80)) +
    geom_rug() +
    geom_point(data = dat[sample(nrow(dat), 1000),], color = "red", alpha = 0.5) + 
    geom_smooth(data = dat[sample(nrow(dat), 1000),], method = "lm", color = "red", fill = "red", fullrange = TRUE) +
    labs(title = "n = 1000") +
    theme(plot.title = element_text(color="red")) +
    ggsave(paste("./n1000/n1000-", i, ".png", sep=""), width=4, height=4, dpi=200)
}
### Crear GIF
imgs1000 <- list.files(path = "./n1000", full.names = TRUE)
img_list1000 <- lapply(imgs1000, image_read)
img_joined1000 <- image_join(img_list1000)
img_animated1000 <- image_animate(img_joined1000, fps = 2)
image_write(image = img_animated1000,
            path = "n1000.gif")