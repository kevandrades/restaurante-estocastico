if (!require(pacman)) install.packages("pacman")

pacman::p_load(simmer, tidyverse)

set.seed(8273)

pedidos_hora <- 12.40 # média de pedidos q chegam por hora. segue poisson
atendimento_hora <- 2.29  # média de pedidos atendidos por hora 
tempo_atendimento <- 26.21 # tempo médio de atendimento por pedido

env <- simmer("Restaurante")   # ambiente