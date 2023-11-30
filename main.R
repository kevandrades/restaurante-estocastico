if (!require(pacman)) install.packages("pacman")

pacman::p_load(simmer, tidyverse)

set.seed(8273)

pedidos_hora <- 12.40 # média de pedidos q chegam por hora. segue poisson
atendimento_hora <- 2.29  # média de pedidos atendidos por hora 
tempo_atendimento <- 26.21 # tempo médio de atendimento por pedido

env <- simmer("Restaurante")   # ambiente

pedido <- trajectory("caminho do pedido") %>%   # definindo a trajetoria do pedido
  seize("preparação", 1) %>% # ocupa uma preparação
  timeout(function() rexp(1, 1/tempo_atendimento)) %>%  # estou considerando o tempo de atendimento total como preparação.... faltam dados
  release("preparação", 1) %>% # libera uma preparação
  seize("entrega", 1) %>%  # ocupa uma entrega
  timeout(function() rnorm(1, 6.2916)) %>% # tentei ver a media de tempo de entrega pelo apendice...
  release("entrega", 1) # libera uma entrega

env %>%
  add_resource("preparação", 7) %>%  # define quantas preparações (7 atendentes)
  add_resource("entrega", 1) %>%   # define entrega (1 entregador)
  add_generator("pedido", pedido, function() rpois(1, 12.40))  # define qual o objeto e o tempo que ele chega na fila