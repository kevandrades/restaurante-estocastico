library(tidyverse)
library(lubridate)
library(simmer)
library(simmer.plot)


#leitura dos dados
dados <- readxl::read_xlsx("Dados Restaurante.xlsx") %>%
  mutate(
    `Tempo no Canal` = force_tz(`Tempo no Canal`, tzone = "UTC"),
    `Tempo de Entrega` = force_tz(`Tempo de Entrega`, tzone = "UTC"),
    `Tempo no Canal` = minute(`Tempo no Canal`),
    `Tempo de Entrega` = minute(`Tempo de Entrega`)
  )

# tabela das médias de tempo de entrega por canal
# aqui estou pensando como separar as médias. o balcão obviamente é separado dos demais,
# porém, não sei se o entregador é o mesmo para as demais atividades. acredito que não faz 
# sentido separar as médias de whatsapp e telefone, pois é o mesmo entregador, logo, o canal não
# deveria influenciar
# agora o APP eu não sei se devemos considerar a mesma média, pois pode ser q o entregador seja da
# plataforma, e não do restaurante... pelo que eu li, existe ambas opções: entregador da plat. ou da loja.
tempo_entrega_pcanal <- dados %>%
  group_by(`Tipo de Canal`) %>%
  summarise("Tempo médio de entrega" = mean(`Tempo de Entrega`))

entrega_telwpp <- mean(tempo_entrega_pcanal$`Tempo médio de entrega`[3:4]) 
entrega_app <- tempo_entrega_pcanal$`Tempo médio de entrega`[2]

# tabela das médias de tempo de atendimento por canal
# aqui existe um problema: o trabalho considerou que o atendimento é o tempo para obter o pedido + preparação
# o que não faz sentido, pois existem dois atendentes: uma que irá passar o pedido e ser liberada para atender outro cliente
# e um cozinheiro que irá fazer o pedido
# o tempo para anotar um pedido geralmente tem médida de poucos minutos, dependendo do canal.
# já a preparação demora mais.
# temos que separar esse resultado e definir o tempo médio das duas atividades...
tempo_atendim_pcanal <- dados %>%
  group_by(`Tipo de Canal`) %>%
  summarise("Tempo médio no Canal" = mean(`Tempo no Canal`))

# tempo de produção do pedido
# aparentemente é algo em torno de 20 a 30 minutos...
temp_preparacao <- 20

# atendimento subjetivamente definido
atend_balcao <- tempo_atendim_pcanal$`Tempo médio no Canal`[1]-temp_preparacao
atend_app <- tempo_atendim_pcanal$`Tempo médio no Canal`[2]-temp_preparacao
atend_tel <- tempo_atendim_pcanal$`Tempo médio no Canal`[3]-temp_preparacao
atend_wpp <- tempo_atendim_pcanal$`Tempo médio no Canal`[4]-temp_preparacao


# alguns dados interessantes disponibilizados pela autora
pedidos_hora <- 12.40 # média de pedidos q chegam por hora. segue poisson
pedidos_minuto <- pedidos_hora/60
atendimento_hora <- 2.29  # média de pedidos atendidos por hora 
tempo_atendimento <- 26.21 # tempo médio de atendimento por pedido
tempo_simulacao <- 3.5*60 



set.seed(8273)

env <- simmer("Restaurante")   # ambiente

#trajetoria do pedido via whatsapp
whatsapp <- trajectory("whatsapp") %>%
  seize("atendente", 1) %>%
  timeout(function() rexp(1, 1/atend_wpp)) %>% # definir média de tempo para a atendente anotar o pedido e solicitar preparação
  release("atendente", 1) %>%
  seize("preparação", 1) %>% # ocupa uma preparação
  timeout(function() rexp(1, 1/temp_preparacao)) %>%  # definir média de tempo de preparação
  release("preparação", 1) %>% # libera uma preparação
  seize("entrega", 1) %>%  # ocupa uma entrega
  timeout(function() rexp(1, 1/entrega_telwpp)) %>%
  release("entrega", 1) # libera uma entrega


#trajetoria do pedido via telefone
telefone <- trajectory("telefone") %>%
  seize("atendente", 1) %>%
  timeout(function() rexp(1, 1/atend_tel)) %>% # definir média de tempo para a atendente anotar o pedido e solicitar preparação
  release("atendente", 1) %>%
  seize("preparação", 1) %>% # ocupa uma preparação
  timeout(function() rexp(1, 1/temp_preparacao)) %>%  # definir média de tempo de preparação
  release("preparação", 1) %>% # libera uma preparação
  seize("entrega", 1) %>%  # ocupa uma entrega
  timeout(function() rexp(1, 1/entrega_telwpp)) %>%
  release("entrega", 1) # libera uma entrega

#trajetoria do pedido via app
app <- trajectory("app") %>%
  seize("atendente", 1) %>%
  timeout(function() rexp(1, 1/atend_app)) %>% # definir média de tempo para a atendente anotar o pedido e solicitar preparação
  release("atendente", 1) %>%
  seize("preparação", 1) %>% # ocupa uma preparação
  timeout(function() rexp(1, 1/temp_preparacao)) %>%  # definir média de tempo de preparação
  release("preparação", 1) %>% # libera uma preparação
  seize("entrega", 1) %>%  # ocupa uma entrega
  timeout(function() rexp(1, 1/entrega_app)) %>%
  release("entrega", 1) # libera uma entrega

# trajetoria do pedido via balcao
balcao <- trajectory("balcao") %>%
  seize("atendente", 1) %>%
  timeout(function() rexp(1, 1/atend_balcao)) %>% # definir média de tempo para a atendente anotar o pedido e solicitar preparação
  release("atendente", 1) %>%
  seize("preparação", 1) %>% # ocupa uma preparação
  timeout(function() rexp(1, 1/temp_preparacao)) %>%  # definir média de tempo de preparação
  release("preparação", 1)

# pelo que li no texto, podemos dizer que os pedidos costumam em cada canal em quantidades diferentes:
# telefone: 40%
# balcao: 30%
# whatsapp: 20%
# app: 10%

# adicionando as probabilidades de cada ramificação (canal de atendimento)
pedidos <-trajectory("caminho do pedido") %>%
  branch(option=function() sample(c(1,2,3,4),size = 1, prob=c(.4,.3,.2,.1)),
continue=FALSE, telefone, balcao, whatsapp, app)


# existem os seguintes funcionários:
# 2 atendentes
# 1 garçom
# 7 cozinheiros (preparacao)
# 1 entregador geral

# adicionando os recursos descritos acima
env %>%
  add_resource("atendente", 2) %>%
  add_resource("preparação", 7) %>%
  add_resource("entrega",1)

# adicionando o gerador de pedidos
env %>%
  add_generator("pedido", pedidos, function() rpois(1, pedidos_minuto))


# reset e run
reset(env) %>%
  run(until=tempo_simulacao)

plot(pedidos)
