if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, simmer, simmer.plot, remotes)
remotes::install_github("r-simmer/simmer.optim")

#leitura dos dados ----
dados <- readxl::read_xlsx("data/Dados Restaurante.xlsx") %>%
  mutate(
    `Tempo no Canal` = force_tz(`Tempo no Canal`, tzone = "UTC"),
    `Tempo de Entrega` = force_tz(`Tempo de Entrega`, tzone = "UTC"),
    `Tempo no Canal` = minute(`Tempo no Canal`),
    `Tempo de Entrega` = minute(`Tempo de Entrega`)
  )

# tabela das médias de tempo de entrega por canal ----
# aqui estou pensando como separar as médias. o balcão obviamente é separado dos demais,
# porém, não sei se o entregador é o mesmo para as demais atividades. acredito que não faz 
# sentido separar as médias de whatsapp e telefone, pois é o mesmo entregador, logo, o canal não
# deveria influenciar
# agora o APP eu não sei se devemos considerar a mesma média, pois pode ser q o entregador seja da
# plataforma, e não do restaurante... pelo que eu li, existe ambas opções: entregador da plat. ou da loja.
tempo_entrega_pcanal <- dados %>%
  group_by(`Tipo de Canal`) %>%
  summarise("Tempo médio de entrega" = mean(`Tempo de Entrega`))

entrega_telwpp <- mean(tempo_entrega_pcanal$`Tempo médio de entrega`[2:4])

# tabela das médias de tempo de atendimento por canal ----
# aqui existe um problema: o trabalho considerou que o atendimento é o tempo para obter o pedido + preparação
# o que não faz sentido, pois existem dois atendentes: uma que irá passar o pedido e ser liberada para atender outro cliente
# e um cozinheiro que irá fazer o pedido
# o tempo para anotar um pedido geralmente tem médida de poucos minutos, dependendo do canal.
# já a preparação demora mais.
# temos que separar esse resultado e definir o tempo médio das duas atividades...
tempo_atendim_pcanal <- dados %>%
  group_by(`Tipo de Canal`) %>%
  summarise("Tempo médio no Canal" = mean(`Tempo no Canal`))

# tempo de produção do pedido ----
# aparentemente é algo em torno de 20 a 30 minutos...
temp_preparacao <- 20

# atendimento subjetivamente definido
atend_balcao <- tempo_atendim_pcanal$`Tempo médio no Canal`[1]-temp_preparacao
atend_app <- tempo_atendim_pcanal$`Tempo médio no Canal`[2]-temp_preparacao
atend_tel <- tempo_atendim_pcanal$`Tempo médio no Canal`[3]-temp_preparacao
atend_wpp <- tempo_atendim_pcanal$`Tempo médio no Canal`[4]-temp_preparacao


# alguns dados interessantes disponibilizados pela autora ----
pedidos_hora <- 12.40 # média de pedidos q chegam por hora. segue poisson
pedidos_minuto <- pedidos_hora/60
atendimento_hora <- 2.29  # média de pedidos atendidos por hora
atendimento_minuto <- 2.29/60 # média de pedidos atendidos por minuto
tempo_atendimento <- 26.21 # tempo médio de atendimento por pedido

tempo_simulacao <- 6*60 



# modelo ----
set.seed(8273)

env <- simmer("Restaurante")   # ambiente
# pelo que li no texto, podemos dizer que os pedidos costumam em cada canal em quantidades diferentes:
# telefone: 40%
# balcao: 30%
# whatsapp: 20%
# app: 10%

# adicionando as probabilidades de cada ramificação (canal de atendimento)

pedidos <- trajectory("pedido") %>%
  seize("atendente", 1) %>%
  branch(
    function() sample(1:4, size = 1, prob=c(.4,.3,.2,.1)),
    c(TRUE, TRUE, TRUE, FALSE),
    trajectory("whatsapp") %>%
      timeout(function() rexp(1, 1/atend_wpp)), # definir média de tempo para a atendente anotar o pedido e solicitar preparação
    trajectory("telefone") %>%
      timeout(function() rexp(1, 1/atend_tel)), # definir média de tempo para a atendente anotar o pedido e solicitar preparação
    trajectory("app") %>%
      timeout(function() rexp(1, 1/atend_app)),
    trajectory("balcao") %>%
      timeout(function() rexp(1, 1/atend_balcao)) %>%
      release("atendente", 1) %>%
      seize("preparação", 1) %>% # ocupa uma preparação
      timeout(function() rexp(1, 1/temp_preparacao)) %>%  # definir média de tempo de preparação
      release("preparação", 1)
  ) %>%
  release("atendente", 1) %>%
  seize("preparação", 1) %>% # ocupa uma preparação
  timeout(function() rexp(1, 1/temp_preparacao)) %>%  # definir média de tempo de preparação
  release("preparação", 1) %>%
  seize("entrega", 1) %>%
  timeout(function() rexp(1, 1/entrega_telwpp)) %>%
  release("entrega", 1)


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
  add_generator("pedido", pedidos, function() rexp(1,pedidos_minuto))

# reset e run
reset(env) %>%
  run(until=tempo_simulacao)

# várias simulações simultaneas ----
envs <- lapply(1:100, function(i){
  simmer() %>%
    add_generator("pedido", pedidos, function() rexp(1, pedidos_minuto)) %>%
    add_resource("atendente", 2) %>%
    add_resource("preparação", 7) %>%
    add_resource("entrega",1) %>%
    run(until=tempo_simulacao)
})


# plot do tempo medio de espera ----
plot(get_mon_arrivals(env), "waiting_time", ann = FALSE)
ggsave('img/umasimulacaoreal.pdf', width = 7, height = 5)
plot(get_mon_arrivals(envs), "waiting_time", ann = FALSE)


# Quantidade de clientes atendidos no sistema: ----
get_mon_arrivals(envs) %>%
  group_by(replication) %>%
  summarise(n=n()) %>%
  .$n %>% mean

# media do numero de clientes servidos antes do tempo previsto (arbitrariamente 30min): ----
get_mon_arrivals(envs) %>%
  filter(activity_time < 30) %>%
  group_by(replication) %>%
  summarise(n=n()) %>%
  .$n %>% mean

prop_atend <- 30.16/46.29


# utilização dos recursos na simulação lote ----
resources <- get_mon_resources(envs)
plot(resources, metric = "utilization")
ggsave('img/recursos_sima.pdf', width = 7, height = 5)


# otimização ----

# quantidade de combinações possiveis
expand.grid(n_atendentes = 1:3,
            n_cozinheiros = 5:10,
            n_entregadores = 1:2) %>% nrow()


# valor da hora pelo salario minimo:8.25
assert_within_budget<-function(envs, budget){
  runtime_hrs <- msr_runtime(envs) / 60
  number_atendentes <- msr_resource_capacity(envs, "atendente")
  number_cozinheiros <- msr_resource_capacity(envs, "preparação")
  number_entregadores <- msr_resource_capacity(envs, "entrega")
  
  (number_atendentes * runtime_hrs * 8.25 +
      number_cozinheiros * runtime_hrs * 8.25 +
      number_entregadores * runtime_hrs * 8.25) <= budget
  
}

library(simmer.optim)
# criando o modelo
sim_model<-function(){
  pedidos <- trajectory("pedido") %>%
    seize("atendente", 1) %>%
    branch(
      function() sample(1:4, size = 1, prob=c(.4,.3,.2,.1)),
      c(TRUE, TRUE, TRUE, FALSE),
      trajectory("whatsapp") %>%
        timeout(function() rexp(1, 1/atend_wpp)),
      trajectory("telefone") %>%
        timeout(function() rexp(1, 1/atend_tel)),
      trajectory("app") %>%
        timeout(function() rexp(1, 1/atend_app)),
      trajectory("balcao") %>%
        timeout(function() rexp(1, 1/atend_balcao)) %>%
        release("atendente", 1) %>%
        seize("preparação", 1) %>% 
        timeout(function() rexp(1, 1/temp_preparacao)) %>%  
        release("preparação", 1)
    ) %>%
    release("atendente", 1) %>%
    seize("preparação", 1) %>% # ocupa uma preparação
    timeout(function() rexp(1, 1/temp_preparacao)) %>%  # definir média de tempo de preparação
    release("preparação", 1) %>%
    seize("entrega", 1) %>%
    timeout(function() rexp(1, 1/entrega_telwpp)) %>%
    release("entrega", 1)
  
  env<-simmer() %>%
    add_generator("pedido", pedidos, function() rexp(1, pedidos_minuto)) %>%
    add_resource("atendente",.opt('n_atendentes')) %>%
    add_resource("preparação",.opt('n_cozinheiros')) %>%
    add_resource("entrega",.opt('n_entregadores')) %>%
    run(until=tempo_simulacao)
  
  env
}




# otimizando o modelo
set.seed(8118)
r <- simmer_optim(
  model = sim_model,
  method = grid_optim,
  direction = "max",
  objective = msr_arrivals_finished,
  constraints = list(with_args(assert_avg_waiting_time_max, max_val = 45),
                     with_args(assert_within_budget, budget = 1000)),
  params = list(n_atendentes = c(1:3),
                n_cozinheiros = c(5:10),
                n_entregadores = c(1:2)),
  control = optim_control(run_args = list(until=tempo_atendimento)))

r

# utilizando os paramêtros encontrados no modelo do restaurante
env_opt <- simmer("otimizado")

env_opt %>%
  add_resource("atendente", 1) %>%
  add_resource("preparação", 5) %>%
  add_resource("entrega",2)

# adicionando o gerador de pedidos
env_opt %>%
  add_generator("pedido", pedidos, function() rexp(1, pedidos_minuto))

# reset e run
reset(env_opt) %>%
  run(until=tempo_simulacao)



# várias simulações simultaneas ----
envs2 <- lapply(1:100, function(i){
  simmer() %>%
    add_generator("pedido", pedidos, function() rexp(1, pedidos_minuto)) %>%
    add_resource("atendente", 1) %>%
    add_resource("preparação", 5) %>%
    add_resource("entrega", 2) %>%
    run(until=tempo_simulacao)
})


# plot do tempo medio de espera ----
plot(get_mon_arrivals(env_opt), "waiting_time", ann = FALSE)
ggsave("img/umasimulacao_opt.pdf", width=7, height=5)

# Quantidade de clientes atendidos no sistema: ----
get_mon_arrivals(envs2) %>%
  group_by(replication) %>%
  summarise(n=n()) %>%
  .$n %>% mean

# numero de clientes servidos antes do tempo previsto (arbitrariamente 30min): ----
get_mon_arrivals(envs2) %>%
  filter(activity_time < 30) %>%
  group_by(replication) %>%
  summarise(n=n()) %>%
  .$n %>% mean

prop_opt <- 40.83/63.64

# utilização dos recursos na simulação lote otimizada ----
resources <- get_mon_resources(envs2)
plot(resources, metric = "utilization")
ggsave('img/recursos_simb.pdf', width = 7, height = 5)
