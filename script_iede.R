# PACOTES -----------------------------------------------------------------
library(deflateBR)
library(ipeadatar)
library(janitor)
library(lubridate)
library(purrr)
library(readxl)
library(sidrar)
library(tidyverse)
library(zoo)

# 1. SELECIONANDO O MUNICIPIO ---------------------------------------------
ipeadatar::search_series(language = "br") %>%
  filter(theme == "Regional") %>% View()

# Baixando dados de gasto com educacao (IPEA)
gasto_educacao <- ipeadata(code = "DFEDUCM", language = "br") %>%
  filter(uname == "Municípios") %>%
  select(-uname) %>% 
  pivot_wider(
    names_from = code,
    values_from = value
  ) %>%
  janitor::clean_names()

# Baixando dados de populacao
periodo <- as.character(seq(2002, 2024))

pop <- purrr::map(periodo,
                  ~get_sidra(6579, geo = "City",
                             period = .x)) %>%
  list_rbind() %>%
  clean_names() %>%
  rename(estpop = valor, tcode = municipio_codigo) %>%
  mutate(tcode = as.double(tcode),
         ano = as.double(ano))

sidra_estpop <- pop %>% mutate(date = ymd(paste0(ano, "-01-01")))

# Salvando dados de projecao populacional do sidra
write_rds(sidra_estpop, "int_data/sidra_estpop.RDS")

estpop2023 <- read_xls("raw_data/estimativa_2023.xls", 
                       range = "A2:E5572") %>% 
  clean_names() %>% 
  mutate(tcode = paste0(cod_uf, cod_munic),
         ano = 2023,
         date = ymd(paste0(ano, "-01-01"))) %>% 
  rename(estpop = populacao,
         municipio = nome_do_municipio)

estpop2024 <- read_xls("raw_data/estimativa_2024.xls", 
                       sheet = 2,
                       range = "A2:E5572") %>% 
  clean_names() %>% 
  mutate(tcode = paste0(cod_uf, cod_munic),
         ano = 2024,
         date = ymd(paste0(ano, "-01-01"))) %>% 
  rename(estpop = populacao_estimada,
         municipio = nome_do_municipio)

populacao <- rbind(sidra_estpop %>% select(date, tcode, municipio, estpop),
                   estpop2023 %>% select(date, tcode, municipio, estpop),
                   estpop2024 %>% select(date, tcode, municipio, estpop)) %>% 
  complete() %>% 
  arrange(date, tcode) %>% 
  filter(estpop != "...") %>% 
  complete(date = seq.Date(min(date), max(date), by = "year"),
           tcode) %>% 
  group_by(tcode) %>%
  mutate(estpop = na.approx(estpop, na.rm = F)) %>% 
  ungroup() %>% 
  mutate(tcode = as.numeric(tcode))

# Calculando a variacao percentual
gasto_tratado <- gasto_educacao %>% 
  left_join(populacao %>% select(tcode, date, estpop), 
            join_by(date, tcode)) %>% 
  group_by(tcode) %>% 
  arrange(date) %>% 
  mutate(dfeducm = ifelse(dfeducm == 0, NA, dfeducm),
         dfeducm = na.approx(dfeducm, na.rm = F)) %>% 
  ungroup() 

gasto_var <- gasto_tratado %>% 
  mutate(dfeducm = deflate(nominal_values = gasto_tratado$dfeducm,
                           nominal_dates = gasto_tratado$date,
                           index = "ipca",
                           real_date = '01/2010'),
         dfeducm_pcp = dfeducm/estpop) %>%
  group_by(tcode) %>% 
  reframe(var_per = (
    dfeducm_pcp[which(date=="2023-01-01")] - 
    dfeducm_pcp[which(date=="2005-01-01")]
  ) / 
    dfeducm_pcp[which(date=="2005-01-01")] * 100)

gasto_var %>% arrange(desc(var_per))

gasto_var %>% summarise(mean = mean(var_per))

# 2. ANALISE EDUCACIONAL --------------------------------------------------

# 2.1 ANALISE AGREGADA ------------------------------------------------------
anos_finais_muni <- read_excel(
  "raw_data/divulgacao_anos_finais_municipios_2023.xlsx",
  range = "A10:DH14411", na = "-") %>% clean_names()

anos_iniciais_muni <- read_excel(
  "raw_data/divulgacao_anos_iniciais_municipios_2023.xlsx",
  range = "A10:DR14507", na = "-") %>% clean_names()

long_ideb_finais_muni <- anos_finais_muni %>% 
  select(sg_uf, co_municipio, no_municipio, rede,
         vl_observado_2005:vl_observado_2023) %>% 
  pivot_longer(cols = vl_observado_2005:vl_observado_2023,
               names_to = "ano", values_to = "ideb") %>% 
  mutate(ano = parse_number(ano),
         avaliacao = "finais")

long_ideb_iniciais_muni <- anos_iniciais_muni %>% 
  select(sg_uf, co_municipio, no_municipio, rede,
         vl_observado_2005:vl_observado_2023) %>% 
  pivot_longer(cols = vl_observado_2005:vl_observado_2023,
               names_to = "ano", values_to = "ideb") %>% 
  mutate(ano = parse_number(ano),
         avaliacao = "iniciais")

df_ideb_muni <- rbind(long_ideb_finais_muni, long_ideb_iniciais_muni)

df_ideb_muni %>% 

df_ideb_muni %>% 
  ungroup() %>% 
  filter(co_municipio == 2312908) %>% 
  ggplot(aes(x = ano, y = ideb, group = avaliacao, color = avaliacao)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.0) +
  scale_color_manual(
    values = c("iniciais" = "lightblue", "finais" = "darkblue"),
    labels = c("iniciais" = "Anos Iniciais", "finais" = "Anos Finais")
  ) +
  labs(color = "Série", x = "Ano",
       y = "IDEB") +
  theme_minimal() +
  facet_wrap(~ rede, nrow = 2)

df_ideb_muni %>% 
  group_by(ano, rede, avaliacao) %>% 
  mutate(media = mean(ideb, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(ideb, media), names_to = "tipo", values_to = "value") %>% 
  filter(co_municipio == 2312908, rede == "Pública") %>% 
  ggplot(aes(x = ano, y = value, group = tipo, color = tipo)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.0) +
  labs(x = "Ano",
       y = "IDEB",
       color = "") +
  scale_color_manual(
    values = c("media" = "lightblue", "ideb" = "darkblue"),
    labels = c("media" = "Média do Brasil", "ideb" = "IDEB do Município")) +
  theme_minimal() +
  facet_wrap(~ avaliacao, nrow = 2)

# ANALISE DESAGREGADA -----------------------------------------------------
anos_finais_escola <- read_excel(
  "raw_data/divulgacao_anos_finais_escolas_2023.xlsx",
  range = "A10:DJ47229", na = "-") %>% clean_names()

anos_iniciais_escola <- read_excel(
  "raw_data/divulgacao_anos_iniciais_escolas_2023.xlsx",
  range = "A10:DT64915", na = "-") %>% clean_names()