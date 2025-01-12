# PACOTES -----------------------------------------------------------------
library(janitor)
library(lubridate)
library(purrr)
library(readxl)
library(tidyverse)
library(zoo)
library(car)
library(deflateBR)
library(ipeadatar)
library(lmtest)
library(plm)
library(sidrar)

# 3. LEFTOVER: ANALISE COMPARADA ---------------------------------------------
ipeadatar::search_series(language = "br") %>%
  filter(theme == "Regional") %>% View()

# Lembrete: DFADMM (adm), DFASPRM (assistencia social), DFSAUSM (saude), 
# RTRCORTOM (transferencias)

## 3.1 Baixando dados de receita e despesa ---------------------------------
indicadores <- c("DFADMM", "DFASPRM", "DFSAUSM", "RTRCORTOM")

# Baixando dados de gasto com educacao (IPEA)
gasto <- ipeadata(code = indicadores, language = "br") %>%
  filter(uname == "Municípios") %>%
  select(-uname) %>% 
  pivot_wider(
    names_from = code,
    values_from = value
  ) %>%
  janitor::clean_names()

# Estimativa da populacao (IBGE)
estpop2023 <- read_xls("raw_data/estimativa_2023.xls", 
                       range = "A2:E5572") %>% 
  clean_names() %>% 
  mutate(tcode = paste0(cod_uf, cod_munic),
         ano = 2023,
         date = ymd(paste0(ano, "-01-01"))) %>% 
  rename(estpop = populacao,
         municipio = nome_do_municipio)

# Transformacao
gasto_pcp <- gasto %>% 
  mutate(ano = year(date)) %>% 
  filter(ano == 2023) %>% 
  left_join(estpop2023 %>% select(tcode, ano, estpop) %>% 
              mutate(tcode = as.integer(tcode)), 
            join_by(tcode, ano)) %>% 
  mutate(across(dfadmm:rtrcortom, ~ (.x / estpop), .names = "{.col}_pcp"),
         across(ends_with("_pcp"), ~ log(.x), .names = "log_{.col}"))

## 3.2 Carregando dados a nivel de escola ---------------------------------
## IDEB
anos_finais_escola <- read_excel(
  "raw_data/divulgacao_anos_finais_escolas_2023.xlsx",
  range = "A10:DJ47229", na = "-") %>% clean_names() %>% 
  select(sg_uf, co_municipio, no_municipio, id_escola, no_escola, rede, 
         vl_observado_2023) %>% 
  rename(ideb = vl_observado_2023) %>% 
  mutate(avaliacao = "finais",
         ideb = as.numeric(ideb))

anos_iniciais_escola <- read_excel(
  "raw_data/divulgacao_anos_iniciais_escolas_2023.xlsx",
  range = "A10:DT64915", na = "-") %>% clean_names() %>% 
  select(sg_uf, co_municipio, no_municipio, id_escola, no_escola, rede, 
         vl_observado_2023) %>% 
  rename(ideb = vl_observado_2023) %>% 
  mutate(avaliacao = "finais",
         ideb = as.numeric(ideb))

## Adequacao da formacao docente
afd <- read_excel(
  "raw_data/afd_escolas_2023_t.xlsx", na = "--") %>% clean_names() %>% 
  mutate(across(fun_ai_cat_1:fun_af_cat_5, ~ as.numeric(
    str_replace_all(.x, pattern = ",", 
                    replacement = "."))))

## Indicador de complexidade da gestao da escola
icg <- read_excel(
  "raw_data/icg_escolas_2023_t.xlsx", na = "-") %>% clean_names() %>% 
  mutate(complex = parse_number(complex))

## Indicador de esforco docente
ied <- read_excel(
  "raw_data/ied_escolas_2023_t.xlsx", na = "--") %>% clean_names() %>% 
  rename_with(~ paste0("ied_", .), .cols = starts_with("fun_"))

## Indicador de regularidade do docente
ird <- read_excel(
  "raw_data/ird_escolas_2023_t.xlsx", na = "--") %>% clean_names()

## Base final
ideb_escola_finais <- anos_finais_escola %>% 
  left_join(afd %>% select(co_entidade, fun_af_cat_1:fun_af_cat_5),
            join_by(id_escola == co_entidade)) %>% 
  left_join(icg %>% select(co_entidade, complex),
            join_by(id_escola == co_entidade)) %>% 
  left_join(ied %>% select(co_entidade, ied_fun_af_cat_1:ied_fun_af_cat_6),
            join_by(id_escola == co_entidade)) %>% 
  left_join(ird %>% select(co_entidade, edu_bas_cat_0),
            join_by(id_escola == co_entidade)) %>% 
  left_join(gasto_pcp %>% select(tcode, estpop:log_rtrcortom_pcp),
            join_by(co_municipio == tcode))

ideb_escola_iniciais <- anos_iniciais_escola %>% 
  left_join(afd %>% select(co_entidade, fun_ai_cat_1:fun_ai_cat_5),
            join_by(id_escola == co_entidade)) %>% 
  left_join(icg %>% select(co_entidade, complex),
            join_by(id_escola == co_entidade)) %>% 
  left_join(ied %>% select(co_entidade, ied_fun_ai_cat_1:ied_fun_ai_cat_6),
            join_by(id_escola == co_entidade)) %>% 
  left_join(ird %>% select(co_entidade, edu_bas_cat_0),
            join_by(id_escola == co_entidade)) %>% 
  left_join(gasto_pcp %>% select(tcode, estpop:log_rtrcortom_pcp),
            join_by(co_municipio == tcode))

## 3.3 Regressao linear ---------------------------------
model <- ideb ~ rede + fun_af_cat_1 + complex + ied_fun_af_cat_1 + edu_bas_cat_0
ols <- lm(model, data = ideb_escola_finais)
vif(ols)  

ideb_escola_finais %>% plm(formula = model,
                           data = .,
                           index = c("co_municipio"),
                           model = "within", effect = "individual") %>%
  coeftest(., vcov = vcovHC(., type="HC0", cluster="group"))

ideb_escola_finais %>% plm(formula = model,
                           data = .,
                           index = c("co_municipio"),
                           model = "pooling", effect = "individual") %>%
  coeftest(., vcov = vcovHC(., type="HC0", cluster="group"))

model2 <- ideb ~ rede + fun_ai_cat_1 + complex + ied_fun_ai_cat_1 + 
  edu_bas_cat_0
ols <- lm(model2, data = ideb_escola_iniciais)
vif(ols)  

fitted.model2 <- ideb_escola_iniciais %>% plm(formula = model2,
                                              data = .,
                                              index = c("co_municipio"),
                                              model = "within", effect = "individual") %>%
  coeftest(., vcov = vcovHC(., type="HC0", cluster="group"))

fitted.model2

nobs(fitted.model2)

ideb_escola_iniciais %>% plm(formula = model2,
                             data = .,
                             index = c("co_municipio"),
                             model = "within", effect = "individual") %>% 
  r.squared()