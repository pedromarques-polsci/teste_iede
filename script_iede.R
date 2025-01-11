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
# EXTRACAO
anos_finais_muni <- read_excel(
  "raw_data/divulgacao_anos_finais_municipios_2023.xlsx",
  range = "A10:DH14411", na = "-") %>% clean_names()

anos_iniciais_muni <- read_excel(
  "raw_data/divulgacao_anos_iniciais_municipios_2023.xlsx",
  range = "A10:DR14507", na = "-") %>% clean_names()

# TRANSFORMANDO EM FORMATO TIDY
long_ideb_finais_muni <- anos_finais_muni %>% 
  select(sg_uf, co_municipio, no_municipio, rede,
         vl_observado_2005:vl_observado_2023,
         starts_with(c("vl_nota_matematica", "vl_nota_portugues"))
  ) %>%  
  mutate(across(starts_with(c("vl_nota_matematica", "vl_nota_portugues")), 
                ~ as.numeric(
                  str_replace(string = .x, pattern = ",", 
                                         replacement = ".")
                  )
                )
         ) %>% 
  pivot_longer(cols = starts_with(c("vl_nota_matematica", "vl_nota_portugues",
                                    "vl_observado")),
               names_to = "x", values_to = "nota") %>% 
  mutate(ano = parse_number(x),
         x = str_extract_all(string = x, 
                             pattern = "matematica|portugues|observado"),
         avaliacao = "finais") %>% 
  pivot_wider(names_from = x, values_from = nota) %>% 
  rename(ideb = observado)

long_ideb_iniciais_muni <- anos_iniciais_muni %>% 
  select(sg_uf, co_municipio, no_municipio, rede,
         vl_observado_2005:vl_observado_2023,
         starts_with(c("vl_nota_matematica", "vl_nota_portugues"))
  ) %>%  
  mutate(across(starts_with(c("vl_nota_matematica", "vl_nota_portugues")), 
                ~ as.numeric(
                  str_replace(string = .x, pattern = ",", 
                              replacement = ".")
                )
  )
  ) %>% 
  pivot_longer(cols = starts_with(c("vl_nota_matematica", "vl_nota_portugues",
                                    "vl_observado")),
               names_to = "x", values_to = "nota") %>% 
  mutate(ano = parse_number(x),
         x = str_extract_all(string = x, 
                             pattern = "matematica|portugues|observado"),
         avaliacao = "iniciais") %>% 
  pivot_wider(names_from = x, values_from = nota) %>% 
  rename(ideb = observado)

# JUNTANDO BASES
df_ideb_muni <- rbind(long_ideb_finais_muni, long_ideb_iniciais_muni)

# SOBRAL VS MEDIA BRASILEIRA PARA ESCOLAS PUBLICAS
sobral_vs_br <- df_ideb_muni %>% 
  group_by(ano, rede, avaliacao) %>% 
  mutate(media = mean(ideb, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(ideb, media), names_to = "tipo", 
               values_to = "value") %>% 
  mutate(across(avaliacao, ~factor(., levels=c("iniciais", "finais"),
                                   labels = c("Anos iniciais", 
                                              "Anos finais")))) %>% 
  filter(co_municipio == 2312908, rede == "Pública")

# PLOTS

# TRAJETORIAS DO IDEB DE SOBRAL
gg_ideb_ts <- df_ideb_muni %>% 
  ungroup() %>% 
  mutate(avaliacao = factor(avaliacao, levels = c("iniciais", "finais"))) %>% 
  filter(co_municipio == 2312908, rede == "Pública") %>% 
  ggplot(aes(x = ano, y = ideb, group = avaliacao, color = avaliacao)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.0) +
  scale_color_manual(
    values = c("iniciais" = "lightblue", "finais" = "darkblue"),
    labels = c("iniciais" = "Anos Iniciais", "finais" = "Anos Finais")
  ) +
  labs(color = "Série", x = "Ano",
       y = "IDEB",
       title = "Trajetória do IDEB (Escolas Públicas)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gg_ideb_ts

ggsave('plot/gg_ideb_ts.jpeg', plot = gg_ideb_ts, dpi = 500, 
       height = 3, width = 6, units = 'in')

gg_mat_ts <- df_ideb_muni %>% 
  ungroup() %>% 
  mutate(avaliacao = factor(avaliacao, levels = c("iniciais", "finais"))) %>% 
  filter(co_municipio == 2312908, rede == "Pública") %>% 
  ggplot(aes(x = ano, y = matematica, group = avaliacao, color = avaliacao)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.0) +
  scale_color_manual(
    values = c("iniciais" = "lightblue", "finais" = "darkblue"),
    labels = c("iniciais" = "Anos Iniciais", "finais" = "Anos Finais")
  ) +
  labs(color = "Série", x = "Ano",
       y = "Proficiência em Matemática",
       title = "Trajetória da Proficiência em Matemática (Escolas Públicas)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gg_mat_ts

ggsave('plot/gg_mat_ts.jpeg', plot = gg_mat_ts, dpi = 500, 
       height = 3, width = 6, units = 'in')

gg_pt_ts <- df_ideb_muni %>% 
  ungroup() %>% 
  mutate(avaliacao = factor(avaliacao, levels = c("iniciais", "finais"))) %>% 
  filter(co_municipio == 2312908, rede == "Pública") %>% 
  ggplot(aes(x = ano, y = portugues, group = avaliacao, color = avaliacao)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.0) +
  scale_color_manual(
    values = c("iniciais" = "lightblue", "finais" = "darkblue"),
    labels = c("iniciais" = "Anos Iniciais", "finais" = "Anos Finais")
  ) +
  labs(color = "Série", x = "Ano",
       y = "Proficiência em Português",
       title = "Trajetória da Proficiência em Português (Escolas Públicas)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gg_pt_ts

ggsave('plot/gg_pt_ts.jpeg', plot = gg_pt_ts, dpi = 500, 
       height = 3, width = 6, units = 'in')

# COMPARANDO A TRAJETORIA DAS ESCOLAS PUBLICAS DE SOBRAL COM A MEDIA BRASILEIRA
gg_sobral_ideb_media <- sobral_vs_br %>% 
  ggplot(aes(x = ano, y = value, group = tipo, 
             color = tipo, linetype = tipo,
             )) + 
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.0) +
  labs(x = "Ano",
       y = "IDEB",
       color = "Legenda", 
       linetype = "Legenda") +
  scale_color_manual(
    values = c("media" = "gray70", "ideb" = "gray1"), 
    labels = c("media" = "Média do Brasil", "ideb" = "IDEB de Sobral")) +
  scale_linetype_manual(
    values = c("media" = "dashed", "ideb" = "solid"), 
    labels = c("media" = "Média do Brasil", "ideb" = "IDEB de Sobral")) +
  labs(title = "Sobral versus média brasileira
  (Escolas Públicas)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ avaliacao, nrow = 2, labeller = label_value)

gg_sobral_ideb_media

ggsave('plot/gg_sobral_ideb_media.jpeg', plot = gg_sobral_ideb_media, dpi = 500, 
       height = 5, width = 6, units = 'in')

# COMPARANDO A RECUPERACAO
ideb_rec <- df_ideb_muni %>% group_by(co_municipio, avaliacao) %>% 
  filter(rede == "Pública") %>% 
  reframe(recuperacao = (
    ideb[which(ano == 2023)] - ideb[which(ano == 2019)]) / 
      ideb[which(ano == 2019)] * 100
    ) %>% group_by(avaliacao) %>% 
  mutate(media_recuperacao = mean(recuperacao, na.rm = T)) %>% 
  ungroup()

mat_rec <- df_ideb_muni %>% group_by(co_municipio, avaliacao) %>% 
  filter(rede == "Pública") %>% 
  reframe(recuperacao = (
    matematica[which(ano == 2023)] - matematica[which(ano == 2019)]) / 
      matematica[which(ano == 2019)] * 100
  ) %>% group_by(avaliacao) %>% 
  mutate(media_recuperacao = mean(recuperacao, na.rm = T)) %>% 
  ungroup()

pt_rec <- df_ideb_muni %>% group_by(co_municipio, avaliacao) %>% 
  filter(rede == "Pública") %>% 
  reframe(recuperacao = (
    portugues[which(ano == 2023)] - portugues[which(ano == 2019)]) / 
      portugues[which(ano == 2019)] * 100
  ) %>% group_by(avaliacao) %>% 
  mutate(media_recuperacao = mean(recuperacao, na.rm = T)) %>% 
  ungroup()

pt_rec %>% 
  group_by(avaliacao) %>% 
  summarize(min = min(recuperacao, na.rm = T),
            q1 = quantile(recuperacao, 0.25, na.rm = T),
            q2 = quantile(recuperacao, 0.50, na.rm = T),
            mean = mean(recuperacao, na.rm = T),
            q3 = quantile(recuperacao, 0.75, na.rm = T),
            max = max(recuperacao, na.rm = T),
            sd = sd(recuperacao, na.rm = T)) %>% 
  left_join(pt_rec %>% filter(co_municipio == 2312908) %>% select(avaliacao,
                                                                  recuperacao),
            join_by(avaliacao))

mat_rec %>% 
  group_by(avaliacao) %>% 
  summarize(min = min(recuperacao, na.rm = T),
            q1 = quantile(recuperacao, 0.25, na.rm = T),
            q2 = quantile(recuperacao, 0.50, na.rm = T),
            mean = mean(recuperacao, na.rm = T),
            q3 = quantile(recuperacao, 0.75, na.rm = T),
            max = max(recuperacao, na.rm = T),
            sd = sd(recuperacao, na.rm = T)) %>% 
  mutate_if(is.numeric, format, 1, digits = 5) %>% 
  left_join(mat_rec %>% filter(co_municipio == 2312908) %>% select(avaliacao,
                                                                  recuperacao),
            join_by(avaliacao))

ideb_rec %>% 
  group_by(avaliacao) %>% 
  summarize(min = min(recuperacao, na.rm = T),
            q1 = quantile(recuperacao, 0.25, na.rm = T),
            q2 = quantile(recuperacao, 0.50, na.rm = T),
            mean = mean(recuperacao, na.rm = T),
            q3 = quantile(recuperacao, 0.75, na.rm = T),
            max = max(recuperacao, na.rm = T),
            sd = sd(recuperacao, na.rm = T)) %>% 
  mutate_if(is.numeric, format, 1, digits = 5) %>% 
  left_join(ideb_rec %>% filter(co_municipio == 2312908) %>% select(avaliacao,
                                                                   recuperacao),
            join_by(avaliacao))

# ANALISE DESAGREGADA -----------------------------------------------------
anos_finais_escola <- read_excel(
  "raw_data/divulgacao_anos_finais_escolas_2023.xlsx",
  range = "A10:DJ47229", na = "-") %>% clean_names()

anos_iniciais_escola <- read_excel(
  "raw_data/divulgacao_anos_iniciais_escolas_2023.xlsx",
  range = "A10:DT64915", na = "-") %>% clean_names()