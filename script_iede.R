# PACOTES -----------------------------------------------------------------
library(janitor)
library(lubridate)
library(purrr)
library(readxl)
library(tidyverse)
library(zoo)

# 1. ANALISE AGREGADA ------------------------------------------------------
## 1.1 IDEB ----------------------------------------------------------------
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
  mutate(across(starts_with("vl_"), 
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
  mutate(across(starts_with(c("vl_")), 
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
                                   labels = c("Turmas iniciais", 
                                              "Turmas finais")))) %>% 
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
    labels = c("iniciais" = "Turmas iniciais", "finais" = "Turmas finais")
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
    labels = c("iniciais" = "Turmas iniciais", "finais" = "Turmas finais")
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
    labels = c("iniciais" = "Turmas iniciais", "finais" = "Turmas finais")
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

## 1.2 DISTORCAO IDADE-SERIE --------------------------------------------------
tdi_extr <- function(x, y, w, z){
  as.name <- substr(w, start = 1, stop = 4)
  
  df <- read_excel(paste0("raw_data/tdi_", w, "_", x, ".xlsx"),
                   range = paste0(z, ":", y), na = "--") %>% clean_names()
  assign(paste0("tdi_", as.name, "_", x), df, envir = .GlobalEnv)
}

tdi_coord <- tibble(x = c(2019,2020,2021,2022,2023),
                    y = c("X65657", "X65637", "X65547", "X65557", "X65581"),
                    w = rep("municipios", 5),
                    z = rep("A9", 5))

tdi_coord %>% 
  pmap(~ tdi_extr(..1, ..2, ..3, ..4))

tdi_muni <- rbind(tdi_muni_2019, tdi_muni_2020, tdi_muni_2021, tdi_muni_2022,
                  tdi_muni_2023)

# PLOTS
gg_tdi_muni <- tdi_muni %>% filter(co_municipio == 2312908, 
                    no_dependencia == "Pública", no_categoria == "Total") %>% 
  ggplot(aes(x = nu_ano_censo)) +
  geom_line(aes(y = fun_ai_cat_0, color = "lightblue"), linewidth = 1.0) +
  geom_line(aes(y = fun_af_cat_0, color = "darkblue"), linewidth = 1.0) +
  geom_point(aes(y = fun_ai_cat_0, color = "lightblue"), size = 2.0) +
  geom_point(aes(y = fun_af_cat_0, color = "darkblue"), size = 2.0) +
  scale_color_manual(values = c("lightblue", "darkblue"),
                     labels = c("Turmas Iniciais", "Turmas Finais")) +
  labs(x = "Ano",
       y = "Taxa de Distorção Idade-Série",
       title = "Distorção Idade-Série em Escolas Públicas", 
       color = "Turma") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gg_tdi_muni

ggsave('plot/gg_tdi_muni.jpeg', plot = gg_tdi_muni, dpi = 500, 
       height = 3, width = 6, units = 'in')

# 2. ANALISE DESAGREGADA ------------------------------------------------------
## 2.1 IDEB ----------------------------------------------------------------
# TRANSFORMANDO EM FORMATO TIDY

# FINAIS
anos_finais_escola <- read_excel(
  "raw_data/divulgacao_anos_finais_escolas_2023.xlsx",
  range = "A10:DJ47229", na = "-") %>% clean_names() %>% 
  mutate(across(starts_with("vl_"), ~as.numeric(
    str_replace(string = .x, pattern = ",", 
                replacement = ".")
  )))

long_ideb_finais_escola <- anos_finais_escola %>% 
  select(sg_uf, co_municipio, no_municipio, no_escola, id_escola, rede,
         vl_observado_2005:vl_observado_2023,
         starts_with(c("vl_nota_matematica", "vl_nota_portugues"))) %>% 
  pivot_longer(cols = starts_with(c("vl_nota_matematica", "vl_nota_portugues",
                                    "vl_observado")),
               names_to = "x", values_to = "nota") %>% 
  mutate(ano = parse_number(x),
         x = str_extract_all(string = x, 
                             pattern = "matematica|portugues|observado"),
         avaliacao = "finais") %>% 
  pivot_wider(names_from = x, values_from = nota) %>% 
  rename(ideb = observado)

# INICIAIS
anos_iniciais_escola <- read_excel(
  "raw_data/divulgacao_anos_iniciais_escolas_2023.xlsx",
  range = "A10:DT64915", na = "-") %>% clean_names() %>% 
  mutate(across(starts_with("vl_"), ~as.numeric(
    str_replace(string = .x, pattern = ",", 
                replacement = ".")
  )))


long_ideb_iniciais_escola <- anos_iniciais_escola %>% 
  select(sg_uf, co_municipio, no_municipio, no_escola, id_escola, rede,
         vl_observado_2005:vl_observado_2023,
         starts_with(c("vl_nota_matematica", "vl_nota_portugues"))) %>% 
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
df_ideb_escola <- rbind(long_ideb_finais_escola, long_ideb_iniciais_escola)

# PLOTS
anos_finais_escola %>% filter(co_municipio == 2312908) %>% 
  ggplot(aes(x = vl_observado_2019, y = vl_observado_2023)) +
  geom_point()

anos_finais_escola %>% filter(co_municipio == 2312908) %>% 
  ggplot(aes(x = vl_observado_2019)) +
  geom_histogram(bins = 5)

long_ideb_finais_escola %>% filter(co_municipio == 2312908) %>% 
  mutate(ano = as.factor(ano)) %>% 
  ggplot(aes(x = ano, y = ideb)) +
  geom_boxplot() +
  coord_flip()

long_ideb_iniciais_escola %>% filter(co_municipio == 2312908) %>% 
  mutate(ano = as.factor(ano)) %>% 
  ggplot(aes(x = ano, y = matematica)) +
  geom_boxplot() +
  coord_flip()


## 2.2 DISTORCAO IDADE-SERIE -------------------------------------------------
tdi_escola_2019 <- read_csv2("raw_data/tdi_escolas_2019.csv", na = "--") %>% 
  clean_names() %>% 
  mutate(across(fun_cat_0:med_cat_0, ~str_replace(string = .x, pattern = ",", 
                                                  replacement = ".")),
         across(fun_cat_0:med_cat_0, ~ as.numeric(.x)))

tdi_escola_2023 <- read_csv2("raw_data/tdi_escolas_2023.csv", na = "--") %>% 
  clean_names() %>% 
  mutate(across(fun_cat_0:med_cat_0, ~str_replace(string = .x, pattern = ",", 
                                                  replacement = ".")),
         across(fun_cat_0:med_cat_0, ~ as.numeric(.x)))

tdi_escola <- rbind(tdi_escola_2019, tdi_escola_2023) %>% 
  clean_names() %>% 
  select(c(nu_ano_censo:fun_af_cat_0, -fun_cat_0)) %>% 
  pivot_wider(names_from = nu_ano_censo,
              values_from = starts_with("fun_a"))

tdi_escola %>% filter(co_municipio == 2312908) %>% ggplot() +
  geom_point(aes(x = fun_af_cat_0_2019, y = fun_af_cat_0_2023))

tdi_escola %>% filter(co_municipio == 2312908) %>% ggplot() +
  geom_point(aes(x = fun_ai_cat_0_2019, y = fun_ai_cat_0_2023))