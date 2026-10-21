require(openxlsx)            
require(dplyr)               
require(gtsummary)           
require(DescTools)          
require(rstatix)            
require(ggplot2)             
require(qqplotr)             

############## Quantitativo

dados <- read.xlsx("Nutrição.xlsx")


theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

tabela_quantitativa <- dados %>%
  select(Embutidos, Idade, Peso, Altura, IMC) %>%
  tbl_summary(
    by = Embutidos,
    statistic = all_continuous() ~ "{mean} ({sd}) | {median} [{p25}, {p75}]",
    digits = all_continuous() ~ 2,
    missing = "no",
    label = list(
      Idade  ~ "Idade (anos)",
      Peso   ~ "Peso (kg)",
      Altura ~ "Altura (m)",
      IMC    ~ "IMC (kg/m²)"
    )
  ) %>%
  add_p(test = all_continuous() ~ "oneway.test") %>% # ANOVA de Welch
  bold_p(t = 0.05) %>%
  modify_spanning_header(all_stat_cols() ~ "**Mudança no Consumo de Embutidos**") %>%
  modify_header(label ~ "**Variáveis Quantitativas**") %>%
  bold_labels() %>%
  modify_footnote(everything() ~ "Estatísticas apresentadas: Média (DP) | Mediana [Q1, Q3]. Valor-p via ANOVA de Welch.")


tabela_quantitativa

# 4. DIAGNÓSTICO DE HOMOGENEIDADE DE VARIÂNCIAS (TESTE DE LEVENE)
# H0: Variâncias homogêneas vs H1: Variâncias heterogêneas
cat("=== Teste de Levene: Idade ===\n")
print(LeveneTest(Idade ~ as.factor(Embutidos), data = dados))

cat("\n=== Teste de Levene: Peso ===\n")
print(LeveneTest(Peso ~ as.factor(Embutidos), data = dados))

cat("\n=== Teste de Levene: IMC ===\n")
print(LeveneTest(IMC ~ as.factor(Embutidos), data = dados))

cat("\n=== Teste de Levene: Altura ===\n")
print(LeveneTest(Altura ~ as.factor(Embutidos), data = dados))

# 5. ANOVA DE WELCH (COMPARAÇÃO DE MÉDIAS SOB HETEROCEDASTICIDADE)
# Justificada pela rejeição da homocedasticidade pelo teste de Levene (p < 0.001)
cat("\n=== ANOVA de Welch: IMC ===\n")
print(oneway.test(IMC ~ Embutidos, data = dados, var.equal = FALSE))

cat("\n=== ANOVA de Welch: Peso ===\n")
print(oneway.test(Peso ~ Embutidos, data = dados, var.equal = FALSE))

cat("\n=== ANOVA de Welch: Idade ===\n")
print(oneway.test(Idade ~ Embutidos, data = dados, var.equal = FALSE))

# 6. COMPARAÇÕES MÚLTIPLAS PAR A PAR (TESTE DE GAMES-HOWELL)
# Utilizado após a ANOVA de Welch para identificar quais pares diferem entre si
cat("\n=== Games-Howell: IMC ===\n")
gh_imc <- dados %>% games_howell_test(IMC ~ Embutidos)
print(gh_imc[, c("group1", "group2", "estimate", "conf.low", "conf.high", "p.adj")])

cat("\n=== Games-Howell: Peso ===\n")
gh_peso <- dados %>% games_howell_test(Peso ~ Embutidos)
print(gh_peso[, c("group1", "group2", "estimate", "conf.low", "conf.high", "p.adj")])

cat("\n=== Games-Howell: Idade ===\n")
gh_idade <- dados %>% games_howell_test(Idade ~ Embutidos)
print(gh_idade[, c("group1", "group2", "estimate", "conf.low", "conf.high", "p.adj")])

# 7. GRÁFICOS BOXPLOT (ILUSTRAÇÃO PARA A APRESENTAÇÃO)
# Boxplot de IMC por Consumo de Embutidos
grafico_imc <- ggplot(dados, aes(x = Embutidos, y = IMC, fill = Embutidos)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.size = 1.5) +
  theme_minimal() +
  labs(
    title = "Distribuição do IMC por Mudança no Consumo de Embutidos",
    subtitle = "ANOVA de Welch: p < 0,001 | Games-Howell: Aumentou > Não consumo",
    x = "Consumo de Embutidos",
    y = "IMC (kg/m²)"
  ) +
  theme(legend.position = "none")

print(grafico_imc)

# Boxplot de Peso por Consumo de Embutidos
grafico_peso <- ggplot(dados, aes(x = Embutidos, y = Peso, fill = Embutidos)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.size = 1.5) +
  theme_minimal() +
  labs(
    title = "Distribuição do Peso por Mudança no Consumo de Embutidos",
    subtitle = "ANOVA de Welch: p < 0,001 | Diferença média de ~9 kg entre grupos",
    x = "Consumo de Embutidos",
    y = "Peso (kg)"
  ) +
  theme(legend.position = "none")

print(grafico_peso)
