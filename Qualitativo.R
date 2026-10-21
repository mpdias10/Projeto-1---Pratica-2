require(openxlsx)            # Leitura de base de dados Excel
require(dplyr)               # Manipulação de dados
require(gtsummary)           # Tabelas bivariadas automáticas
require(gt)                  # Formatação de tabelas
require(rstatix)             # Coeficiente V de Cramér e Games-Howell
require(DescTools)           # Teste de Levene
require(ggplot2)             # Visualização gráfica
require(qqplotr)             # Diagnóstico de normalidade (Q-Q plot)


# 2. LEITURA DA BASE DE DADOS E FORMATAÇÃO

dados <- read.xlsx("Nutrição.xlsx")


theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")


# 3. ANÁLISE DESCRITIVA DA VARIÁVEL RESPOSTA (EMBUTIDOS)

table(dados$Embutidos)
prop.table(table(dados$Embutidos)) * 100


# 4. PREPARAÇÃO E TRATAMENTO METODOLÓGICO DOS DADOS QUALITATIVOS

DadosQuali <- dados %>% 
  select(
    Genero, Raca_cor, Regiao, Isolamento, Trabalha, Escolaridade, Renda_familiar,
    Covid, Consulta_Nutricionista, Dificuldade_Financeira, cigarro, Ansiedade,
    Depressao, Insonia, Enxaqueca, Embutidos
  )



# 5. DIAGNÓSTICO DE FREQUÊNCIAS ESPERADAS DO QUI-QUADRADO ($expected)

chisq.test(DadosQuali_ajustado$Genero, DadosQuali_ajustado$Embutidos)$expected
chisq.test(DadosQuali_ajustado$Raca_cor, DadosQuali_ajustado$Embutidos)$expected
chisq.test(DadosQuali_ajustado$Regiao, DadosQuali_ajustado$Embutidos)$expected
chisq.test(DadosQuali_ajustado$Escolaridade, DadosQuali_ajustado$Embutidos)$expected
chisq.test(DadosQuali_ajustado$Dificuldade_Financeira, DadosQuali_ajustado$Embutidos)$expected
chisq.test(DadosQuali_ajustado$Consulta_Nutricionista, DadosQuali_ajustado$Embutidos)$expected
chisq.test(DadosQuali_ajustado$Ansiedade, DadosQuali_ajustado$Embutidos)$expected


# 6. FUNÇÃO PARA O COEFICIENTE V DE CRAMÉR

cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- rstatix::cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 3))
}


# 7. TABELA QUALITATIVA COMPLETA (GTSUMMARY)

tabela_quali_completa <- tbl_summary(
  data = DadosQuali_ajustado,
  by = Embutidos,
  percent = "row",
  missing = "no",
  label = list(
    Genero                 ~ "Gênero<sup>Q</sup>",
    Raca_cor               ~ "Raça/Cor<sup>Q</sup>",
    Regiao                 ~ "Região<sup>Q</sup>",
    Trabalha               ~ "Situação de Trabalho<sup>Q</sup>",
    Escolaridade           ~ "Nível de Escolaridade<sup>Q</sup>",
    Renda_familiar         ~ "Faixa de Renda Familiar<sup>Q</sup>",
    Isolamento             ~ "Isolamento Social<sup>Q</sup>",
    Covid                  ~ "Diagnóstico de COVID-19<sup>Q</sup>",
    Dificuldade_Financeira ~ "Dificuldade Financeira<sup>Q</sup>",
    Consulta_Nutricionista ~ "Consulta com Nutricionista<sup>Q</sup>",
    cigarro                ~ "Hábito de Fumar<sup>Q</sup>",
    Ansiedade              ~ "Relato de Ansiedade<sup>Q</sup>",
    Depressao              ~ "Relato de Depressão<sup>Q</sup>",
    Insonia                ~ "Relato de Insônia<sup>Q</sup>",
    Enxaqueca              ~ "Relato de Enxaqueca<sup>Q</sup>"
  )
) %>%
  add_p(
    test = all_categorical() ~ "chisq.test",
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun) %>%
  modify_spanning_header(all_stat_cols() ~ "**Mudança no Consumo de Embutidos**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)") %>%
  modify_footnote(everything() ~ NA)


tabela_quali_completa


# 8. ANÁLISE DE RESÍDUOS PADRONIZADOS AJUSTADOS (CASELAS INFLUENTES |z| > 1.96)

# 1. Dificuldade Financeira
tab_fin <- table(DadosQuali_ajustado$Dificuldade_Financeira, DadosQuali_ajustado$Embutidos)
chisq.test(tab_fin)$stdres

# 2. Consulta Nutricionista
tab_nutri <- table(DadosQuali_ajustado$Consulta_Nutricionista, DadosQuali_ajustado$Embutidos)
chisq.test(tab_nutri)$stdres

# 3. Gênero
tab_gen <- table(DadosQuali_ajustado$Genero, DadosQuali_ajustado$Embutidos)
chisq.test(tab_gen)$stdres

# 4. Ansiedade
tab_ans <- table(DadosQuali_ajustado$Ansiedade, DadosQuali_ajustado$Embutidos)
chisq.test(tab_ans)$stdres


# 9. ANÁLISE DAS VARIÁVEIS QUANTITATIVAS

# Tabela 2: Medidas Resumo (Média e Desvio-Padrão) e ANOVA de Welch
tabela_quanti <- dados %>%
  select(Embutidos, Idade, Peso, Altura, IMC) %>%
  tbl_summary(
    by = Embutidos,
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_p(test = all_continuous() ~ "oneway.test") %>% # ANOVA de Welch
  bold_p(t = 0.05) %>%
  modify_spanning_header(all_stat_cols() ~ "**Mudança no Consumo de Embutidos**") %>%
  modify_header(label ~ "**Variáveis Quantitativas**") %>%
  bold_labels()


tabela_quanti


# 10. TESTES DE HOMOGENEIDADE DE VARIÂNCIAS (LEVENE)

LeveneTest(Idade ~ as.factor(Embutidos), data = dados)
LeveneTest(Peso ~ as.factor(Embutidos), data = dados)
LeveneTest(IMC ~ as.factor(Embutidos), data = dados)


# 11. ANOVA DE WELCH (SOB HETEROCEDASTICIDADE)

oneway.test(Idade ~ Embutidos, data = dados, var.equal = FALSE)
oneway.test(Peso ~ Embutidos, data = dados, var.equal = FALSE)
oneway.test(IMC ~ Embutidos, data = dados, var.equal = FALSE)


# 12. COMPARAÇÕES MÚLTIPLAS DE GAMES-HOWELL

gh_imc <- dados %>% games_howell_test(IMC ~ Embutidos)
print(gh_imc)


gh_peso <- dados %>% games_howell_test(Peso ~ Embutidos)
print(gh_peso)


gh_idade <- dados %>% games_howell_test(Idade ~ Embutidos)
print(gh_idade)


# 13. GRÁFICO ILUSTRATIVO (BOXPLOT DE IMC POR CONSUMO DE EMBUTIDOS)

grafico_imc <- ggplot(dados, aes(x = Embutidos, y = IMC, fill = Embutidos)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  theme_minimal() +
  labs(
    title = "Distribuição do IMC segundo a Mudança no Consumo de Embutidos",
    x = "Consumo de Embutidos",
    y = "IMC (kg/m²)"
  ) +
  theme(legend.position = "none")

print(grafico_imc)
