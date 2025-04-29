<p align="center">
  <img src="https://github.com/GJFR71/Reg_Log_Campanha_Bankc/blob/main/Capa_Regr_Log_Portfolio.png" width="800"/>
</p>

# 🎯 Previsão de Adesão a Campanhas Bancárias com Regressão Logística

## 💡 Objetivo
Este projeto aplica um pipeline completo de regressão logística para prever a adesão de clientes a campanhas de marketing direto de um banco, com base no conjunto de dados `bank.csv` da UCI Machine Learning Repository. A abordagem inclui análise estatística detalhada, transformação de variáveis, construção e avaliação de modelos preditivos, com ênfase na interpretação prática dos resultados.

## 📊 Técnicas Aplicadas
- Análise exploratória de dados (EDA)  
- Criação de variáveis dummies e categorização  
- Análise de multicolinearidade (VIF)  
- Regressão logística com seleção stepwise  
- Avaliação de desempenho por:
  - Curva ROC e AUC  
  - Lift Chart  
  - Precisão e Recall  
  - Estatística KS

## 📁 Estrutura
- `scripts/`: código R completo com comentários (`Analise_padrao_regressao_Logistica.r`)  
- `data/`: instrução para acesso ao dataset público `bank.csv` (UCI)  
- `outputs/`: gráficos das curvas ROC, Lift, e da variável resposta

## 🛠️ Ferramentas
- **Linguagem:** R  
- **Pacotes principais:** `tidyverse`, `pROC`, `ROCR`, `caret`, `ggplot2`, `psych`, `dplyr`  
- **Editor utilizado:** RStudio

## 📌 Destaques da Análise
- Identificação das variáveis mais influentes na conversão de campanhas  
- Ajuste do ponto de corte com base na estatística KS, otimizando a sensibilidade  
- Interpretação dos coeficientes logísticos com foco em odds ratio  
- Segmentação de grupos de risco com base no escore logístico

## 🔗 Fonte dos Dados
- Dataset original disponível em: [UCI Repository - Bank Marketing Data Set](https://archive.ics.uci.edu/ml/datasets/bank+marketing)

## 📊 Visualizações

### 🎯 Distribuição da Variável Resposta
![Distribuição de Adesões](outputs/distribuicao_resposta_simulada.png)  
*Frequência de clientes que aderiram (1) e não aderiram (0) à campanha.*

### 📈 Curva ROC
![Curva ROC](outputs/curva_ROC_simulada.png)  
*A curva ROC demonstra o desempenho do classificador para diferentes thresholds. AUC ≈ 0.83 (simulado).*

### 📈 Curva de Lift
![Curva de Lift](outputs/lift_curve_simulada.png)  
*O gráfico de Lift ilustra o ganho em relação a um modelo aleatório ao priorizar contatos com maior escore.*

---

👤 **Autor:** [Glaucio Jorge Ferreira Rosa](https://www.linkedin.com/in/glaucio-jferreirarosa)  
🎓 Estatístico | Cientista de Dados

