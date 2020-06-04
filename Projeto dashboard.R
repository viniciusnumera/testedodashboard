setwd("~/Desktop/Projeto dashboard")
library(tidyr)
library(dplyr)
library(plotly)
library(forcats)
library(scales)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dash)
library(dashBootstrapComponents)


base <- read.csv("Completo certo.csv")
base <- base %>%
  filter(Status == "Complete") %>%
  filter(NomeEmpresa == "Aberta") %>%
  filter(!is.na(Qual.o.nome.da.empresa.em.que.você.trabalha.)) %>%
  filter(!grepl("Não|não|autono|Autono|autô|Autô|N/A|NA|Desemp|desemp|Xxx|Autó|autó|Nenhu|nenhu",Qual.o.nome.da.empresa.em.que.você.trabalha.))%>%
  filter(Qual.o.nome.da.empresa.em.que.você.trabalha. != "Na" & Qual.o.nome.da.empresa.em.que.você.trabalha. != "X" & Qual.o.nome.da.empresa.em.que.você.trabalha. != ".")
base$engajamento <- ifelse(base$smean.Engajamento >= 4.5, "Muito engajado", 
                           ifelse(base$smean.Engajamento >= 3.5 & base$smean.Engajamento < 4.5,"Engajado",
                                  ifelse(base$smean.Engajamento >= 2.5 & base$smean.Engajamento < 3.5, "Neutro",
                                         ifelse(base$smean.Engajamento >= 1.5 & base$smean.Engajamento < 2.5, "Desengajado",
                                                ifelse(base$smean.Engajamento < 1.5, "Muito desengajado", "Erro")))))

base$produtividade <- ifelse(base$smean.ProdGeral >= 4.5, "Muito produtivo", 
                           ifelse(base$smean.ProdGeral >= 3.5 & base$smean.ProdGeral < 4.5,"Produtivo",
                                  ifelse(base$smean.ProdGeral >= 2.5 & base$smean.ProdGeral < 3.5, "Neutro",
                                         ifelse(base$smean.ProdGeral >= 1.5 & base$smean.ProdGeral < 2.5, "Improdutivo",
                                                ifelse(base$smean.ProdGeral < 1.5, "Muito improdutivo", "Erro")))))

base$engajamento <- as.factor(as.character(base$engajamento))
base$produtividade <- as.factor(as.character(base$produtividade))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

histogramaengajamento <- plot_ly()

histogramaprodutividade <- plot_ly()

app$layout(
  htmlDiv(
    htmlH1("Impacto das medidas de distanciamento social na percepção de produtividade e egajamento dos colaboradores",
           style = list(textAlign = "center",
                        family = "roboto"))),
  htmlDiv(style = list(textAlign = "center",
                      family = "roboto"),list(
    htmlDiv(list(
      htmlH3("Filtro1"),
      htmlLabel("Gênero"),
              dccChecklist(id = "filtrogenero1",
                           options = list(list(label = "Masculino", value = "Masculino"),
                                          list(label = "Feminino", value = "Feminino")
                           ), value = list("Masculino", "Feminino")),
      htmlDiv(list(
        htmlDiv(id = 'Faixaetaria1'),
      dccRangeSlider(id = "filtroidade1",
                     min = 18, max = 65, step = 1 , value = c(18,65), tooltip = "always_visible"
                     ))),
      htmlLabel('Região'),
      dccDropdown(id = "filtroregiao1",
                   options = list(list(label = "Norte", value = "NORTE"),
                                  list(label = "Nordeste", value = "NORDESTE"),
                                  list(label = "Centro-Oeste", value = "CENTRO-OESTE"),
                                  list(label = "Sudeste", value = "SUDESTE"),
                                  list(label = "Sul", value = "SUL")),
                   multi = TRUE,
                   clearable = TRUE,
                   value = list("SUL","SUDESTE","NORTE","NORDESTE","CENTRO-OESTE")),
      htmlH3("Filtro2"),
      htmlLabel("Gênero"),
      dccChecklist(id = "filtrogenero2",
                   options = list(list(label = "Masculino", value = "Masculino"),
                                  list(label = "Feminino", value = "Feminino")
                   ), value = list("Masculino", "Feminino")),
      htmlDiv(list(
        htmlDiv(id = 'Faixaetaria2'),
      dccRangeSlider(id = "filtroidade2",
                     min = 18, max = 65, step = 1, value = c(18,65),tooltip = "always_visible"
                     ))),
      htmlLabel('Região'),
      dccDropdown(id = "filtroregiao2",
                   options = list(list(label = "Norte", value = "NORTE"),
                                  list(label = "Nordeste", value = "NORDESTE"),
                                  list(label = "Centro-Oeste", value = "CENTRO-OESTE"),
                                  list(label = "Sudeste", value = "SUDESTE"),
                                  list(label = "Sul", value = "SUL")),
                  multi = TRUE,
                  clearable = TRUE,
                   value = list("SUL","SUDESTE","NORTE","NORDESTE","CENTRO-OESTE"))),
      className = "col-2"),
      htmlDiv(dccGraph(
      id = "graficoengajamento",
      figure = histogramaengajamento
    ), className = "col-5"),
    htmlDiv(dccGraph(
      id = "graficoprodutividade",
      figure = histogramaprodutividade
    ), className = "col-5")
    ), className = "row")
)

app$callback(
  output = list(id= "graficoengajamento", property='figure'),
  params = list(input(id='filtrogenero1', property='value'),
                input(id='filtrogenero2', property='value'),
                input(id='filtroregiao1', property='value'),
                input(id='filtroregiao2', property='value'),
                input(id='filtroidade1', property='value'),
                input(id='filtroidade2', property='value')),
  
  function(filtro1genero,filtro2genero,filtro1regiao,filtro2regiao,filtro1idade,filtro2idade) {
    
    valordofiltro1genero  <- c()
    valordofiltro1genero <- filtro1genero
    valordofiltro2genero <- c()
    valordofiltro2genero <- filtro2genero
    valordofiltro1regiao  <- c()
    valordofiltro1regiao <- filtro1regiao
    valordofiltro2regiao <- c()
    valordofiltro2regiao <- filtro2regiao
filtro1idade <- as.numeric(filtro1idade)
filtro2idade <- as.numeric(filtro2idade)
    basefiltro1 <- base 
    basefiltro2 <- base
 
if (length(valordofiltro1genero > 0)) {
   basefiltro1 <- base %>%
   filter(Com.qual.gênero.você.se.identifica. %in% valordofiltro1genero)} 

if (length(valordofiltro1regiao > 0)) {
    basefiltro1 <- basefiltro1  %>%
    filter(Regiao %in% valordofiltro1regiao)}
    
if (length(valordofiltro2genero > 0)) {
   basefiltro2 <- base %>%
   filter(Com.qual.gênero.você.se.identifica. %in% valordofiltro2genero)} 
    
if (length(valordofiltro2regiao > 0)) {
   basefiltro2 <- basefiltro2  %>%
   filter(Regiao %in% valordofiltro2regiao)}
   
      basefiltro1engajamento <- basefiltro1 %>%
        filter(!is.na(engajamento)) %>%
        filter(between(Qual.a.sua.idade.,min(filtro1idade),max(filtro1idade))) %>%
        count(engajamento, .drop = FALSE) %>%
        mutate(porcentagem = n/sum(n),
               engajamento = fct_reorder(engajamento,porcentagem,.desc = F))
      
      basefiltro2engajamento <- basefiltro2 %>%
        filter(!is.na(engajamento)) %>%
        filter(between(Qual.a.sua.idade.,min(filtro2idade),max(filtro2idade))) %>%
        count(engajamento, .drop = FALSE) %>%
        mutate(porcentagem = n/sum(n),
               engajamento = fct_reorder(engajamento,porcentagem,.desc = F))
     
    histogramaengajamento <- histogramaengajamento %>%
      add_bars(data = basefiltro1engajamento, hoverinfo = "text", hovertext = ~paste(engajamento, ": ", percent(basefiltro1engajamento$porcentagem, accuracy = 0.1)), y= ~engajamento, x= ~porcentagem, color = I("#5288DB"), name = "FILTRO1") %>%
      add_annotations(yref = "y",xref = "x",
                      x = 0.5, y = 0.3,
                      text = ~paste("Filtro 1: n=",sum(basefiltro1engajamento$n)),
                      font = list(family = 'Roboto', size = 14,
                                  color = "#4d4d4d"),
                      showarrow = FALSE)
    
    histogramaengajamento <- histogramaengajamento %>%
      add_bars(data = basefiltro2engajamento, hoverinfo = "text", hovertext = ~paste(engajamento, ": ", percent(basefiltro2engajamento$porcentagem, accuracy = 0.1)), y= ~engajamento, x= ~porcentagem, color = I("#4D4D4D"), name = "FILTRO2") %>%
      add_annotations(yref = "y",xref = "x",
                      x = 0.5, y = 0.1,
                      text = ~paste("Filtro 2: n=",sum(basefiltro2engajamento$n)),
                    font = list(family = 'Roboto', size = 14,
                                color = "#4d4d4d"),
                    showarrow = FALSE)
      
    histogramaengajamento <- histogramaengajamento %>%
      layout(barmode = "identity",
             yaxis = list(showgrid = FALSE, title = ""),
             xaxis = list(showgrid = FALSE, title = "", tickformat= "%"))
  }
)

app$callback(
  output = list(id= "graficoprodutividade", property='figure'),
  params = list(input(id='filtrogenero1', property='value'),
                input(id='filtrogenero2', property='value'),
                input(id='filtroregiao1', property='value'),
                input(id='filtroregiao2', property='value'),
                input(id='filtroidade1', property='value'),
                input(id='filtroidade2', property='value')),
  
  function(filtro1genero,filtro2genero,filtro1regiao,filtro2regiao,filtro1idade,filtro2idade) {
    
    valordofiltro1genero  <- c()
    valordofiltro1genero <- filtro1genero
    valordofiltro2genero <- c()
    valordofiltro2genero <- filtro2genero
    valordofiltro1regiao  <- c()
    valordofiltro1regiao <- filtro1regiao
    valordofiltro2regiao <- c()
    valordofiltro2regiao <- filtro2regiao
    filtro1idade <- as.numeric(filtro1idade)
    filtro2idade <- as.numeric(filtro2idade)
    basefiltro1 <- base 
    basefiltro2 <- base
    
    if (length(valordofiltro1genero > 0)) {
      basefiltro1 <- base %>%
        filter(Com.qual.gênero.você.se.identifica. %in% valordofiltro1genero)} 
    
    if (length(valordofiltro1regiao > 0)) {
      basefiltro1 <- basefiltro1  %>%
        filter(Regiao %in% valordofiltro1regiao)}
    
    if (length(valordofiltro2genero > 0)) {
      basefiltro2 <- base %>%
        filter(Com.qual.gênero.você.se.identifica. %in% valordofiltro2genero)} 
    
    if (length(valordofiltro2regiao > 0)) {
      basefiltro2 <- basefiltro2  %>%
        filter(Regiao %in% valordofiltro2regiao)}
    
    basefiltro1produtividade <- basefiltro1 %>%
      filter(!is.na(produtividade)) %>%
      filter(between(Qual.a.sua.idade.,min(filtro1idade),max(filtro1idade))) %>%
      count(produtividade, .drop = FALSE) %>%
      mutate(porcentagem = n/sum(n),
             produtividade = fct_reorder(produtividade,porcentagem,.desc = F))
    
    basefiltro2produtividade <- basefiltro2 %>%
      filter(!is.na(produtividade)) %>%
      filter(between(Qual.a.sua.idade.,min(filtro2idade),max(filtro2idade))) %>%
      count(produtividade, .drop = FALSE) %>%
      mutate(porcentagem = n/sum(n),
             produtividade = fct_reorder(produtividade,porcentagem,.desc = F))
    
    histogramaprodutividade <- histogramaprodutividade %>%
      add_bars(data = basefiltro1produtividade, hoverinfo = "text", hovertext = ~paste(produtividade, ": ", percent(basefiltro1produtividade$porcentagem, accuracy = 0.1)), 
               y= ~produtividade, x= ~porcentagem, color = I("#5288DB"), name = "FILTRO1") %>%
      add_annotations(yref = "y",xref = "x",
                      x = 0.3, y = 0.3,
                      text = ~paste("Filtro 1: n=",sum(basefiltro1produtividade$n)),
                      font = list(family = 'Roboto', size = 14,
                                  color = "#4d4d4d"),
                      showarrow = FALSE)
    
    histogramaprodutividade <- histogramaprodutividade %>%
      add_bars(data = basefiltro2produtividade, hoverinfo = "text", hovertext = ~paste(produtividade, ": ", percent(basefiltro2produtividade$porcentagem, accuracy = 0.1)), 
               y= ~produtividade, x= ~porcentagem, color = I("#4D4D4D"), name = "FILTRO2") %>%
      add_annotations(yref = "y",xref = "x",
                      x = 0.3, y = 0.1,
                      text = ~paste("Filtro 2: n=",sum(basefiltro2produtividade$n)),
                      font = list(family = 'Roboto', size = 14,
                                  color = "#4d4d4d"),
                      showarrow = FALSE)
    
    histogramaprodutividade <- histogramaprodutividade %>%
      layout(barmode = "identity",
             yaxis = list(showgrid = FALSE, title = ""),
             xaxis = list(showgrid = FALSE, title = "", tickformat= "%"))
  }
  )

app$callback(
  output(id = 'Faixaetaria1', property='children'),
  params=list(input(id='filtroidade1', property='value')),
  function(value1) {
    sprintf(paste('Idade entre', value1[1],"e", value1[2], "anos"))
  }
  )

app$callback(
  output(id = 'Faixaetaria2', property='children'),
  params=list(input(id='filtroidade2', property='value')),
  function(value2) {
    sprintf(paste('Idade entre', value2[1],"e", value2[2], "anos"))
  }
  )

app$run_server()


