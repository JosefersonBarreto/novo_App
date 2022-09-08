e_chart() |> 
  e_list(list(
    tooltip = list(trigger = "item"),
    legend = list(top = "5%", left = "center"),
    series = list(
      list(
        name = "Access From",
        type = "pie",
        radius = c("40%", "70%"),
        avoidLabelOverlap = FALSE,
        itemStyle = list(
          borderRadius = 10,
          borderColor = "#fff",
          borderWidth = 2
        ),
        label = list(show = FALSE, position = "center"),
        emphasis = list(
          label = list(
            show = TRUE, 
            fontSize = 40,
            fontWeight = "bold"
          )
        ),
        labelLine = list(show = FALSE),
        data = list(
          list(value = 1048, name = "Search Engine"),
          list(value = 735, name = "Direct"),
          list(value = 580, name = "Email"),
          list(value = 484, name = "Union Ads"),
          list(value = 300, name = "Video Ads")
        )
      )
    )
  )) |> e_theme("dark")






#grafico2 



library(echarts4r)
library(dplyr)

set.seed(42)

df <- data.frame(
  var = sample(LETTERS, 5),
  value = rnorm(5, mean = 200, sd = 100)
)

df |> 
  mutate(bottom = cumsum(dplyr::lag(value, default = 0)),
         bottom = ifelse(value < 0, bottom + value, bottom),
         top = abs(value)) |>
  e_charts(var) |> 
  e_bar(bottom, stack = "var", itemStyle = list(color = "transparent", barBorderColor  = "transparent")) |>
  e_bar(top, stack = "var")





#=============================================================================
#grafico3

#Eu sou o autor do echarts4r, desculpe por ver sua pergunta tão tarde. Você pode usar e_visual_mape configurá-lo para por partes.

df <- tibble::tibble(
  Month = as.character(1:10),
  Revenue = runif(10, -10, 10)
)

library(echarts4r)

df %>% 
  e_charts(Month) %>% 
  e_bar(Revenue) %>% 
  e_visual_map(
    type = "piecewise",
    pieces = list(
      list(
        gt = 0,
        color = "green"
      ),
      list(
        lte = 0,
        color = "red"
      )
    )
  )

# ou 



df %>% 
  dplyr::mutate(cond = ifelse(Revenue > 0, "green", "red")) %>% 
  dplyr::group_by(cond) %>% 
  e_charts(Month) %>% 
  e_bar(Revenue) %>% 
  e_color(color = c("green", "red")) %>% 
  e_legend(FALSE)


#ou 
 df$Revenue<-round(as.numeric(df$Revenue),3)
 
df %>% 
  e_charts(Month) %>%
  e_bar(Revenue,
        itemStyle = list(color = htmlwidgets::JS("
          function(params) {
                                var colorList = ['blue', 'blue', 'pink', 'blue', 'pink', 'blue', 'pink', 'blue', 'blue', 'pink'];
                                return colorList[params.dataIndex]
                                }
                                
    "))) %>% 
  e_legend(FALSE) %>% e_labels(fontSize = 9,position = 'inside' )|> e_theme("dark")


e_label



#==============================================================================

library(geobr)


library(tidyverse)

geobr<-read_municipality(code_muni = "all")
unique(dados$Estado)

dados$abbrev_state<-factor(dados$Estado,levels = c("Amazonas" ,"Ceará","Minas Gerais","Espírito Santo" ,    
                                                   "Pernambuco","São Paulo","Sergipe" ,"Mato Grosso do Sul" ,
                                                   "Rio de Janeiro","Rio Grande do Norte", "Bahia","Rio Grande do Sul",  
                                                   "Distrito Federal","Goiás" ,"Paraná","Paraíba",           
                                                   "Roraima","Rondônia","Santa Catarina" ,"Maranhão",           
                                                   "Mato Grosso" ,"Piauí" ,"Pará" ,"Alagoas",            
                                                   "Tocantins" ,"Amapá","Acre" ),
                           labels = c("AM","CE","MG","ES","PE","SP",
"SE","MS","RJ","RN","BA","RS","DF","GO","PR","PB","RR","RO","SC","MA","MT","PI",
"PA","AL","TO","AM","AC"))

geobr$id<-rep(1:nrow(geobr),1)
dados$id<-rep(1:nrow(dados),1)
dados<-left_join(dados,geobr,by = c("abbrev_state" = "abbrev_state"))
#right_join<-semi_join(dados,geobr,by="abbrev_state")
library(esquisse)
library(leaflet)
glimpse(geobr)
esquisser(novo)

dados %>% leaflet() %>%
  addProviderTiles(dados$Regiao,
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addMarkers(data = points())



 leaflet() %>%
  addTiles(lng=dados$abbrev_state.x, lat=dados$abbrev_state.y, popup="The birthplace of R"
  ) %>%
  addMarkers(data = points())
 
 
 library(leaflet)
 
 m <- leaflet() %>%
   addTiles() %>%  # Add default OpenStreetMap map tiles
   addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
 
 
 
 leaflet() %>%
   addTiles() %>%  # Add default OpenStreetMap map tiles
   addMarkers(lng=dados$geom, lat=dados$geom, popup="The birthplace of R")

 
g<-data.frame(dados$geom) 
library(ggplot2)

ggplot() + # Inicia o gráfico ggplot
  geom_sf(data = dados,
          # Camada do mapa da base completa (Estado SP)
          alpha = .9,
          color = NA) +
  geom_sf(aes(fill = dados$abbrev_state.x))

library(geobr)
states<- read_state(
  year=2019, 
  showProgress = FALSE
)
plot(geob)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Plot all Brazilian states
ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis





# Read data.frame with life expectancy data
df <- utils::read.csv(system.file("extdata/br_states_lifexpect2017.csv", package = "geobr"), encoding = "UTF-8")

states$name_state <- tolower(states$name_state)
df$uf <- tolower(df$uf)

# join the databases
states <- dplyr::left_join(states, df, by = c("name_state" = "uf"))
#Mapa temático do lote
ggplot() +
  geom_sf(data=states, aes(fill=ESPVIDA2017), color= NA, size=.15) +
  labs(subtitle="Life Expectancy at birth, Brazilian States, 2014", size=8) +
  scale_fill_distiller(palette = "Blues", name="Life Expectancy", limits = c(65,80)) +
  theme_minimal() +
  no_axis


ggplot() +
  geom_sf(data=dados, aes(fill=Preco))

          