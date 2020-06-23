### GRÁFICOS Tendencias acumuladas 

library(rio)
library(gridExtra)
library(kableExtra)
library(JLutils) # devtools::install_github("larmarange/JLutils")
library(tidyverse)
library(cowplot)
library(sf)
library(ggrepel)
library(readxl)
library(cowplot)
library(ggpubr)
library(directlabels)
library(ggiraph)
library(lubridate)
data=import("reportes_minsa.xlsx")


### Grafico 1

f1=data %>%
  ggplot(aes(x = Dia)) +
  #ylim(0,max(data$Positivos)*2.1)+
  geom_bar_interactive(aes(y = PCR_diario_positivo,
                           tooltip = PCR_diario_positivo,fill = "Nuevos"), 
                       stat = "identity", alpha=.5)+
  #geom_label(aes(Dia, Nuevos_Positivos, label = Nuevos_Positivos), vjust = 0.5,label.size = 0.10)+
  geom_line(aes(y = PCR_positivos, col = "Acumulados"), size=1) +
  geom_point_interactive(aes(y = PCR_positivos,tooltip = PCR), col = "#8B1C62")+
  scale_y_continuous(trans = "log2")+
  labs(y = "Número de casos reportados (log2)", color = " Casos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"),
                        "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU")) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  ggtitle("Número de casos positivos de pacientes COVID-19\n Perú (solo moleculares)")+
  theme_minimal() +
  theme(legend.position="bottom")


f1_1=data %>%
  mutate(Positivos=PCR_positivos+replace_na(PR_positivos,0),
         Nuevos_Positivos=PCR_diario_positivo+replace_na(PR_diario_positivos,0))%>%
  ggplot(aes(x = Dia)) +
  geom_bar_interactive(aes(y = PCR_diario_positivo,
                           tooltip = PCR_diario_positivo,fill = "Nuevos"), 
                       stat = "identity", alpha=.5)+
  #geom_label(aes(Dia, Nuevos_Positivos, label = Nuevos_Positivos), vjust = 0.5,label.size = 0.10)+
  geom_line(aes(y = PCR_positivos, col = "Acumulados"), size=1) +
  geom_point_interactive(aes(y = PCR_positivos,tooltip = PCR), col = "#8B1C62")+
  scale_y_continuous(trans = "log2")+
  labs(y = "Número de casos reportados (log2)", color = " Casos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"),
                        "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU")) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  ggtitle("Número de casos positivos de pacientes COVID-19\n Perú (solo moleculares)")+
  theme_minimal() +
  theme(legend.position="bottom")

fallecidos=data %>%
  mutate(Nuevos_Fallecidos=Fallecidos-lag(Fallecidos,1))%>%
  dplyr::select(Dia,Fallecidos,Nuevos_Fallecidos)%>%
  na.omit()%>%
  ggplot(aes(x = Dia)) +
  #ylim(0,max(data$Fallecidos)*1.1)+
  geom_bar_interactive(aes(y = Nuevos_Fallecidos,
                           tooltip = Nuevos_Fallecidos,fill = "Nuevos"), stat = "identity", alpha=.5)+
  #geom_label(aes(Dia, Nuevos_Positivos, label = Nuevos_Positivos), vjust = 0.5,label.size = 0.10)+
  geom_line(aes(y = Fallecidos, col = "Acumulados"), size=1) +
  geom_point_interactive(aes(y = Fallecidos,tooltip = Fallecidos), col = "#8B1C62")+
  scale_y_continuous(trans = "log10")+
  labs(y = "Número de Fallecidos", color = " Fallecidos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"),
                        "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU")) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  ggtitle("Número de fallecidos COVID-19\n Perú (log10)")+
  theme_minimal() +
  theme(legend.position="bottom")


f2=data %>%
  dplyr::select(Dia,PCR_positivos, PCR_descartados)%>%
  gather(type,value,-Dia)%>%
  group_by(Dia)%>%
  mutate(perc=round(value/sum(value)*100,2))%>%
  ggplot(aes(x=Dia,y=value)) +
  geom_bar_interactive(aes(tooltip=perc, fill=type),position="fill", stat="identity")+
  labs(y = "proportion", fill = "Resultado",
       #title = paste0("Proporción de positivos del total \nde muestras analizadas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_y_continuous(labels = scales::percent_format())+
    theme_minimal() +
    theme(legend.position="bottom")


f2_1=data %>%
  mutate(Positivos=replace_na(PR_diario_positivos,0)+PCR_diario_positivo,
         Descartados=replace_na(PR_diario,0)-replace_na(PR_diario_positivos,0)+
           PCR_descartados)%>%
  dplyr::select(Dia,Positivos,Descartados)%>%
  gather(type,value,-Dia)%>%
  group_by(Dia)%>%
  mutate(perc=round(value/sum(value)*100,2))%>%
  ggplot(aes(x=Dia,y=value)) +
  geom_bar_interactive(aes(tooltip=perc, fill=type),position="fill", stat="identity")+
  labs(y = "proportion", fill = "Resultado",
       #title = paste0("Proporción de positivos del total \nde muestras analizadas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal() +
  theme(legend.position="bottom")

f3=data%>%
  mutate(neg_new=PCR_diario-PCR_diario_positivo)%>%
  dplyr::select(Dia, Descartados=neg_new, 
                Positivos=PCR_diario_positivo)%>%
  gather(type,value,-Dia)%>%
  group_by(Dia)%>%
  mutate(perc=round(value/sum(value)*100,2))%>%
  ggplot(aes(x=Dia,y=value)) +
  geom_bar_interactive(aes(tooltip=perc, fill=type),position="fill", stat="identity")+
  labs(y = "Porcentaje", fill = "Resultado",
       #title = paste0("Proporción de positivos del total \nde muestras analizadas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_y_continuous(labels = scales::percent_format())+
  ggtitle("Proporción de casos positivos COVID-19 x dia, Perú")
  theme_minimal() +
  theme(legend.position="bottom")


f3_1=data%>%
  mutate(neg_new=PCR_diario-PCR_diario_positivo+
           replace_na(PR_diario,0)+replace_na(PR_diario_positivos,0),
         Nuevos_positivos=PCR_diario_positivo+replace_na(PR_diario_positivos,0))%>%
  dplyr::select(Dia, Descartados=neg_new, 
                Positivos=Nuevos_positivos)%>%
  gather(type,value,-Dia)%>%
  group_by(Dia)%>%
  mutate(perc=round(value/sum(value)*100,2))%>%
  ggplot(aes(x=Dia,y=value)) +
  geom_bar_interactive(aes(tooltip=perc, fill=type),position="fill", stat="identity")+
  labs(y = "proportion", fill = "Resultado",
       #title = paste0("Proporción de positivos del total \nde muestras analizadas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal() +
  theme(legend.position="bottom")

f5=data%>%#mutate(Pruebas_diaPR=replace_na(Pruebas_diaPR,0))%>%
  select(Dia,
         Moleculares=PCR_diario,
         Pruebas_Rap=PR_diario)%>%
  gather(tipo,numero, -Dia)%>%
  ggplot(aes(fill=tipo, y=numero, x=Dia,tooltip=numero)) + 
  geom_bar_interactive(stat="identity")+
  labs(y = "Número de pruebas analizadas", color = " Casos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"),
                        "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU"))+
  ggtitle("Número de pruebas analizadas (moleculares y rápidas), Perú")+
  theme(legend.position="bottom")



f6=data%>%
  slice(21:n())%>%
  ggplot(aes(x=Dia))+
  #ylim(0,1400)+
  geom_line(aes(y=Hospitalizados_UCI))+
  geom_point(aes(y = Hospitalizados_UCI), col = "#8B1C62")+
  geom_text_repel(
    aes(x=Dia,y=Hospitalizados_UCI,label = Hospitalizados_UCI), 
    vjust = -0.3,
    size = 3.1,
    box.padding = 0.05)+
  labs(y = "Hospitalizados camas UCI", color = " Casos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m")))+
  ggtitle("Hospitalizados en camas UCI")+
  geom_hline(yintercept=276, linetype="dashed", 
             color = "red", size=1)+
  # geom_hline(yintercept=525, linetype="dashed", 
  #            color = "orange", size=1)+
  # geom_hline(yintercept=880, linetype="dashed", 
  #            color = "blue", size=1)+
  geom_hline(yintercept=1241, linetype="dashed", 
             color = "blue", size=1)+
  annotate(geom="text", 
           label="Capacidad inicial", x=max(data$Dia), 
           y=276, vjust=-1,hjust = 1)+
  # annotate(geom="text", 
  #          label="Capacidad al 20/04", x=max(data$Dia), 
  #          y=525, vjust=-1,hjust = 2)  +
  # annotate(geom="text", 
  #          label="Capacidad al 05/05: 880", x=max(data$Dia), 
  #          y=880, vjust=-1,hjust = 2) +
  annotate(geom="text", 
           label="Capacidad actual: 1241", x=max(data$Dia), 
           y=1241, vjust=-1,hjust = 2) +
  theme_minimal() +
  theme(legend.position="bottom")
### Nuevo gráfico



f1_1=data %>%
  mutate(Positivos=PCR_positivos+replace_na(PR_positivos,0),
         Nuevos_Positivos=PCR_diario_positivo+replace_na(PR_diario_positivos,0),
         diferencia=Nuevos_Positivos-lag(Nuevos_Positivos,1,0),
         semana=rep(1:30,length.out=104,each=7)
         )%>%
  group_by(semana)%>%
  summarise(promedio=mean(Nuevos_Positivos))%>%
  ggplot(aes(x = semana)) +
  geom_bar_interactive(aes(y = promedio,
                           tooltip = promedio,fill = "Nuevos"), 
                       stat = "identity", alpha=.5)+
    geom_line(aes(y = PCR_positivos, col = "Acumulados"), size=1) +
  geom_point_interactive(aes(y = PCR_positivos,tooltip = PCR), col = "#8B1C62")+
  scale_y_continuous(trans = "log2")+
  labs(y = "Número de casos reportados (log2)", color = " Casos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"),
                        "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU")) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  ggtitle("Número de casos positivos de pacientes COVID-19\n Perú (solo moleculares)")+
  theme_minimal() +
  theme(legend.position="bottom")


### Descartados 

f4=data %>%
  ggplot(aes(x = Dia)) +
  ylim(0,max(data$Total_Pruebas)*1.1)+
  geom_bar_interactive(aes(y = Pruebas_dia,tooltip =Pruebas_dia, fill = "Nuevos"), stat = "identity", alpha=.5)+
  geom_line(aes(y = Total_Pruebas, col = "Acumulados"), size=1) +
  geom_point_interactive(aes(y = Total_Pruebas,tooltip=Total_Pruebas), col = "#8B1C62")+
  scale_y_continuous(trans = "log10")+
  labs(y = "Número de pruebas analizadas (log10)", color = " Casos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"),
                        "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU")) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  ggtitle("Número de pruebas moleculares analizadas COVID-19 \n (acumulados y nuevos), Perú")+
  theme_minimal() +
  theme(legend.position="bottom")

f4_1=data %>%
  mutate(Total_Pruebas=Total_Pruebas+replace_na(PruebasRapidas,0),
         Pruebas_dias=Pruebas_dia+replace_na(Pruebas_diaPR,0))%>%
  dplyr::select(Dia,Pruebas_dias,Total_Pruebas)%>%
  ggplot(aes(x = Dia)) +
  ylim(0,max(data$Total_Pruebas+data$PruebasRapidas,na.rm=TRUE)*1.1)+
  geom_bar_interactive(aes(y = Pruebas_dias,tooltip =Pruebas_dias, fill = "Nuevos"), stat = "identity", alpha=.5)+
  geom_line(aes(y = Total_Pruebas, col = "Acumulados"), size=1) +
  geom_point_interactive(aes(y = Total_Pruebas,tooltip =Total_Pruebas), col = "#8B1C62")+
  scale_y_continuous(trans = "log10")+
  labs(y = "Número de pruebas analizadas (log10)", color = " Casos", fill = " ", 
       #title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"),
                        "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU")) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  ggtitle("Número de pruebas moleculares y rápidas \n analizadas COVID-19 (acumulados y nuevos), Perú")+
  theme_minimal() +
  theme(legend.position="bottom")

# f5=data%>%#mutate(Pruebas_diaPR=replace_na(Pruebas_diaPR,0))%>%
#   select(Dia,
#          Moleculares=Pruebas_dia,
#          Pruebas_Rap=Pruebas_diaPR)%>%
#   gather(tipo,numero, -Dia)%>%
#   ggplot(aes(fill=tipo, y=numero, x=Dia)) + 
#   geom_bar(position="dodge", stat="identity")+
#   geom_text(aes(label=numero), position=position_dodge(width=0.9),
#             angle = 90,size = 3)+
#   theme_minimal() +
#   theme(legend.position="bottom")


Para los gráficos he adaptado del post de Gabriel Carrasco Escobar (http://gcarrasco.rbind.io/blog/covid19_viz/)


```{r, eval=FALSE}
#Gráfico de líneas antiguo
p1=data%>%
  dplyr::group_by(Dia)%>%
  dplyr::summarise(Positivos=max(Positivos))%>%
  ggplot(aes(x=Dia,y=Positivos, label=Positivos))+
  ggtitle("Pacientes positivos (acumulados)")+
  geom_line()+geom_label()
p2=data%>%
  dplyr::group_by(Dia)%>%
  dplyr::summarise(Positivos=max(Positivos))%>%
  mutate(lag=lag(Positivos),
         Nuevos=Positivos-lag)%>%
  ggplot(aes(x=Dia,y=Nuevos, label=Nuevos))+
  ggtitle("Pacientes nuevos por dia")+
  geom_line()+geom_label()
grid.arrange(p1, p2, ncol = 1)
```

