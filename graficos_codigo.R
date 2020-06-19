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
  labs(y = "proportion", fill = "Resultado",
       #title = paste0("Proporción de positivos del total \nde muestras analizadas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_y_continuous(labels = scales::percent_format())+
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


# Informes de Minsa

Los reportes se construyen a partir de las notas de prensa publicadas aquí:
  https://www.gob.pe/busquedas?contenido[]=noticias&institucion[]=minsa&reason=sheet&sheet=1
y de los reportes de la sala situacional. 

04/13

https://www.gob.pe/institucion/minsa/noticias/112315-minsa-casos-confirmados-por-coronavirus-covid-19-ascienden-a-9-784-en-el-peru-comunicado-n-64

03/29

https://www.gob.pe/institucion/minsa/noticias/111590-minsa-casos-confirmados-por-coronavirus-covid-19-ascienden-a-852-en-el-peru-comunicado-n-40

03/28 

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">📄Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 00:00 horas del 28 de marzo. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/kxI5j275r3">pic.twitter.com/kxI5j275r3</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1243956611653214209?ref_src=twsrc%5Etfw">March 28, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/25

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 00:00 horas del 25 de marzo. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/9wE4kaIYZ5">pic.twitter.com/9wE4kaIYZ5</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1242872053709115395?ref_src=twsrc%5Etfw">March 25, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/24

Dos fallecidos 

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🔴COMUNICADO | El <a href="https://twitter.com/hashtag/Minsa?src=hash&amp;ref_src=twsrc%5Etfw">#Minsa</a> lamenta informar el sensible fallecimiento de dos personas por <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en territorio peruano. Se trata de un varón de 38 años y una mujer de 66 años. Extendemos nuestras condolencias a sus familias. <a href="https://t.co/xrsq52SpwW">pic.twitter.com/xrsq52SpwW</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1242488321869262848?ref_src=twsrc%5Etfw">March 24, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/23

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 1:00 horas del 23 de marzo. Se registran 36 ciudadanos hospitalizados. Esta actualización corrige información publicada. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/zYNDZNOSrL">pic.twitter.com/zYNDZNOSrL</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1242146715588853761?ref_src=twsrc%5Etfw">March 23, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/22 

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 2:30 horas del 22 de marzo. Se registran 31 ciudadanos hospitalizados. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/GWRxaHvY2B">pic.twitter.com/GWRxaHvY2B</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1241831353684983808?ref_src=twsrc%5Etfw">March 22, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/21

Casos:
  
  https://www.gob.pe/8662

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🔴COMUNICADO | El <a href="https://twitter.com/hashtag/Minsa?src=hash&amp;ref_src=twsrc%5Etfw">#Minsa</a> lamenta informar que hoy, sábado 21 de marzo, a las 07:20 horas, se registró el sensible fallecimiento de una persona con <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Piura. El ministerio extiende sus condolencias a la familia. <a href="https://t.co/ZwMefFiTsq">pic.twitter.com/ZwMefFiTsq</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1241395322632503300?ref_src=twsrc%5Etfw">March 21, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  
  03/20

### Casos

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 12:00 horas del 20 de marzo. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/Kg7Gr2QRRL">pic.twitter.com/Kg7Gr2QRRL</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1241080637622292482?ref_src=twsrc%5Etfw">March 20, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  <blockquote class="twitter-tweet"><p lang="es" dir="ltr">🔴COMUNICADO | El <a href="https://twitter.com/hashtag/Minsa?src=hash&amp;ref_src=twsrc%5Etfw">#Minsa</a> informa que, a las 9:17 horas de hoy, viernes 20 de marzo, se registró la muerte de una persona con <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en territorio peruano. Expresamos nuestras más sentidas condolencias a sus familiares. <a href="https://t.co/B8MEz7alXR">pic.twitter.com/B8MEz7alXR</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1241048470955458560?ref_src=twsrc%5Etfw">March 20, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  03/19 

Casos: 
  <blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 12:00 horas del 19 de marzo. Se registran dos altas. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrN5dc">https://t.co/F4GaDrN5dc</a> <a href="https://t.co/zYppUus7Ip">pic.twitter.com/zYppUus7Ip</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1240706791366733824?ref_src=twsrc%5Etfw">March 19, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  Fallecidos: 
  
  Segundo y tercero
<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🔴 URGENTE | El <a href="https://twitter.com/hashtag/Minsa?src=hash&amp;ref_src=twsrc%5Etfw">#Minsa</a> lamenta informar la sensible muerte de dos ciudadanos por coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en territorio peruano. Extendemos nuestras condolencias a sus familias. <a href="https://t.co/UF5X1ukqtR">pic.twitter.com/UF5X1ukqtR</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1240818095280869377?ref_src=twsrc%5Etfw">March 20, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  Primero 

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🔴COMUNICADO | El <a href="https://twitter.com/hashtag/Minsa?src=hash&amp;ref_src=twsrc%5Etfw">#Minsa</a> lamenta informar que, a las 15:00 horas de hoy, jueves 19 de marzo, se registró el primer fallecimiento por <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a>. <a href="https://t.co/sgC0nhk87b">pic.twitter.com/sgC0nhk87b</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1240761528007221249?ref_src=twsrc%5Etfw">March 19, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  03/18

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 7:00 horas del 18 de marzo. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/pPPwxu13dG">pic.twitter.com/pPPwxu13dG</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1240305962256343040?ref_src=twsrc%5Etfw">March 18, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/17 

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 8:20 horas del 17 de marzo. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/8yojgp7eYw">pic.twitter.com/8yojgp7eYw</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1239982787404455940?ref_src=twsrc%5Etfw">March 17, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/16 

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus <a href="https://twitter.com/hashtag/COVID19?src=hash&amp;ref_src=twsrc%5Etfw">#COVID19</a> en Perú hasta las 8:46 horas del 16 de marzo. <a href="https://twitter.com/hashtag/Per%C3%BAEst%C3%A1EnNuestrasManos?src=hash&amp;ref_src=twsrc%5Etfw">#PerúEstáEnNuestrasManos</a><br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/fsY4PTgtFL">pic.twitter.com/fsY4PTgtFL</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1239569760704253952?ref_src=twsrc%5Etfw">March 16, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  
  03/15

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus en Perú hasta las 13:10 horas del 15 de marzo. <br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/HUR87UVlxQ">pic.twitter.com/HUR87UVlxQ</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1239264360549224449?ref_src=twsrc%5Etfw">March 15, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/14

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus en Perú hasta las 19:20 horas del 14 de marzo. <br><br>Para más información, visita: <a href="https://t.co/F4GaDrvulE">https://t.co/F4GaDrvulE</a> <a href="https://t.co/102MJF7uPE">pic.twitter.com/102MJF7uPE</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1238994119646744578?ref_src=twsrc%5Etfw">March 15, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/13

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus en Perú hasta las 16:00 horas del 13 de marzo. <br><br>Para más información, visita: <a href="https://t.co/ATXAITDSWN">https://t.co/ATXAITDSWN</a> <a href="https://t.co/ERDFeBV7Tq">pic.twitter.com/ERDFeBV7Tq</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1238579158852263936?ref_src=twsrc%5Etfw">March 13, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/12

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus en Perú hasta las 9:55 horas del 12 de marzo. <br><br>Para más información, visita: <a href="https://t.co/ATXAITDSWN">https://t.co/ATXAITDSWN</a> <a href="https://t.co/Yr2gGdPc1f">pic.twitter.com/Yr2gGdPc1f</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1238126203485270022?ref_src=twsrc%5Etfw">March 12, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/11

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus en Perú hasta las 20:00 horas del 11 de marzo. <br><br>Para más información, visita: <a href="https://t.co/ATXAITDSWN">https://t.co/ATXAITDSWN</a> <a href="https://t.co/RgArH78FMz">pic.twitter.com/RgArH78FMz</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1237916429975945217?ref_src=twsrc%5Etfw">March 12, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/10

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus en Perú hasta las 8:00 horas del 10 de marzo. <br><br>Para más información, visita: <a href="https://t.co/Mv594Y83Y2">https://t.co/Mv594Y83Y2</a> <a href="https://t.co/y5o2DZffuL">pic.twitter.com/y5o2DZffuL</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1237366697591541766?ref_src=twsrc%5Etfw">March 10, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/09

<blockquote class="twitter-tweet"><p lang="es" dir="ltr">🗓 Reporte │ Esta es la situación del coronavirus en Perú hasta las 8:30 horas del 9 de marzo. <br><br>Para más información, visita: <a href="https://t.co/ATXAITDSWN">https://t.co/ATXAITDSWN</a> <a href="https://t.co/VUWdMFaJkJ">pic.twitter.com/VUWdMFaJkJ</a></p>&mdash; Ministerio de Salud (@Minsa_Peru) <a href="https://twitter.com/Minsa_Peru/status/1237064965468667904?ref_src=twsrc%5Etfw">March 9, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
  
  03/08

https://www.americatv.com.pe/noticias/actualidad/minsa-confirma-nuevo-caso-coronavirus-peru-n407677?hootPostID=e28f310a3c6b843dce1663d1e934ffed

Este tweet fue RT por el Minsa, por eso asumo que es la información oficial. MINSA no publicó ningún reporte como en los dias anteriores o posteriores. 



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

