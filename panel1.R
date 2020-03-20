library(ggpubr)
dat2 <- data %>%
  dplyr::group_by(Dia)%>%
  dplyr::summarise(Total_Pruebas=max(Total_Pruebas),
                   Positivos=max(Positivos))%>%
  mutate(pos_new=Positivos-lag(Positivos,default = 0),
         pruebas_dia=Total_Pruebas-lag(Total_Pruebas,default = 0))

f3=dat2 %>%
  mutate(neg_new=pruebas_dia-pos_new)%>%
  dplyr::select(Dia, Descartados=neg_new, 
                Positivos=pos_new)%>%
  gather(res, count, -Dia)%>%
  uncount(count) %>%
  ggplot(aes(x = Dia, fill = res)) +
  geom_bar(position = "fill") +
  stat_fill_labels(size=2)+
  #scale_fill_discrete_sequential(palette="BluGrn") +
  labs(y = "proportion", fill = "Resultado",
       title = paste0("Proporción de positivos del total \n de muestras x dia"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  theme_bw() +
  theme(text = element_text(size=10),
        legend.position="bottom")

f1=dat2 %>%
  ggplot(aes(x = Dia)) +
  ylim(0,max(dat2$Positivos)*1.1)+
  geom_bar(aes(y = pos_new, fill = "Nuevos"), stat = "identity", alpha=.5)+
  geom_label(aes(Dia, pos_new, label = pos_new), vjust = 0.5,label.size = 0.10)+
  geom_line(aes(y = Positivos, col = "Acumulados"), size=1) +
  geom_point(aes(y = Positivos), col = "#8B1C62") +
  geom_label(aes(Dia, Positivos, label = Positivos), vjust = -0.3,
             label.size = 0.07)+
  labs(y = "Número de casos reportados", color = " Casos", fill = " ", 
       title = paste0("Numero de casos confirmados"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  #scale_y_continuous() +
  theme_minimal() +
  theme(legend.position="bottom")


f2=data %>%
  dplyr::select(Dia, Positivos, Descartados)%>%
  gather(res, count, -Dia)%>%
  uncount(count) %>%
  ggplot(aes(x = Dia, fill = res)) +
  geom_bar(position = "fill") +
  stat_fill_labels(size=2)+
  #scale_fill_discrete_sequential(palette="BluGrn") +
  labs(y = "proportion", fill = "Resultado",
       title = paste0("Proporción de positivos del total \nde muestras analizadas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  theme_bw() +
  theme(legend.position="bottom")

f4=dat2 %>%
  ggplot(aes(x = Dia)) +
  ylim(0,max(dat2$Total_Pruebas)*1.2)+
  geom_bar(aes(y = pruebas_dia, fill = "Nuevos"), stat = "identity", alpha=.5)+
  geom_label(aes(Dia, pruebas_dia, label = pruebas_dia), vjust = 0.5)+
  geom_line(aes(y = Total_Pruebas, col = "Acumulados"), size=1) +
  geom_point(aes(y = Total_Pruebas), col = "#8B1C62") +
  geom_label(aes(Dia, Total_Pruebas, label = Total_Pruebas), vjust = -0.5,
             label.size = 0.07)+
  labs(y = "Numero de pruebas reportadas", color = " Pruebas", fill = " ",
       title = paste0("Numero de pruebas reportadas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  #scale_y_continuous(sec.axis = sec_axis(~ .)) +
  theme_minimal() +
  theme(legend.position="bottom")

f5=dat2 %>%
  ggplot(aes(x = Dia)) +
  ylim(0,max(dat2$pruebas_dia)*1.2)+
  geom_bar(aes(y = pos_new, fill = "Casos Positivos"), stat = "identity", alpha=.5)+
  geom_label(aes(Dia, pos_new, label = pos_new), vjust = 0.5)+
  geom_line(aes(y = pruebas_dia, col = "Pruebas"), size=1) +
  geom_point(aes(y = pruebas_dia), col = "#8B1C62") +
  geom_label(aes(Dia, pruebas_dia, label = pruebas_dia), vjust = -0.5,
             label.size = 0.03)+
  labs(y = " ", color = " ", fill = " ",
       title = paste0("Pruebas y casos positivos por dia"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_fill_manual(values = c("Casos Positivos" = "#43CD80")) +
  scale_color_manual(values = c("Pruebas" = "#8B1C62")) +
  #scale_y_continuous(sec.axis = sec_axis(~ .)) +
  theme_minimal() +
  theme(legend.position="bottom")


f6=dat2 %>%
  ggplot(aes(x = Dia)) +
  ylim(0,max(dat2$Total_Pruebas)*1.3)+
  geom_bar(aes(y = Positivos, fill = "Positivos Acumulados"), stat = "identity", alpha=.5)+
  geom_label(aes(Dia, Positivos, label = Positivos), vjust = 0.5,label.size = 0.10)+
  geom_line(aes(y = Total_Pruebas, col = "Pruebas Acumuladas"), size=1) +
  geom_point(aes(y = Total_Pruebas), col = "#8B1C62") +
  geom_label(aes(Dia, Total_Pruebas, label = Total_Pruebas), vjust = -0.3,
             label.size = 0.03)+
  labs(y = "", color = "", fill = " ", 
       title = paste0("Tendencias acumuladas"),
       caption = paste0("Actualizado al", format(as.Date(max(data$Dia)),"%d/%m"))) +
  scale_fill_manual(values = c("Positivos Acumulados" = "#43CD80")) +
  scale_color_manual(values = c("Pruebas Acumuladas" = "#8B1C62")) +
  #scale_y_continuous() +
  theme_minimal() +
  theme(legend.position="bottom")

panel=ggarrange(f1,f4,f2,f3,
          ncol=2,nrow=2,
          widths = c(0.5,0.5,0.5,0.5),
          font.label = list(size = 10, color = "black", face =
                              "bold", family = NULL))


annotate_figure(panel,
                top = text_grob("COVID19-PERÚ", color = "red", face = "bold", size = 14),
                bottom = text_grob("Fuente: \n Minsa. Ver (https://jincio.github.io/COVID_19_PERU/Propagacion.html)", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                #left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                #right = "www.joseincio.com",
                #fig.lab = "Figure 1", fig.lab.face = "bold"
)

panel2=ggarrange(f5,f6,
                ncol=1,nrow=2,
                #widths = c(0.5,0.5,0.5,0.5),
                font.label = list(size = 10, color = "black", face =
                                    "bold", family = NULL))

annotate_figure(panel2,
                top = text_grob("COVID19-PERÚ", color = "red", face = "bold", size = 14),
                bottom = text_grob("Fuente: \n Minsa. Ver (https://jincio.github.io/COVID_19_PERU/Propagacion.html)", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                #left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                #right = "www.joseincio.com",
                #fig.lab = "Figure 1", fig.lab.face = "bold"
)


