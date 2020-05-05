tendencias_grafico=function(data,variable)
{
  temp2=data%>%dplyr::select(REGION, variable,Fecha)%>%
    mutate(
      MACROREG= dplyr::case_when(
        REGION %in% c("TUMBES","PIURA","LAMBAYEQUE","LA LIBERTAD","CAJAMARCA","ANCASH") ~"NORTE",
        REGION %in% c("AMAZONAS","LORETO","UCAYALI","MADRE DE DIOS","SAN MARTIN") ~ "ORIENTE",
        REGION %in% c("ICA","MOQUEGUA","AREQUIPA","TACNA","PUNO","CUSCO") ~ "SUR",
        REGION %in% c("AYACUCHO","APURIMAC","JUNIN", "HUANCAVELICA", "HUANUCO", "PASCO") ~ "CENTRO",
        REGION %in% c("LIMA", "CALLAO") ~ "LIMAyCALLAO",
        TRUE ~ "OTRO"))%>%
    na.omit()%>%
    group_by(Fecha,MACROREG)%>%
    summarise(variable=sum(variable))%>%
    ungroup()%>%
    filter(variable>10)%>%
    group_by(MACROREG)%>%
    mutate(variable2=ifelse(variable==min(variable),10,variable)
    )%>%
    mutate(days=seq(0,n()-1,1))%>%
    ungroup()
  
  gg=ggplot()+
    geom_line(data=temp2,aes(x=days,y=variable2,
                             color=MACROREG,group=MACROREG))+
    geom_point_interactive(data=temp2,aes(x=days,y = variable2,tooltip = variable2,
                                          group=MACROREG, color=MACROREG),
                           size=1)+
    theme_bw() +
    geom_line(data=data1,aes(x=days,y=dd), linetype="dashed")+
    geom_line(data=data1,aes(x=days,y=d4),linetype="dashed")+
    geom_line(data=data1,aes(x=days,y=d6),linetype="dashed")+
    geom_line(data=data1,aes(x=days,y=d10),linetype="dashed")+
    coord_cartesian(ylim=c(10, max(temp2$variable2)),
                    xlim=c(0,max(temp2$days)))+
    scale_y_continuous(trans = "log10")+
    annotate("text", x = 11, y = 510, label = "duplica:2 dias",
             angle=55, color="red", size=3)+
    annotate("text", x = 16, y = 198, label = "duplica:4 dias",
             angle=35, color="orange",size=3)+
    annotate("text", x = 22, y = 150, label = "duplica: 6 dias",
             angle=28, color="orange", size=3)+
    annotate("text", x = 20, y = 50, label = "duplica: 10 dias",
             angle=19, color="orange", size=3)+
    ggtitle("Tendencia de fallecidos por Macro-Región")+
    theme(legend.position="bottom")+
    labs(y = "Número de Fallecidos", color = " Macro-Region", fill = " ", 
         #title = paste0("Numero de casos confirmados"),
         caption = paste0("Actualizado al", format(as.Date(max(temp2$Dia)),"%d/%m"),
                          "\n Fuente Minsa. Repositorio:https://github.com/jincio/COVID_19_PERU",
                          "\n créditos: @fjsistemas-@jincio"))
  
  girafe(code = print(gg) )
  
}


