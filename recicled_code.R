### Reciclado
#### Paneles para mapas

panel<- ggpubr::ggarrange(m1,
                          ncol=1,nrow=1,
                          font.label = list(size = 10, color = "black", face = "bold", family = NULL))
ggpubr::annotate_figure(panel,
                        top = ggpubr::text_grob("Distribución  de casos positivos COVID-19 x departamento,\n (moleculares + pruebas rápidas) Perú", 
                                                color = "#474785", 
                                                face = "bold", 
                                                size = 14),
                        bottom = ggpubr::text_grob(
                          paste0(
                            "Actualizado al ", 
                            format(as.Date(max(data$Dia)),"%d/%m"),
                            "\n",
                            "Fuente: MINSA.Ver (https://jincio.github.io/COVID_19_PERU/Propagacion.html)"), 
                          color = "black", 
                          hjust = 1, 
                          x = 1, 
                          face = "italic", 
                          size = 8)
                        #left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                        #fig.lab = "Figure 1", fig.lab.face = "bold"
)


```{r Panelm2, echo=FALSE, fig.height=7, message=FALSE, warning=FALSE}
# Panel para publicacion
panel<- ggpubr::ggarrange(m2,
                          ncol=1,nrow=1,
                          font.label = list(size = 10, color = "black", face = "bold", family = NULL))
ggpubr::annotate_figure(panel,
                        top = ggpubr::text_grob("Distribución  de fallecidos COVID-19 x departamento,\n Perú", 
                                                color = "#474785", 
                                                face = "bold", 
                                                size = 14),
                        bottom = ggpubr::text_grob(
                          paste0(
                            "Actualizado al ", 
                            format(as.Date(max(data$Dia)),"%d/%m"),
                            "\n",
                            "Fuente: MINSA.Ver (https://jincio.github.io/COVID_19_PERU/Propagacion.html)"), 
                          color = "black", 
                          hjust = 1, 
                          x = 1, 
                          face = "italic", 
                          size = 8)
                        #left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                        #fig.lab = "Figure 1", fig.lab.face = "bold"
)

```
