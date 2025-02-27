setwd("~/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/")

scale_fill_manual(
  values=c("#0a6264","#428f87","#86bca6","#e6e2bd","#ecb176","#e77f44","#db4324"),
  labels=c("0", "1", "2", "3-5", "6-10", "11-50", "51+", ""),
  name="",
  guide = guide_legend(
    keyheight = unit(3, units = "mm"),
    keywidth=unit(12, units = "mm"),
    label.position = "bottom",
    title.position = 'top', nrow=1)
)


mapdata$oid <- factor(mapdata$OBJECTID)
mapdata$st <- factor( as.numeric(mapdata$ST_CODE) + as.numeric(mapdata$ST_CODE)*(1-2*(as.numeric(mapdata$ST_CODE) %% 3)))
  
states <- ggplot(mapdata, aes( x = long, y = lat, group = group )) +
  theme_void() +
  geom_polygon(aes(fill=st), color = NA) +
  scale_fill_grey() + 
# scale_fill_brewer(  type = "qual",palette = "Accent") +
  #scale_fill_viridis(discrete = TRUE) +
      theme(legend.position="none") +
    theme(
    text = element_text(color = "#22211d", family="Palatino Linotype"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=15, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
  ) +
  coord_map()

states



ggsave("./Output/figures/map_states.pdf", family="Palatino Linotype",
       states,
       width = 8, height = 8,
       device = cairo_pdf)



