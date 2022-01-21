## para graficar la mixingmatrix

t1 <- mixingmatrix(red_inter, "Linea_formativa")
  
ggplot(t1,
       aes(x = From, y = Prop, width = Cut.Freq, fill = To)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  #  geom_text(aes(label = scales::percent(Prop)), position = position_stack(vjust = 0.5)) + # if labels are desired
  facet_grid(~From, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  theme(legend.title = element_blank(),
        axis.text.y.right = element_text())