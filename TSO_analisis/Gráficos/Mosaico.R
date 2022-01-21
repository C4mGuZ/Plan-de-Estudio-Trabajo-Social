df <- diamonds %>%
  group_by(cut, clarity) %>%
  summarise(count = n()) %>%
  mutate(cut.count = sum(count),
         prop = count/sum(count)) %>%
  ungroup()

ggplot(df,
       aes(x = cut, y = prop, width = cut.count, fill = clarity)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
# geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) + # if labels are desired
  facet_grid(~cut, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "RdYlGn") +
# theme(panel.spacing.x = unit(0, "npc")) + # if no spacing preferred between bars
  theme_void() 
