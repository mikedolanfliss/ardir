
# Quick testing (should be external report or file)
mvc_aaf_tbl |> arrange(state, year, sex, age_range) |> filter(state == "North Carolina", year == 2024)
ggplot(mvc_aaf_tbl |> filter(state == "North Carolina"), aes(x = age_range, color = year, y = aaf_num, group = year))+
  geom_line(stat = "smooth", se = F, alpha = 0.7)+
  facet_grid(.~sex)+
  theme_minimal()+
  labs(title = "NC MVC AAFs")
ggsave("testing/MVC AAFs - North Carolina Graph.png")

ggplot(mvc_aaf_tbl |> filter(state == "Rhode Island"), aes(x = age_range, color = year, y = aaf_num, group = year))+
  geom_line(stat = "smooth", se = F, alpha = 0.7)+
  facet_grid(.~sex)+
  theme_minimal()+
  labs(title = "Rhode Island MVC AAFs")
ggsave("testing/MVC AAFs - Rhode Island Graph.png")
