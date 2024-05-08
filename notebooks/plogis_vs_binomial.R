
rv <- rlogis(1000)

bench::press(
  n = 2**(0:13),
  {
    rv <- rlogis(n)
    bench::mark(
      plogis = plogis(rv),
      binomial = binomial()$linkinv(rv),
    )
  }
) -> bp_link

bp_link %>%
  plot() +
  ggpubr::theme_pubclean()

bp_link %>%
  glimpse() %>%
  mutate(expression = as.character(expression)) %>%
  unnest(c(time,gc)) %>%
  select(-memory, -result) %>%
  glimpse() ->
  bp_full

# bp_full %>%
#   group_by(expression) %>%
#   # rowid_to_column("rowid") %>%
#   ungroup() %>%
#   # View()
#   count(rowid)
#   # print(width = Inf)
#   pivot_wider(names_from = expression,
#               values_from = -c(n, expression),
#               id_cols = c(n)) %>%
#   # glimpse() %>%
#   mutate(`plogis <= binomial` = time_plogis <= time_binomial) ->
#   bp_full_flic
#
# bp_full_flic %>%
#   ggplot() +
#   aes(n, `plogis <= binomial`) +
#   geom_step() +
#   # geom_tile(stat = StatSum, size= 10) +
#   theme_grey()
#
# # bench:::autoplot.bench_mark()
#
# #
# # bench::mark(
# #   plogis = plogis(rv),
# #   binomial = binomial()$linkinv(rv),
# # ) -> bm_link
# #
# # bm_link %>%
# #   plot(shape = ".") +
# #   ggpubr::theme_pubclean()
