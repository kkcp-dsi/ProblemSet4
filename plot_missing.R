plot_missing <- function(data, percent=F) {

  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()

  pivot_missing_patterns <- missing_patterns %>%
    rownames_to_column(var = "id") %>%
    gather(key, value, -c(id, count))

  variable_missing <- missing_patterns %>%
    summarise(across(-one_of(c("count")), ~ sum(., is.na(.), 0))) %>%
    gather(key, value = "missing_count") %>%
    arrange(desc(missing_count))

  pivot_missing_patterns <- pivot_missing_patterns %>%
    inner_join(variable_missing, "key") %>%
    arrange(desc(missing_count), id) %>%
    mutate(id = factor(id)) %>%
    mutate(key = fct_reorder(key, missing_count, .desc = T))

  missing_id <- pivot_missing_patterns %>%
    group_by(id) %>%
    summarise(n_of_missing = sum(value)) %>%
    filter(n_of_missing == 0) %>%
    pull(id)

  x_levels <- levels(pivot_missing_patterns$key)
  missing_id_x <- x_levels[ceiling((length(x_levels)) / 2)]
  missing_id_y <- levels(missing_id)[missing_id]

  pivot_missing_patterns <- pivot_missing_patterns %>%
    mutate(value = ifelse(id == missing_id_y, "Complete", ifelse(value, "Missing", "Not missing"))) %>%
    mutate(value = factor(value, levels = c("Missing", "Not missing", "Complete")))

  ## visualize missing value pattern
  p_tile <- ggplot(pivot_missing_patterns, aes(x = key, y = fct_rev(id), fill = value)) +
    geom_tile(color = "white") +
    scale_fill_manual(values=c("#756bb1", "#bdbdbd", "#636363")) +
    xlab("Variable")  +
    ylab("Missing Pattern") +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    annotate("text", x = missing_id_x, y = missing_id_y, label = "Complete Case")

  input_variable_hist <- pivot_missing_patterns %>%
    group_by(key) %>%
    summarise(variable_count=sum(count[value=="Missing"]))

  if (percent){
    input_variable_hist$variable_count = input_variable_hist$variable_count / nrow(data)
  }

  input_pattern_hist <- missing_patterns %>%
    rownames_to_column(var = "id") %>%
    select(c(id, count))

  if (percent){
    input_pattern_hist$count = input_pattern_hist$count / nrow(data)
  }

  p_variable_hist <- ggplot(input_variable_hist, aes(x=key, y=variable_count)) +
    geom_bar(stat = "identity", fill="#9ecae1") +
    xlab(element_blank()) +
    ylab("number rows missing") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_line( size=.1, color="gray")
    )

  if (percent){
    p_variable_hist <- p_variable_hist +
      ylab("% rows missing") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L, suffix=""), limits = c(0, 1.0))
  }

  p_pattern_hist <- ggplot(input_pattern_hist, aes(x=fct_rev(id), y=count)) +
    geom_bar(stat = "identity", fill="#9ecae1") +
    coord_flip() +
    xlab(element_blank())  +
    ylab("row count") +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank()
    )

  if (percent){
    p_pattern_hist <- p_pattern_hist +
      ylab("% rows") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L, suffix=""), limits = c(0, 1.0))
  }

  p_variable_hist + plot_spacer() + p_tile + p_pattern_hist + plot_layout(widths = c(4, 1), heights = c(2, 8))
}
