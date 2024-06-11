plot_foi_estimates_constant_model <- function(
    seromodel,
    serosurvey,
    alpha = 0.05,
    foi_df = NULL,
    foi_max = NULL,
    size_text = 11,
    model_type = "time"
) {
  # TODO: Add checks for foi_df (size, colnames, etc.)
  checkmate::assert_class(seromodel, "stanfit", null.ok = TRUE)

  model_name <- paste0(
    seromodel@model_name, "_",
    model_type
  )

  foi_central_estimates <- extract_central_estimates(
    seromodel = seromodel,
    serosurvey = serosurvey,
    alpha = alpha,
    par_name = "foi_expanded"
  )

  if (endsWith(model_name, "age")) {
    xlab <- "Age"
    ages <- 1:max(serosurvey$age_max)
    foi_central_estimates <- mutate(
      foi_central_estimates,
      age = ages
    )

    foi_plot <- ggplot2::ggplot(
      data = foi_central_estimates, ggplot2::aes(x = age)
    )
  } else if (endsWith(model_name, "time")) {
    xlab <- "Year"
    ages <- rev(1:max(serosurvey$age_max))
    years <- unique(serosurvey$tsur) - ages
    foi_central_estimates <- mutate(
      foi_central_estimates,
      year = years
    )

    foi_plot <- ggplot2::ggplot(
      data = foi_central_estimates, ggplot2::aes(x = year)
    )
  }

  if (is.null(foi_max))
    foi_max <- max(foi_central_estimates$upper)

  foi_plot <- foi_plot +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$lower,
        ymax = .data$upper
      ),
      fill = "#41b6c4",
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$median),
      colour = "#253494"
    ) +
    ggplot2::theme_bw(size_text) +
    ggplot2::coord_cartesian(ylim = c(0, foi_max)) +
    ggplot2::ylab("Force-of-Infection") +
    ggplot2::xlab(xlab)

  return(foi_plot)
}
