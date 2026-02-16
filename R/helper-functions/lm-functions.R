# Transform (0, 100) to strictly (0, 1), avoiding exactly 0 and 1 (smithson transformation)
scale_to_01 <- function(x, n = length(x)) {
  if (max(x) <= 10) {
    # for 1-10 scales instead of 1-100 scales (hacky)
    y <- x / 100  # First scale to [0, 1]
  } else {
    y <- x / 100
  }
  (y * (n - 1) + 0.5) / n
}

#
# # apply to data
# d_before_after_change_scale01 <- d_before_after_change %>%
#   mutate(across(
#     where(is.numeric) &
#       (ends_with("Before") | ends_with("After")),
#     scale_to_01
#   ))
#
# scale_to_01_beta <- function(x) {
#   # First scale to [0, 1]
#   x_scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#   # Then transform to (0, 1) - strictly between, not including boundaries
#   x_scaled <- pmin(pmax(x_scaled, 0.001), 0.999)
#   return(x_scaled)
# }

runMultipleLMs = function(data,
                          variables,
                          change = FALSE,
                          Bayesian = TRUE) {
  model_output <-  NULL
  for (i in 1:length(variables)) {
    variable <- variables[i]
    
    if (length(change) > 1) {
      change_i <- change[i]
    } else {
      change_i <- change
    }
    
    # Dynamically build formula as string
    
    if (change_i) {
      formula_str <- paste0(variable, "After ~ ", variable, "Before + Condition")
    } else {
      formula_str <- paste0(variable, " ~ Condition")
    }
    
    if (Bayesian) {
      m = brm(
        data = data,
        formula = as.formula(formula_str),
        family = Beta(),
        cores = 2
      )
      summ_m <- summary(m)$fixed %>%
        mutate(model = variable) %>%
        rownames_to_column(var = "term") %>%
        rename(estimate = Estimate) # rename to be consistent with glm
      
      model_output <- bind_rows(model_output, summ_m) %>%
        mutate(model = factor(model, levels = variables))
    } else {
      m = glm(
        data = data,
        formula = as.formula(formula_str),
        family = quasibinomial(link = "logit"),
      ) %>%
        tidy() %>%
        mutate(model = variable)
      
      model_output <- bind_rows(model_output, m) %>%
        mutate(model = factor(model, levels = variables))
    }
    
    
  }
  model_output
}

plotMultipleModelCoeffs = function(model_output,
                                   title = NULL ,
                                   xlab = NULL,
                                   ylab = NULL,
                                   subtitle = NULL,
                                   legend.position = "none",
                                   ytext = TRUE,
                                   xlim = c(-1.2, 0.8),
                                   by_topic = TRUE,
                                   n_comparisons = 23,
                                   Bayesian = TRUE,
                                   base_size = 14) {
  keywords <- c("AI", "Trans", "Climate", "Israel")
  
  # Extract condition label (Left/Right)
  clean_m <- model_output %>%
    filter(term == "ConditionLeft" | term == "ConditionRight") %>%
    mutate(model_clean = model,
           Condition = ifelse(grepl("Left", term), "Left", "Right"))
  
  
  if (by_topic) {
    clean_m <- clean_m %>%
      mutate(
        model_clean = str_extract(model, paste(keywords, collapse = "|")),
        model_clean = if_else(is.na(model_clean), "Overall", model_clean),
        model_clean = factor(model_clean, levels = c(keywords, "Overall"))
      )
  }
  
  
  
  
  # Compute bonferroni adjusted CIs
  if (!Bayesian) {
    adjusted_alpha <- 0.05 / n_comparisons
    z_crit <- qnorm(1 - adjusted_alpha / 2)
  }
  
  
  
  
  # Plot
  p <- ggplot(clean_m, aes(x = estimate, y = model_clean, color = Condition)) +
    geom_errorbar(
      aes(
        xmin = if (Bayesian)
          `l-95% CI`
        else
          estimate - z_crit * std.error,
        xmax = if (Bayesian)
          `u-95% CI`
        else
          estimate + z_crit * std.error
      ),
      width = 0.2,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      aes(fill = Condition),
      position = position_dodge(width = 0.6),
      size = 3,
      shape = 21,
      colour = "black",
      stroke = 0.6
    ) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = conditionColours["Control"]) +
    scale_color_manual(values = conditionColours) +
    scale_fill_manual(values = conditionColours) +
    labs(
      x = xlab,
      y = ylab,
      color = "Condition",
      title = title,
      subtitle = subtitle
    ) +
    lims(x = xlim) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = legend.position,
      plot.title = element_text(hjust = 0)
    )
  
  
  if (!ytext) {
    p <- p +
      theme(axis.text.y = element_blank())
  }
  p
}


plotMultipleBarPlots = function(data,
                                variables,
                                y_title,
                                title = NULL,
                                subtitle = NULL,
                                legend.position = "none",
                                change = TRUE,
                                ylim = c(-10, 10),
                                scales = "fixed",
                                base_size = 14) {
  if (change) {
    change_cols <- paste0(variables, "Change")
  } else {
    change_cols <- variables
  }
  
  d_bar_plots <- data[, c("UserName", "Condition", change_cols)] %>%
    mutate(Condition = factor(Condition, levels = condition_order))
  
  
  
  
  d_bar_plots_long <- d_bar_plots %>%
    pivot_longer(cols = change_cols, names_to = "Variable")
  
  # Strip the variable names to their core topic so that the order can be aligned with the coeffient plots
  keywords <- c("AI", "Trans", "Climate", "Israel")
  
  # check if this is a variable that needs to be broken down by topic (keyword)
  if (any(str_extract(d_bar_plots_long$Variable, paste(keywords, collapse = "|")) %in% keywords)) {
    d_bar_plots_long <- d_bar_plots_long %>%
      mutate(
        Variable = str_extract(Variable, paste(keywords, collapse = "|")),
        Variable = if_else(is.na(Variable), "Overall", Variable),
        Variable = factor(Variable, levels = rev(c(
          keywords, "Overall"
        )))
      )
  }
  
  
  
  
  
  d_long_summ <- d_bar_plots_long %>%
    group_by(Condition, Variable) %>%
    summarise(mean = mean(value),
              sd = sd(value),
              n = n()) %>%
    mutate(se = sd / sqrt(n))
  
  
  p <-
    d_long_summ %>%
    ggplot(aes(x = Condition, y = mean, fill = Condition)) +
    geom_col(alpha = .5, colour = "black") +
    geom_jitter(
      data = d_bar_plots_long,
      aes(y = value, colour = Condition),
      alpha = .15,
      size = 0.5
    ) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2) +
    facet_wrap(~ Variable, ncol = 1, scales = scales) +
    scale_fill_manual(values = conditionColours) +
    scale_colour_manual(values = conditionColours) +
    theme_minimal(base_size = base_size) +
    coord_flip() +
    labs(
      y = y_title ,
      x = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme(
      # Removes the background box
      strip.background = element_blank(),
      # Removes the text labels
      strip.text = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = legend.position
    )
  
  if (is.numeric(ylim)) {
    p <- p + lims(y = ylim)
  }
  
  p
}

plot_lm_and_bar = function(variable_groups,
                           xlabs_lm,
                           xlabs_bar,
                           titles,
                           d_lm,
                           d_bar,
                           change = TRUE,
                           ylim = NULL,
                           ytext = FALSE,
                           xlim = c(-1.2, 0.8),
                           by_topic = TRUE,
                           Bayesian = TRUE,
                           run_models = TRUE,
                           title_size = 16,
                           base_size = 14) {
  plot_list <- list()
  
  for (i in 1:length(variable_groups)) {
    variable_group <- variable_groups[[i]]
    output_name <- capture.output(cat(paste0(variable_group), sep = "-"))
    
    if (run_models) {
      models <- runMultipleLMs(d_lm,
                               variable_group,
                               change = change,
                               Bayesian = Bayesian)
      save(models, file = here(
        paste0(
          "R/analyse/lm-output/",
          output_name,
          "-Bayes-",
          Bayesian,
          ".Rdata"
        )
      ))
    } else {
      # if (!Bayesian){
      #   warning("Loading Bayisan output but Bayesian analysis not selected: change run_models argument to FALSE")
      # }
      load(here(
        paste0(
          "R/analyse/lm-output/",
          output_name,
          "-Bayes-",
          Bayesian,
          ".Rdata"
        )
      ))
    }
    
    xlab_lm <- xlabs_lm[i]
    xlab_bar <- xlabs_bar[i]
    title <- titles[i]
    
    if (i == 1) {
      legend.position = "top"
    } else{
      legend.position = "none"
    }
    
    p_models <- plotMultipleModelCoeffs(
      models,
      NULL,
      xlab_lm,
      ytext = ytext,
      legend.position = legend.position,
      xlim = xlim,
      by_topic = by_topic,
      Bayesian = Bayesian,
      base_size = base_size
    )
    
    p_bar <- plotMultipleBarPlots(
      d_bar,
      variable_group,
      xlab_bar,
      ylim = ylim,
      scales = "free",
      change = change,
      legend.position = legend.position,
      base_size = base_size
    )
    
    p_combined <- ggarrange(p_models, p_bar)
    
    p_combined <- annotate_figure(p_combined, top = text_grob(title, size = title_size, face = "bold"))
    
    plot_list[[i]] <- p_combined
  }
  
  plot_list
}
