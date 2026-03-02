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
                          predictors = "Condition",
                          change = FALSE,
                          Bayesian = TRUE
                          ) {
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
      formula_str <- paste0(variable, "After ~ ", variable, "Before + ",predictors)
    } else {
      formula_str <- paste0(variable, " ~ ", predictors)
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
      y = if (ytext) ylab else NULL,
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
      #   warning("Loading Bayesian output but Bayesian analysis not selected: change run_models argument to FALSE")
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


# plotMultipleModelCoeffs <- function(model_output,
#                                     control_var,
#                                     title = NULL,
#                                     xlab = "Estimate",
#                                     ylab = "Model",
#                                     legend.position = "top",
#                                     xlim = NULL,
#                                     by_topic = TRUE,
#                                     Bayesian = TRUE,
#                                     base_size = base_size) {
#   
#   clean <- model_output %>%
#     filter(term != "(Intercept)") %>%
#     mutate(term = as.character(term)) %>%
#     group_by(model) %>%
#     mutate(has_interaction = any(str_detect(term, fixed(":")))) %>%
#     ungroup() %>%
#     # remove control main effect only
#     filter(!term %in% c(control_var, paste0("`", control_var, "`"))) %>%
#     group_by(model, has_interaction) %>%
#     filter(if (has_interaction[1]) str_detect(term, fixed(":")) else TRUE) %>%
#     ungroup() %>%
#     mutate(condition = case_when(
#       grepl("ConditionLeft", term) ~ "Left",
#       grepl("ConditionRight", term) ~ "Right",
#       TRUE ~ "Not by condition"
#     ))
#   
#   ggplot(clean, aes(x = estimate, y = model, colour = condition)) +
#     geom_point(size = 3, position = position_dodge(width = 0.6)) +
#     geom_errorbar(
#       aes(
#         xmin = estimate - 1.96 * std.error,
#         xmax = estimate + 1.96 * std.error
#       ),
#       width = 0.2,
#       position = position_dodge(width = 0.6)
#     ) +
#     geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
#     labs(x = xlab, y = ylab, title = title) +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.grid.major.y = element_blank(),
#       legend.position = "none"
#     )
# }

# load and combine posts seen data with survey data
load_and_combine_data <- function(troll = "no_troll") {
  
  load(here("data/magpie/processed/combined/posts-seen-by-user.Rdata"))
  
  # Select appropriate posts-seen data
  posts_seen <- if (troll == "no_troll") {
    unique_posts_seen_by_user_no_troll
  } else if (troll == "all_posts") {
    unique_posts_seen_by_user 
  } else if (troll == "only_troll") {
    unique_posts_seen_by_user_troll_only
  }
  
  
  
  # Load posts-seen data
  load(here("data/magpie/processed/combined/posts-seen-by-user.Rdata"))
  
  # Load replies received data
  replies_received <- read_csv(
    here("data/magpie/processed/combined/replies-received.csv"))# %>%
  # keep participants with posts-seen data
  #filter(UserName %in% posts_seen$username)
  
  # add participants who are missing with 
  
  # Load survey data
  load(here("data/magpie/processed/combined/survey-before-after-change.Rdata"))
  # Keep participants with posts-seen data
  d_survey <- d_before_after_change %>%
    filter(UserName %in% posts_seen$username)
  
  # Merge datasets
  combined <- d_survey %>%
    left_join(
      posts_seen,
      by = c("UserName" = "username", "Condition")
    ) %>%
    left_join(
      replies_received,
      by = c("UserName", "Condition")
    )
  
  # Scale numeric variables for coefficient comparison
  # Use as.numeric(scale()) to avoid matrix columns
  combined_scaled <- combined %>%
    mutate(across(where(is.numeric), ~as.numeric(scale(.))))
  
  list(
    scaled = combined_scaled,
    unscaled = combined
  )
}

runModelComparisons = function(data,
                               variables,
                               comparison_models,
                               predictor_names,
                               change = FALSE,
                               run_models = TRUE,
                               skip_fitted = TRUE,
                               save_dir = "R/analyse/lm-output/posts-seen/"
) {
  
  models_dir <- file.path(save_dir, "models")
  if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
  
  make_label = function(predictor_str) {
    terms <- trimws(strsplit(predictor_str, "\\+")[[1]])
    paste(names(predictor_names)[match(terms, predictor_names)], collapse = "_")
  }
  
  labels <- sapply(comparison_models, make_label)
  legend <- data.frame(label = labels, predictors = comparison_models)
  
  for (j in seq_along(comparison_models)) {
    predictors <- comparison_models[j]
    label      <- labels[j]
    base_model <- NULL
    
    print(paste0("Running models with predictors: ", predictors))
    
    for (i in seq_along(variables)) {
      variable   <- variables[i]
      change_i   <- if (length(change) > 1) change[i] else change
      model_path <- file.path(models_dir, paste0(variable, "__", label, ".rdata"))
      
      print(paste0("Running for outcome variable: ", variable))
      
      if (skip_fitted && file.exists(model_path)) {
        print(paste0("Skipping — loading saved loo"))
        e <- new.env()
        load(model_path, envir = e)  # restores `loo_to_save`
        # base_model stays NULL — update() won't work but we don't need it for skipped models
      } else {
        if (change_i) {
          formula_str <- paste0(variable, "After ~ ", variable, "Before + ", predictors)
        } else {
          formula_str <- paste0(variable, " ~ ", predictors)
        }
        
        if (is.null(base_model)) {
          base_model <- brm(
            data    = data,
            formula = as.formula(formula_str),
            family  = Beta(),
            cores   = 2
          )
        } else {
          base_model <- update(base_model, formula = as.formula(formula_str), newdata = data)
        }
        
        base_model <- add_criterion(base_model, "loo")
        loo_to_save    <- base_model$criteria$loo
        fixef_to_save  <- fixef(base_model) %>% 
          as.data.frame() %>% 
          rownames_to_column("term")
        save(loo_to_save, fixef_to_save, file = model_path)
      }
      
      # Free memory after each model is saved
      rm(base_model)
      base_model <- NULL
      gc()
    }
  }
}


computeModelWeights = function(variables,
                               legend,
                               save_dir = "R/analyse/lm-output/posts-seen/"
) {
  
  models_dir     <- file.path(save_dir, "models")
  weights_output <- NULL
  fixef_output   <- NULL
  
  for (variable in variables) {
    loo_objects <- list()
    
    for (j in seq_len(nrow(legend))) {
      model_path <- file.path(models_dir, paste0(variable, "__", legend$label[j], ".rdata"))
      
      if (!file.exists(model_path)) {
        warning(paste0("Missing model file: ", basename(model_path), " — skipping"))
        next
      }
      
      e <- new.env()
      load(model_path, envir = e)
      loo_objects[[legend$predictors[j]]] <- e$loo_to_save
      
      # extract fixed effects if available
      if (exists("fixef_to_save", envir = e)) {
        fixef_output <- bind_rows(fixef_output,
                                  e$fixef_to_save %>% mutate(predictors = legend$predictors[j], variable = variable)
        )
      }
    }
    
    weights_i <- loo_model_weights(unname(loo_objects), method = "pseudobma") %>%
      as.numeric() %>%
      data.frame(weight = .) %>%
      mutate(
        predictors = names(loo_objects),
        weight     = weight / sum(weight)
      ) %>%
      left_join(legend, by = "predictors") %>%
      mutate(variable = variable)
    
    weights_output <- bind_rows(weights_output, weights_i)
  }
  
  list(weights = weights_output, fixef = fixef_output)
}