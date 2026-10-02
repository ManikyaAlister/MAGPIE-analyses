# ─────────────────────────────────────────────────────────────────────────────
# Plotting functions for the "posts seen" coefficient and correlation grids.
#
# These are shared between the main analysis (03_linear-modelling.qmd, which
# produces the main Figure 5 using `plotCoefficientsGrid(trust_type = "overall")`)
# and the supplementary materials (04_supplementary-materials.qmd, which produces
# the trust-subscale coefficient grid and both correlation grids). They were
# previously defined inline in 03_linear-modelling.qmd; they live here so both
# documents can source them rather than redefining them.
#
# Expected to be available in the calling environment:
#   - colour palettes are not required (colours are hard-coded below)
#   - `plotCoefficientsGrid` loads BRMS fixef output from
#       output/models/posts-seen/weights/<group><run_label>.rdata
#   - `plotCorrelationsGrid` requires `cor_all`, `cor_nontroll`, `tidy_cor`,
#       `change_beliefs`, `change_consensus`, `change_trust`, and
#       `after_only_questions`. These are produced by sourcing
#       R/functions/posts-seen-correlations-prep.R
#
# Required packages: tidyverse, patchwork, cowplot, scales, here
# ─────────────────────────────────────────────────────────────────────────────

# ── Shared panel builder (used by both grid functions) ────────────────────
# Expects: Estimate, Q2.5, Q97.5, variable (factor), comparison (factor),
#          predictor (factor). All points are drawn fully opaque: colour and
#          shape only distinguish all content from non-troll content.
make_coef_panel = function(d, title, x_label, hide_y = FALSE) {
  d <- d %>%
    mutate(comparison = factor(comparison, levels = c("All Content", "Non-Troll Content")))

  p <- ggplot(d, aes(x = Estimate, y = variable,
                     colour = comparison,
                     shape  = comparison,
                     group  = comparison)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
    geom_errorbarh(
      aes(xmin = Q2.5, xmax = Q97.5),
      height    = 0.2,
      linewidth = 1,
      position  = position_dodge(width = 0.5)
    ) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    facet_wrap(~ predictor, nrow = 1) +
    scale_colour_manual(
      values = c("All Content" = "#E15759", "Non-Troll Content" = "#4E79A7"),
      drop = FALSE   # keep both levels in legend even if absent from a panel
    ) +
    scale_shape_manual(
      values = c("All Content" = 16, "Non-Troll Content" = 17),
      drop = FALSE
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text       = element_text(face = "bold", size = 11),
      axis.title.x     = element_text(size = 12, colour = "grey40"),
      panel.spacing    = unit(0.6, "lines"),
      panel.grid.minor = element_blank(),
      legend.text      = element_text(size = 14),
      title            = element_text(face = "bold", size = 14)
      # legend.position intentionally omitted — controlled by assemble_grid
    ) +
    labs(x = x_label, y = NULL,
         colour = "", shape = "",
         title  = title)

  if (hide_y) p <- p + theme(axis.text.y  = element_blank(),
                              axis.ticks.y = element_blank())
  p
}
# ── Shared patchwork assembly ─────────────────────────────────────────────
# Left column: beliefs + consensus
# Right column: trust + after_only (matching updated plotCoefficientsGrid layout)
assemble_grid = function(p_beliefs, p_consensus, p_trust, p_after_list,
                          n_belief_vars, n_consensus_vars, n_trust_vars) {

  # Use p_trust — contains both comparison levels
  legend <- cowplot::get_legend(
    p_trust + theme(legend.position = "bottom") 
  )

  no_legend <- function(p) p + theme(legend.position = "none")

  col_left  <- wrap_plots(
    lapply(list(p_beliefs, p_consensus), no_legend),
    ncol    = 1,
    heights = c(n_belief_vars, n_consensus_vars)
  )
  col_right <- wrap_plots(
    lapply(c(list(p_trust), p_after_list), no_legend),
    ncol    = 1,
    heights = c(n_trust_vars, rep(1, length(p_after_list)))
  )

  main <- col_left | col_right

  cowplot::plot_grid(
    main, legend,
    ncol        = 1,
    rel_heights = c(1, 0.05)
  )
}

# ── BRMS coefficients ───────────────────
plotCoefficientsGrid = function(run_label = NULL, title = NULL,
                                trust_type = "overall") {

  trust_group <- if (trust_type == "overall") "trust_overall" else "change_trust"
  keywords    <- c("AI", "Trans", "Climate", "Israel")

  after_only_config <- list(
    list(var = "OverallExperience", title = "D. Overall experience",
         x_label = "< 0 = enjoyed less"),
    list(var = "PropLiked",         title = "E. Proportion of users liked",
         x_label = "< 0 = liked fewer other users"),
    list(var = "PropTrolls",        title = "F. Proportion perceived as trolls",
         x_label = "< 0 = fewer trolls perceived"),
    list(var = "PropAligned",       title = "G. Perceived political alignment",
         x_label = "< 0 = less politically aligned"),
    list(var = "MagpieSimilarity",  title = "H. Similarity to real social media",
         x_label = "< 0 = less similar")
  )

  load_fixef = function(group) {
    load(here(paste0("output/models/posts-seen/weights/", group, run_label, ".rdata")))
    bind_rows(
      results_all$fixef      %>% mutate(comparison = "All Content"),
      results_nontroll$fixef %>% mutate(comparison = "Non-Troll Content")
    ) %>%
      filter(
        term != "Intercept",
        !grepl("Before$", term),
        !grepl("^Condition", term),
        predictors != "Condition",
        !predictors %in% c("prop_right_all + prop_left_all",
                           "prop_right_nontroll + prop_left_nontroll")
      ) %>%
      mutate(
        credible  = sign(Q2.5) == sign(Q97.5),
        predictor = case_when(
          predictors %in% c("prop_right_all",    "prop_right_nontroll")    ~ "Prop. Right Seen",
          predictors %in% c("prop_left_all",     "prop_left_nontroll")     ~ "Prop. Left Seen",
          predictors %in% c("prop_friendly_all", "prop_friendly_nontroll") ~ "Prop. Friendly Seen"
        ),
        predictor  = factor(predictor, levels = c("Prop. Right Seen", "Prop. Left Seen", "Prop. Friendly Seen")),
        comparison = factor(comparison, levels = c("All Content", "Non-Troll Content"))
      )
  }

  d_beliefs <- load_fixef("change_beliefs") %>%
    mutate(
      variable = str_extract(variable, paste(keywords, collapse = "|")),
      variable = if_else(is.na(variable), "Overall", variable),
      variable = factor(variable, levels = rev(c(keywords, "Overall")))
    )
  p_beliefs <- make_coef_panel(d_beliefs,
    title = "A. Political beliefs", x_label = "< 0 = beliefs more left wing")

  d_consensus <- load_fixef("change_consensus") %>%
    mutate(
      variable = str_extract(variable, paste(keywords, collapse = "|")),
      variable = if_else(is.na(variable), "Overall", variable),
      variable = factor(variable, levels = rev(c(keywords, "Overall")))
    )
  p_consensus <- make_coef_panel(d_consensus,
    title = "B. Perceptions of consensus", x_label = "< 0 = less belief in consensus")

  d_trust <- load_fixef(trust_group) %>%
    mutate(variable = str_remove(variable, "^Trust"))
  if (trust_type == "subscales")
    d_trust <- d_trust %>%
      mutate(variable = factor(variable,
               levels = rev(c("Media", "Unis", "Govt", "Business", "People", "Overall"))))

  p_trust <- make_coef_panel(d_trust,
    title   = "C. Overall trust",
    x_label = "< 0 = less trusting",
    hide_y  = (trust_type == "overall"))

  d_after      <- load_fixef("after_only_questions")
  p_after_list <- lapply(after_only_config, function(cfg)
    make_coef_panel(d_after %>% filter(variable == cfg$var),
                    title = cfg$title, x_label = cfg$x_label, hide_y = TRUE))

  assemble_grid(p_beliefs, p_consensus, p_trust, p_after_list,
                n_belief_vars    = length(levels(d_beliefs$variable)),
                n_consensus_vars = length(levels(d_consensus$variable)),
                n_trust_vars     = if (trust_type == "overall") 1 else 6)
}

# correlation equivalent ───────────────────────────────────────────
plotCorrelationsGrid = function(trust_type = "overall") {

  keywords <- c("AI", "Trans", "Climate", "Israel")

  after_only_config <- list(
    list(var = "OverallExperience", title = "D. Overall experience",
         x_label = "< 0 = enjoyed less"),
    list(var = "PropLiked",         title = "E. Proportion of users liked",
         x_label = "< 0 = liked fewer other users"),
    list(var = "PropTrolls",        title = "F. Proportion perceived as trolls",
         x_label = "< 0 = fewer trolls perceived"),
    list(var = "PropAligned",       title = "G. Perceived political alignment",
         x_label = "< 0 = less politically aligned"),
    list(var = "MagpieSimilarity",  title = "H. Similarity to real social media",
         x_label = "< 0 = less similar")
  )

  # Prep into the same column format make_coef_panel expects
  d_cor <- bind_rows(
    tidy_cor(cor_all,      "All posts"),
    tidy_cor(cor_nontroll, "Non-troll initiated posts")
  ) %>%
    rename(variable = Parameter2) %>%
    mutate(
      variable   = str_remove(variable, "Change$"),   # align names with fixef
      Estimate   = r,
      Q2.5       = CI_low,
      Q97.5      = CI_high,
      comparison = recode(type,
        "All posts"                 = "All Content",
        "Non-troll initiated posts" = "Non-Troll Content"
      ),
      comparison = factor(comparison, levels = c("All Content", "Non-Troll Content")),
      credible   = sign(Q2.5) == sign(Q97.5),
      predictor  = recode(as.character(predictor),
        "Right"    = "Prop. Right Seen",
        "Left"     = "Prop. Left Seen",
        "Friendly" = "Prop. Friendly Seen"
      ),
      predictor  = factor(predictor,
                    levels = c("Prop. Right Seen", "Prop. Left Seen", "Prop. Friendly Seen"))
    )

  d_beliefs <- d_cor %>%
    filter(variable %in% str_remove(change_beliefs, "Change$")) %>%
    mutate(
      variable = str_extract(variable, paste(keywords, collapse = "|")),
      variable = if_else(is.na(variable), "Overall", variable),
      variable = factor(variable, levels = rev(c(keywords, "Overall")))
    )
  p_beliefs <- make_coef_panel(d_beliefs,
    title = "A. Political beliefs", x_label = "< 0 = beliefs more left wing")

  d_consensus <- d_cor %>%
    filter(variable %in% str_remove(change_consensus, "Change$")) %>%
    mutate(
      variable = str_extract(variable, paste(keywords, collapse = "|")),
      variable = if_else(is.na(variable), "Overall", variable),
      variable = factor(variable, levels = rev(c(keywords, "Overall")))
    )
  p_consensus <- make_coef_panel(d_consensus,
    title = "B. Perceptions of consensus", x_label = "< 0 = less belief in consensus")

  trust_vars <- str_remove(change_trust, "Change$")
  if (trust_type == "overall") trust_vars <- "TrustOverall"

  d_trust <- d_cor %>%
    filter(variable %in% trust_vars) %>%
    mutate(variable = str_remove(variable, "^Trust"))
  if (trust_type == "subscales")
    d_trust <- d_trust %>%
      mutate(variable = factor(variable,
               levels = rev(c("Media", "Unis", "Govt", "Business", "People", "Overall"))))

  p_trust <- make_coef_panel(d_trust,
    title   = "C. Overall trust.",
    x_label = "< 0 = less trusting",
    hide_y  = (trust_type == "overall"))

  d_after      <- d_cor %>% filter(variable %in% after_only_questions)
  p_after_list <- lapply(after_only_config, function(cfg)
    make_coef_panel(d_after %>% filter(variable == cfg$var),
                    title = cfg$title, x_label = cfg$x_label, hide_y = TRUE))

  assemble_grid(p_beliefs, p_consensus, p_trust, p_after_list,
                n_belief_vars    = length(levels(d_beliefs$variable)),
                n_consensus_vars = length(levels(d_consensus$variable)),
                n_trust_vars     = if (trust_type == "overall") 1 else 6)
}
