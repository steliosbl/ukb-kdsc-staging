#' Plot covariate distributions by sex
#'
#' Creates violin plots for numerical covariates and bar plots for binary covariates,
#' stratified by sex. Expects data in long format with a 'type' column for variable
#' names and a 'value' column for measurements.
#'
#' @param dt_covariates Data.table in long format with columns: eid, type, value
#' @param dt_cohort Data.table with columns: eid, sex
#' @param binary_vars Character vector of variable types that are binary (0/1 or TRUE/FALSE)
#' @param variable_labels Named vector of display labels for each variable type (optional)
#' @param variable_units Named vector of units for each variable type (optional)
#' @param variable_ranges Named list of ranges for each variable type (optional), each element should be c(min, max)
#' @param plot_title Title for the plot (optional)
#' @param out_file Output file path for PDF (optional)
#' @return Combined ggplot object
plot_covariate_distributions <- function(dt_covariates,
                                         dt_cohort,
                                         binary_vars = c("current_smoker", "lipid_lowering_medication", "hypertension_medication"),
                                         variable_labels = NULL,
                                         variable_units = NULL,
                                         variable_ranges = NULL,
                                         plot_title = "Covariate distributions by sex",
                                         out_file = NULL) {
    library(ggplot2)
    library(cowplot)
    library(dplyr)
    library(tidyr)
    library(data.table)
    library(dtplyr)

    # Convert to wide format for easier processing
    dt_wide <- dt_covariates %>%
        lazy_dt() %>%
        select(eid, type, value) %>%
        pivot_wider(
            names_from = type,
            values_from = value,
            values_fn = function(x) x[1] # In case of duplicates, take first
        ) %>%
        as.data.table()

    # Join with cohort to get sex
    dt_plot <- dt_wide %>%
        lazy_dt() %>%
        inner_join(
            dt_cohort %>% select(eid, sex),
            by = "eid"
        ) %>%
        mutate(sex = ifelse(sex == "Female", "F", "M")) %>%
        as.data.table()

    # Identify numerical vs binary variables
    all_vars <- setdiff(names(dt_plot), c("eid", "sex"))
    numerical_vars <- setdiff(all_vars, binary_vars)

    # Default labels and units if not provided
    if (is.null(variable_labels)) {
        variable_labels <- setNames(all_vars, all_vars)
    }

    if (is.null(variable_units)) {
        variable_units <- setNames(rep("", length(all_vars)), all_vars)
    }

    # Color scheme
    color_male <- "#196FC1"
    color_female <- "#0F2B53"

    # Prepare numerical covariates for plotting
    if (length(numerical_vars) > 0) {
        dt_numerical <- dt_plot %>%
            select(eid, sex, all_of(numerical_vars)) %>%
            pivot_longer(
                cols = all_of(numerical_vars),
                names_to = "variable",
                values_to = "value"
            ) %>%
            filter(!is.na(value)) %>%
            mutate(
                variable_label = sapply(variable, function(v) {
                    label <- ifelse(v %in% names(variable_labels), variable_labels[v], v)
                    unit <- ifelse(v %in% names(variable_units) && variable_units[v] != "",
                        paste0(" (", variable_units[v], ")"), ""
                    )
                    paste0(label, unit)
                })
            ) %>%
            as.data.table()

        # Prepare range data for geom_hline if provided
        if (!is.null(variable_ranges)) {
            dt_ranges <- data.table(
                variable = names(variable_ranges),
                ymin = sapply(variable_ranges, function(x) x[1]),
                ymax = sapply(variable_ranges, function(x) x[2])
            )
            # Add variable labels
            dt_ranges <- dt_ranges %>%
                mutate(
                    variable_label = sapply(variable, function(v) {
                        label <- ifelse(v %in% names(variable_labels), variable_labels[v], v)
                        unit <- ifelse(v %in% names(variable_units) && variable_units[v] != "",
                            paste0(" (", variable_units[v], ")"), ""
                        )
                        paste0(label, unit)
                    })
                )
        }

        # Create violin plots for numerical variables
        p_numerical <- ggplot(dt_numerical, aes(x = sex, y = value, fill = sex)) +
            geom_violin(quantiles = c(0.25, 0.5, 0.75)) +
            facet_wrap(~variable_label, scales = "free_y", nrow = 2) +
            scale_fill_manual(values = c("M" = color_male, "F" = color_female)) +
            labs(x = NULL, y = NULL, fill = "Sex") +
            theme_cowplot(font_family = "mono") +
            theme(
                strip.background = element_rect(fill = "grey90"),
                strip.text = element_text(size = 7, family = "mono"),
                legend.position = "top"
            )

        # Add reference lines if ranges provided
        if (!is.null(variable_ranges)) {
            p_numerical <- p_numerical +
                geom_hline(
                    data = dt_ranges, aes(yintercept = ymin),
                    linetype = "dashed", color = "darkgray", linewidth = 0.3
                ) +
                geom_hline(
                    data = dt_ranges, aes(yintercept = ymax),
                    linetype = "dashed", color = "darkgray", linewidth = 0.3
                )
        }
    } else {
        p_numerical <- NULL
    }

    # Prepare binary covariates for plotting
    if (length(binary_vars) > 0) {
        # Filter to only binary vars that exist in the long-format data
        existing_binary_vars <- unique(dt_covariates$type)
        existing_binary_vars <- intersect(binary_vars, existing_binary_vars)

        if (length(existing_binary_vars) > 0) {
            # Get total size by sex for denominator - based on eids present in dt_covariates
            dt_cohort_counts <- dt_covariates %>%
                lazy_dt() %>%
                distinct(eid) %>%
                inner_join(dt_cohort %>% select(eid, sex), by = "eid") %>%
                mutate(sex = ifelse(sex == "Female", "F", "M")) %>%
                group_by(sex) %>%
                summarise(n_total = n(), .groups = "drop") %>%
                as.data.table()

            # Count unique eids with value=1 for each binary variable, from original long format
            dt_binary_counts <- dt_covariates %>%
                lazy_dt() %>%
                filter(type %in% existing_binary_vars) %>%
                filter(!is.na(value) & (value == 1 | value == TRUE)) %>%
                distinct(eid, type) %>% # Get unique eids per variable type
                inner_join(dt_cohort %>% select(eid, sex), by = "eid") %>%
                mutate(sex = ifelse(sex == "Female", "F", "M")) %>%
                group_by(type, sex) %>%
                summarise(n_with_value = n(), .groups = "drop") %>%
                as.data.table()

            # Add variable labels
            dt_binary_summary <- dt_binary_counts %>%
                mutate(
                    variable_label = sapply(type, function(v) {
                        label <- ifelse(v %in% names(variable_labels), variable_labels[v], v)
                        paste0(label)
                    })
                ) %>%
                left_join(dt_cohort_counts, by = "sex") %>%
                mutate(
                    prop = n_with_value / n_total * 100
                ) %>%
                as.data.table()

            # Create bar plots for binary variables
            p_binary <- ggplot(dt_binary_summary, aes(x = sex, y = prop, fill = sex)) +
                geom_col() +
                geom_text(
                    aes(label = sprintf("%.1f%%", prop)),
                    vjust = -0.5,
                    size = 3,
                    family = "mono"
                ) +
                facet_wrap(~variable_label, nrow = 1) +
                scale_fill_manual(values = c("M" = color_male, "F" = color_female)) +
                scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
                labs(x = NULL, y = "Prevalence (%)", fill = "Sex") +
                theme_cowplot(font_family = "mono") +
                theme(
                    strip.background = element_rect(fill = "grey90"),
                    strip.text = element_text(size = 7, family = "mono"),
                    legend.position = "none"
                )
        } else {
            p_binary <- NULL
        }
    } else {
        p_binary <- NULL
    }

    # Combine plots
    if (!is.null(p_numerical) && !is.null(p_binary)) {
        p_combined <- plot_grid(
            p_numerical,
            p_binary,
            ncol = 1,
            rel_heights = c(2, 1),
            labels = c("A", "B"),
            label_fontfamily = "mono"
        )
    } else if (!is.null(p_numerical)) {
        p_combined <- p_numerical
    } else if (!is.null(p_binary)) {
        p_combined <- p_binary
    } else {
        stop("No variables to plot")
    }

    # Add overall title
    title <- ggdraw() +
        draw_label(
            plot_title,
            fontface = "bold",
            fontfamily = "mono",
            x = 0,
            hjust = 0
        ) +
        theme(
            plot.margin = margin(0, 0, 0, 7)
        )

    p_final <- plot_grid(
        title,
        p_combined,
        ncol = 1,
        rel_heights = c(0.05, 1)
    )

    # Save plot if output file specified
    if (!is.null(out_file)) {
        dir.create(file.path("data", dirname(out_file)), showWarnings = FALSE, recursive = TRUE)
        ggsave(
            filename = file.path("data", out_file),
            plot = p_final,
            width = 297,
            height = 210,
            units = "mm",
            device = "pdf"
        )
        message("Saved plot to ", out_file)
        upload_to_dx(file.path("data", out_file), out_file)
    }

    return(p_final)
}
