# Sankey diagram of CKD stage transitions using ggforce parallel sets.
# Replaces the previous networkD3/HTML implementation with a static ggplot.

library(ggforce)
library(cowplot)
library(dplyr)

plot_sankey_transitions <- function(dt_start, dt_stop,
                                    column = "stage",
                                    start_label = "Baseline",
                                    end_label = "10y",
                                    min_flow = 10,
                                    title = NULL,
                                    out_file = NULL) {
    if (!column %in% c("stage", "kdigo", "kdigo_risk")) {
        stop("column must be one of: 'stage', 'kdigo', 'kdigo_risk'")
    }

    # --- Configuration ---
    na_labels <- c(stage = "ACR only", kdigo = "Unclassified", kdigo_risk = "Unclassified")
    na_label <- na_labels[[column]]

    column_orders <- list(
        stage = c("ACR only", "G1", "G2", "G3", "G3a", "G3b", "G4", "G5"),
        kdigo = c(
            "No CKD", "G1", "G2", "G3", "G3a", "G3b", "G4", "G5",
            "G1A1", "G1A2", "G1A3",
            "G2A1", "G2A2", "G2A3",
            "G3A1", "G3A2", "G3A3",
            "G3aA1", "G3aA2", "G3aA3",
            "G3bA1", "G3bA2", "G3bA3",
            "G4A1", "G4A2", "G4A3",
            "G5A1", "G5A2", "G5A3",
            "A1", "A2", "A3", "Unclassified"
        ),
        kdigo_risk = c("Low", "Moderate", "High", "Very High", "Unclassified")
    )

    set2_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
    color_scales <- list(
        stage = setNames(set2_colors[1:8], c("ACR only", "G1", "G2", "G3", "G3a", "G3b", "G4", "G5")),
        kdigo_risk = setNames(set2_colors[c(1, 6, 2, 5, 8)], c("Low", "Moderate", "High", "Very High", "Unclassified"))
    )

    AXIS_W <- 0.12

    value_order <- column_orders[[column]]
    colors_use <- if (column %in% names(color_scales)) color_scales[[column]] else NULL

    # --- Count transitions ---
    dt_trans <- dt_start %>%
        select(eid, start = !!sym(column)) %>%
        inner_join(
            dt_stop %>% select(eid, end = !!sym(column)),
            by = "eid"
        ) %>%
        mutate(
            start = coalesce(start, na_label),
            end = coalesce(end, na_label)
        ) %>%
        count(start, end, name = "n") %>%
        filter(n >= min_flow) %>%
        as.data.frame()

    # Restrict to values present in the data, in canonical order
    present <- intersect(value_order, unique(c(dt_trans$start, dt_trans$end)))
    dt_trans$start <- factor(dt_trans$start, levels = present)
    dt_trans$end <- factor(dt_trans$end, levels = present)
    dt_trans <- dt_trans[!is.na(dt_trans$start) & !is.na(dt_trans$end), ]

    # Rename columns to axis labels for ggforce
    names(dt_trans)[1:2] <- c(start_label, end_label)

    # Compute per-axis counts for labels
    start_counts <- dt_trans %>%
        group_by(cat = .data[[start_label]]) %>%
        summarise(count = sum(n), .groups = "drop") %>%
        mutate(N = sum(count), pct = 100 * count / N, axis_idx = 1)
    end_counts <- dt_trans %>%
        group_by(cat = .data[[end_label]]) %>%
        summarise(count = sum(n), .groups = "drop") %>%
        mutate(N = sum(count), pct = 100 * count / N, axis_idx = 2)
    all_counts <- bind_rows(start_counts, end_counts)

    # Reshape into long format for geom_parallel_sets
    dt_long <- gather_set_data(dt_trans, 1:2)
    dt_long$x <- factor(dt_long$x, levels = c(start_label, end_label))

    # --- Build base plot (ribbons + bars, no labels yet) ---
    p_base <- ggplot(dt_long, aes(x, id = id, split = y, value = n)) +
        geom_parallel_sets(
            aes(fill = !!sym(start_label)),
            alpha = 0.4, sep = 0.005, axis.width = AXIS_W, strength = 0.5, n = 100
        ) +
        geom_parallel_sets_axes(
            sep = 0.005, axis.width = AXIS_W,
            fill = "grey25", colour = "grey25"
        )

    # --- Extract axis bar positions and build label data ---
    axes_raw <- layer_data(p_base, 2)
    bars_df <- axes_raw %>%
        distinct(label, xmin, xmax, ymin, ymax) %>%
        mutate(
            x_mid = (xmin + xmax) / 2,
            y_mid = (ymin + ymax) / 2,
            height = ymax - ymin,
            axis_idx = round(x_mid)
        )

    # Join counts
    bars_df <- bars_df %>%
        left_join(all_counts, by = c("label" = "cat", "axis_idx"))

    total_height <- max(bars_df$ymax) - min(bars_df$ymin)
    bars_df$small <- bars_df$height / total_height < 0.05

    # Labels: inside for large blocks, outside for small
    bars_df$label_inside <- sprintf(
        "%s\nn=%s\n(%.1f%%)",
        bars_df$label,
        formatC(bars_df$count, format = "d", big.mark = ","),
        bars_df$pct
    )
    bars_df$label_outside <- sprintf(
        "%s  n=%s (%.1f%%)",
        bars_df$label,
        formatC(bars_df$count, format = "d", big.mark = ","),
        bars_df$pct
    )

    # Outside labels: left axis -> nudge left, right axis -> nudge right
    nudge <- 0.02
    bars_df$x_outside <- ifelse(
        bars_df$axis_idx == 1,
        bars_df$xmin - nudge,
        bars_df$xmax + nudge
    )
    bars_df$hjust_outside <- ifelse(bars_df$axis_idx == 1, 1, 0)

    large <- bars_df[!bars_df$small, ]
    small <- bars_df[bars_df$small, ]

    # --- Assemble final plot ---
    p <- p_base +
        geom_text(
            data = large,
            aes(x = x_mid, y = y_mid, label = label_inside),
            inherit.aes = FALSE,
            colour = "white", size = 2.8, fontface = "bold", family = "mono",
            lineheight = 0.85
        ) +
        geom_text(
            data = small,
            aes(x = x_outside, y = y_mid, label = label_outside, hjust = hjust_outside),
            inherit.aes = FALSE,
            colour = "grey25", size = 2.5, fontface = "bold", family = "mono"
        ) +
        scale_x_discrete(expand = expansion(mult = 0.15)) +
        coord_cartesian(clip = "off") +
        labs(title = title) +
        theme_cowplot() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "mono"),
            text = element_text(family = "mono"),
            axis.text.x = element_text(size = 11, face = "bold", family = "mono"),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 10, family = "mono"),
            plot.margin = margin(10, 120, 10, 120)
        )

    if (!is.null(colors_use)) {
        p <- p + scale_fill_manual(values = colors_use, name = column)
    }

    # Save as PDF
    if (!is.null(out_file)) {
        dir.create(file.path("data", dirname(out_file)), showWarnings = FALSE, recursive = TRUE)
        ggsave(
            filename = file.path("data", out_file),
            plot = p,
            width = 297, height = 210, units = "mm", device = "pdf"
        )
        message("Saved plot to ", out_file)
        upload_to_dx(file.path("data", out_file), out_file)
    }

    return(p)
}
