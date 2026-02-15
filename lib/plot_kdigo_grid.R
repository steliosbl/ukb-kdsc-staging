library(ggplot2)
library(grid)
library(gtable)
library(patchwork)
library(cowplot)

plot_kdigo_heatmap <- function(dt, plot_title = "KDIGO Risk Classification") {
    # Define factor levels for proper ordering
    stage_levels <- c(
        "NA" = "Stage unknown",
        "G5" = "Kidney failure",
        "G4" = "Severely decreased",
        "G3b" = "Moderately to severely decreased",
        "G3a" = "Mildly to moderately decreased",
        "G3" = "Stage 3 (no subtype)",
        "G2" = "Mildly decreased",
        "G1" = "Normal or high"
    )
    stage_gfrs <- c(
        "G1" = "≥90",
        "G2" = "60-89",
        "G3" = "30-59",
        "G3a" = "45-59",
        "G3b" = "30-44",
        "G4" = "15-29",
        "G5" = "<15",
        "NA" = ""
    )

    albuminuria_levels <- c(
        "A1" = "Normal to mildly increased",
        "A2" = "Moderately increased",
        "A3" = "Severely increased",
        "NA" = "Albuminuria unknown"
    )
    albuminria_acrs <- c(
        "A1" = "<30 mg/g",
        "A2" = "30-299 mg/g",
        "A3" = "≥300 mg/g",
        "NA" = ""
    )

    # Create count table with explicit handling of NAs and Unclassified
    total_n <- nrow(dt)

    # Create main count table, mapping Unclassified cases to NA×NA cell
    # For NA×NA cell, we want to show ONLY Unclassified cases
    count_table <- dt %>%
        mutate(
            stage_clean = case_when(
                kdigo == "Unclassified" ~ "NA", # Map Unclassified to NA stage
                is.na(stage) ~ "NA",
                TRUE ~ as.character(stage)
            ),
            albuminuria_clean = case_when(
                kdigo == "Unclassified" ~ "NA", # Map Unclassified to NA albuminuria
                is.na(albuminuria) ~ "NA",
                TRUE ~ as.character(albuminuria)
            )
        ) %>%
        # Filter out non-Unclassified NA×NA cases (we only want Unclassified in that cell)
        filter(!(stage_clean == "NA" & albuminuria_clean == "NA" & kdigo != "Unclassified")) %>%
        count(stage_clean, albuminuria_clean, name = "n") %>%
        # Ensure NA×NA combination exists even if count is 0
        tidyr::complete(
            stage_clean = c("G1", "G2", "G3", "G3a", "G3b", "G4", "G5", "NA"),
            albuminuria_clean = c("A1", "A2", "A3", "NA"),
            fill = list(n = 0)
        ) %>%
        mutate(
            pct = (n / total_n) * 100,
            label = paste0("N=", n, "\n", sprintf("%.1f%%", pct)),
            risk_category = case_when(
                # Unclassified cases in NA×NA cell get dark grey
                stage_clean == "NA" & albuminuria_clean == "NA" ~ "Unclassified",
                # Green - Low risk
                (stage_clean == "G1" & albuminuria_clean == "A1") |
                    (stage_clean == "G2" & albuminuria_clean == "A1") ~ "Low risk",
                # Yellow - Moderately increased risk
                (stage_clean == "G1" & albuminuria_clean == "A2") |
                    (stage_clean == "G2" & albuminuria_clean == "A2") |
                    (stage_clean %in% c("G3", "G3a") & albuminuria_clean == "A1") ~ "Moderately increased risk",
                # Orange - High risk
                (stage_clean == "G1" & albuminuria_clean == "A3") |
                    (stage_clean == "G2" & albuminuria_clean == "A3") |
                    (stage_clean %in% c("G3", "G3a") & albuminuria_clean == "A2") |
                    (stage_clean == "G3b" & albuminuria_clean == "A1") ~ "High risk",
                # Red - Very high risk
                (stage_clean %in% c("G3", "G3a") & albuminuria_clean == "A3") |
                    (stage_clean == "G3b" & albuminuria_clean %in% c("A2", "A3")) |
                    (stage_clean == "G4" & albuminuria_clean %in% c("A1", "A2", "A3")) |
                    (stage_clean == "G5" & albuminuria_clean %in% c("A1", "A2", "A3")) ~ "Very high risk",
                # Grey for other NA combinations (stage NA but albuminuria known, or vice versa)
                stage_clean == "NA" | albuminuria_clean == "NA" ~ "Incomplete data"
            )
        )

    # Factor levels with NA at right for albuminuria
    alb_order <- c("A1", "A2", "A3", "NA")
    stage_order <- rev(c("G1", "G2", "G3", "G3a", "G3b", "G4", "G5", "NA"))

    count_table <- count_table %>%
        mutate(
            stage_clean = factor(stage_clean, levels = stage_order),
            albuminuria_clean = factor(albuminuria_clean, levels = alb_order),
            risk_category = factor(risk_category,
                levels = c(
                    "Low risk", "Moderately increased risk",
                    "High risk", "Very high risk", "Incomplete data", "Unclassified"
                )
            )
        )

    # Define colors
    risk_colors <- c(
        "Low risk" = "#4CAF50", # Green
        "Moderately increased risk" = "#FFEB3B", # Yellow
        "High risk" = "#FF9800", # Orange
        "Very high risk" = "#F44336", # Red
        "Incomplete data" = "#BDBDBD", # Grey
        "Unclassified" = "#757575" # Dark Grey
    )

    # Create albuminuria header plot with 4 rows (including title row)
    alb_header_df <- data.frame(
        albuminuria = c(rep(factor(alb_order, levels = alb_order), 3), rep("A1", 4)),
        row_type = factor(c(rep(c("code", "description", "acr"), each = 4), rep("title", 4)),
            levels = c("acr", "description", "code", "title")
        ),
        label = c(
            alb_order, # Row 2: codes
            albuminuria_levels[alb_order], # Row 3: descriptions
            albuminria_acrs[alb_order], # Row 4: ACR ranges
            c("Albuminuria Categories", "", "", "") # Row 1: title spanning all columns
        ),
        x_pos = c(
            1:4, # Row 2: codes
            1:4, # Row 3: descriptions
            1:4, # Row 4: ACR ranges
            c(2.5, 2.5, 2.5, 2.5) # Row 1: title centered
        ),
        width_val = c(
            rep(1, 12), # Normal rows
            c(4, 0, 0, 0) # Title row: first cell spans all 4 columns
        )
    )

    p_alb_header <- ggplot(alb_header_df %>% filter(width_val > 0), aes(x = x_pos, y = row_type)) +
        geom_tile(aes(width = width_val, fill = row_type), color = "black", linewidth = 0.5) +
        scale_fill_manual(values = c("title" = "#ece9f0", "code" = "#dcdfe8", "description" = "#c4c6d5", "acr" = "#c4c6d5"), guide = "none") +
        geom_text(aes(label = stringr::str_wrap(label, width = 15), fontface = ifelse(row_type == "title", "italic", "bold")),
            size = 3.2, family = "mono", lineheight = 0.85
        ) +
        scale_y_discrete(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0), limits = c(0.5, 4.5)) +
        coord_cartesian(clip = "off") +
        theme_void(base_family = "mono") +
        theme(
            plot.margin = margin(0, 0, 0, 0, "pt"),
            plot.background = element_blank(),
            panel.spacing = unit(0, "pt")
        )

    # Create main heatmap without x-axis labels
    p_main <- ggplot(count_table, aes(x = albuminuria_clean, y = stage_clean)) +
        geom_tile(aes(fill = risk_category), color = "black", linewidth = 0.5) +
        geom_text(aes(label = label), size = 4, family = "mono", fontface = "bold") +
        scale_fill_manual(
            values = risk_colors,
            name = "Risk Category",
            drop = FALSE
        ) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(labels = NULL, expand = c(0, 0)) +
        labs(x = NULL, y = NULL) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 11, base_family = "mono") +
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.spacing = unit(0, "pt"),
            legend.position = "none",
            legend.title = element_text(face = "bold", family = "mono"),
            legend.text = element_text(family = "mono"),
            plot.margin = margin(0, 0, 0, 0, "pt"),
            plot.background = element_blank()
        )

    # Create version with legend for extraction
    p_main_with_legend <- p_main + theme(legend.position = "bottom")

    # Create y-axis label plot with 4 columns (including left title column)
    # First create the regular columns
    stage_df_regular <- expand.grid(
        stage = factor(stage_order, levels = stage_order),
        column_type = factor(c("code", "description", "gfr"), levels = c("code", "description", "gfr"))
    ) %>%
        mutate(
            label = case_when(
                column_type == "code" ~ as.character(stage),
                column_type == "description" ~ stage_levels[as.character(stage)],
                column_type == "gfr" ~ stage_gfrs[as.character(stage)]
            ),
            x_pos = case_when(
                column_type == "code" ~ 0.5,
                column_type == "description" ~ 2.4,
                column_type == "gfr" ~ 4.3
            ),
            width_val = case_when(
                column_type == "code" ~ 0.9,
                column_type == "description" ~ 3.0,
                column_type == "gfr" ~ 0.8
            )
        )

    # Create the single title cell that spans all rows
    stage_df_title <- data.frame(
        stage = factor("G3a", levels = stage_order), # Use middle stage for centering
        column_type = factor("title", levels = c("title", "code", "description", "gfr")),
        label = "GFR Categories",
        x_pos = -0.4,
        width_val = 0.7,
        y_pos = 4.5 # Middle of 8 rows (G1, G2, G3, G3a, G3b, G4, G5, NA)
    )

    stage_df_long <- bind_rows(stage_df_regular, stage_df_title)

    # Add color mapping to regular cells
    stage_df_regular <- stage_df_regular %>%
        mutate(fill_color = case_when(
            column_type == "code" ~ "#c4c6d5",
            column_type == "description" ~ "#dcdfe8",
            column_type == "gfr" ~ "#c4c6d5"
        ))

    p_stage_labels <- ggplot(stage_df_long, aes(x = x_pos, y = stage)) +
        geom_tile(data = stage_df_regular, aes(width = width_val, fill = fill_color), color = "black", linewidth = 0.5, height = 1) +
        scale_fill_identity() +
        # Single spanning tile for title
        geom_tile(data = stage_df_title, aes(width = width_val, y = y_pos), fill = "#ece9f0", color = "black", linewidth = 0.5, height = 8) +
        geom_text(
            data = stage_df_regular, aes(label = label),
            size = 3.5, family = "mono", fontface = "bold", lineheight = 0.9
        ) +
        geom_text(
            data = stage_df_title, aes(label = label, y = y_pos, angle = 90),
            size = 3.5, family = "mono", fontface = "italic"
        ) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.8, 4.8)) +
        scale_y_discrete(expand = c(0, 0)) +
        theme_void(base_family = "mono") +
        theme(
            plot.margin = margin(0, 0, 0, 0, "pt"),
            plot.background = element_blank(),
            panel.spacing = unit(0, "pt")
        )

    # Create a spacer for alignment
    p_spacer <- ggplot() +
        theme_void() +
        theme(plot.margin = margin(0, 0, 0, 0))

    # Combine plots using cowplot for better spacing control
    top_row <- plot_grid(p_spacer, p_alb_header, ncol = 2, rel_widths = c(1.8, 2.2), align = "hv")
    bottom_row <- plot_grid(p_stage_labels, p_main, ncol = 2, rel_widths = c(1.8, 2.2), align = "hv")
    combined_plot_no_title <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1.2, 2.5), align = "hv")

    # Add title and legend
    title_grob <- ggdraw() +
        draw_label(plot_title,
            fontface = "bold", fontfamily = "mono", size = 14, hjust = 0.5
        )

    # Extract legend from version with legend enabled
    legend <- get_legend(p_main_with_legend)

    # Combine with title and legend
    combined_plot <- plot_grid(
        title_grob,
        combined_plot_no_title,
        legend,
        ncol = 1,
        rel_heights = c(0.1, 1, 0.1)
    )

    return(combined_plot)
}
