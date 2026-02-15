plot_sankey_transitions <- function(dt_start, dt_stop,
                                    column = "stage",
                                    start_label = "Baseline",
                                    end_label = "10y",
                                    min_flow = 10) {
    library(networkD3)
    library(htmlwidgets)
    library(dplyr)

    if (!column %in% c("stage", "kdigo", "kdigo_risk")) {
        stop("column must be one of: 'stage', 'kdigo', 'kdigo_risk'")
    }

    column_orders <- list(
        stage = c("G1", "G2", "G3", "G3a", "G3b", "G4", "G5"),
        kdigo = c(
            "G1", "G2", "G3", "G3a", "G3b", "G4", "G5",
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
        stage = setNames(set2_colors[1:7], c("G1", "G2", "G3", "G3a", "G3b", "G4", "G5")),
        kdigo_risk = setNames(set2_colors[c(1, 6, 2, 5, 8)], c("Low", "Moderate", "High", "Very High", "Unclassified"))
    )

    value_order <- column_orders[[column]]

    dt_transitions <- dt_start %>%
        select(eid, start_value = !!sym(column)) %>%
        inner_join(
            dt_stop %>% select(eid, end_value = !!sym(column)),
            by = "eid"
        ) %>%
        filter(!is.na(start_value), !is.na(end_value)) %>%
        mutate(
            start_value = as.character(start_value),
            end_value = as.character(end_value)
        ) %>%
        count(start_value, end_value, name = "n") %>%
        filter(n > min_flow)

    # Get all unique values from the actual data
    all_values <- unique(c(dt_transitions$start_value, dt_transitions$end_value))

    # Add any values not in predefined order to the end
    missing_values <- setdiff(all_values, value_order)
    if (length(missing_values) > 0) {
        message(sprintf(
            "Adding %d missing values to order: %s",
            length(missing_values), paste(missing_values, collapse = ", ")
        ))
        value_order <- c(value_order, sort(missing_values))
    }

    start_totals <- dt_transitions %>%
        group_by(start_value) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(pct = 100 * total / sum(total))

    end_totals <- dt_transitions %>%
        group_by(end_value) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(pct = 100 * total / sum(total))

    # Create all nodes with proper ordering
    nodes <- bind_rows(
        tibble(value = all_values, timepoint = start_label) %>%
            left_join(start_totals, by = c("value" = "start_value")) %>%
            filter(!is.na(total)),
        tibble(value = all_values, timepoint = end_label) %>%
            left_join(end_totals, by = c("value" = "end_value")) %>%
            filter(!is.na(total))
    ) %>%
        arrange(match(value, value_order)) %>%
        mutate(
            name = sprintf("%s\nn=%d (%.1f%%)", value, total, pct),
            group = value
        ) %>%
        select(name, group)

    message(sprintf("Created %d nodes", nrow(nodes)))

    links <- dt_transitions %>%
        mutate(
            source_name = sprintf(
                "%s\nn=%d (%.1f%%)",
                start_value,
                start_totals$total[match(start_value, start_totals$start_value)],
                start_totals$pct[match(start_value, start_totals$start_value)]
            ),
            target_name = sprintf(
                "%s\nn=%d (%.1f%%)",
                end_value,
                end_totals$total[match(end_value, end_totals$end_value)],
                end_totals$pct[match(end_value, end_totals$end_value)]
            )
        ) %>%
        mutate(
            source = match(source_name, nodes$name) - 1,
            target = match(target_name, nodes$name) - 1,
            value = n
        ) %>%
        # Filter out links with NA indices (nodes that were filtered out)
        filter(!is.na(source), !is.na(target)) %>%
        select(source, target, value)

    message(sprintf("Created %d links", nrow(links)))

    # Check for invalid indices
    if (any(links$source < 0) || any(links$target < 0)) {
        stop("Negative indices found in links!")
    }
    if (any(links$source >= nrow(nodes)) || any(links$target >= nrow(nodes))) {
        stop("Out of bounds indices found in links!")
    }

    if (column %in% names(color_scales)) {
        color_scale_js <- sprintf(
            "d3.scaleOrdinal().domain([%s]).range([%s])",
            paste0('"', names(color_scales[[column]]), '"', collapse = ","),
            paste0('"', unname(color_scales[[column]]), '"', collapse = ",")
        )
    } else {
        color_scale_js <- "d3.scaleOrdinal(d3.schemeCategory20)"
    }

    sankey_plot <- sankeyNetwork(
        Links = links, Nodes = nodes,
        Source = "source", Target = "target", Value = "value",
        NodeID = "name", NodeGroup = "group", units = "individuals", fontSize = 11,
        nodeWidth = 30, nodePadding = 15, iterations = 0,
        colourScale = color_scale_js
    )

    return(sankey_plot)
}

save_sankey_html <- function(sankey_plot, filepath) {
    dir.create(dirname(filepath), showWarnings = FALSE, recursive = TRUE)
    saveWidget(sankey_plot, filepath, selfcontained = TRUE)
    message(sprintf("Saved HTML: %s", filepath))
    return(filepath)
}
