#Generate table of top boy and girls name
generate_top_names_tables <- function(top_25_names) {
    library(dplyr)
    library(gt)
    library(scales)

    # Split into boys' and girls' tables
    top_25_girls <- top_25_names %>%
        filter(gender == "F") %>%
        select(-gender) %>%
        arrange(desc(total_count))

    top_25_boys <- top_25_names %>%
        filter(gender == "M") %>%
        select(-gender) %>%
        arrange(desc(total_count))

    # Create a gt table for girls
    girls_table <- top_25_girls %>%
        gt() %>%
        tab_header(
            title = "Top 25 Most Popular Girls' Names (1910-2014)",
            subtitle = "Based on total count across all US states"
        ) %>%
        cols_label(
            name = "Name",
            total_count = "Total Count"
        ) %>%
        fmt_number(
            columns = vars(total_count),
            sep_mark = ",",
            decimals = 0
        ) %>%
        data_color(
            columns = vars(total_count),
            colors = scales::col_numeric(
                palette = c("white", "pink"),
                domain = NULL
            )
        )

    # Create a gt table for boys
    boys_table <- top_25_boys %>%
        gt() %>%
        tab_header(
            title = "Top 25 Most Popular Boys' Names (1910-2014)",
            subtitle = "Based on total count across all US states"
        ) %>%
        cols_label(
            name = "Name",
            total_count = "Total Count"
        ) %>%
        fmt_number(
            columns = vars(total_count),
            sep_mark = ",",
            decimals = 0
        ) %>%
        data_color(
            columns = vars(total_count),
            colors = scales::col_numeric(
                palette = c("white", "lightblue"),
                domain = NULL
            )
        )

    return(list(girls_table = girls_table, boys_table = boys_table))
}