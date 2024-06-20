plot_allocation_data <- function(alloc_plus_commit) {
    library(dplyr)
    library(tidyr)
    library(ggplot2)

    # Transform the data
    alloc_long <- alloc_plus_commit %>%
        select(country, eu_member, gdp_in_2021_billion, financial_allocations_billion, humanitarian_allocations_billion, military_allocations_billion) %>%
        gather(key = "allocation_type", value = "amount_billion",
               financial_allocations_billion, humanitarian_allocations_billion, military_allocations_billion)

    # Create the plot
    plot <- alloc_long %>%
        filter(country != "EU (Commission and Council)") %>%
        group_by(eu_member) %>%
        ggplot(aes(x = country, y = amount_billion, fill = allocation_type)) +
        geom_bar(stat = "identity", position = "stack") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
            title = "Aid Allocation By Type",
            subtitle = "All countries to the left of Australia are EU members",
            x = "Country",
            y = "Total Bilateral Allocations (Billion)",
            fill = "Allocation Type"
        )

    return(plot)
}