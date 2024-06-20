plot_commitment_data <- function(alloc_plus_commit) {
    library(dplyr)
    library(tidyr)
    library(ggplot2)

    # Transform the data
    commit_long <- alloc_plus_commit %>%
        select(country, eu_member, gdp_in_2021_billion, financial_commitments_billion, humanitarian_commitments_billion, military_commitments_billion) %>%
        gather(key = "commitment_type", value = "amount_billion",
               financial_commitments_billion, humanitarian_commitments_billion, military_commitments_billion)

    # Create the plot
    plot <- commit_long %>%
        filter(country != "EU (Commission and Council)") %>%
        group_by(eu_member) %>%
        ggplot(aes(x = country, y = amount_billion, fill = commitment_type)) +
        geom_bar(stat = "identity", position = "stack") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
            title = "Aid Commitment By Type",
            subtitle = "All countries to the left of Australia are EU members",
            x = "Country",
            y = "Total Bilateral Commitments (Billion)",
            fill = "Commitment Type"
        )

    return(plot)
}