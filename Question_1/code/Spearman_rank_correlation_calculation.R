#Spearman rank correlation

calculate_spearman_correlation <- function(Baby_Names) {
    library(dplyr)

    # Function to get top 25 names for each gender and year
    get_top_names <- function(data, year, gender) {
        data %>%
            filter(year == !!year, gender == !!gender) %>%
            top_n(25, wt = count) %>%
            arrange(desc(count))
    }

    # Get top 25 names by year and gender
    top_names_by_year <- Baby_Names %>%
        group_by(year, gender) %>%
        do(get_top_names(., .$year[1], .$gender[1])) %>%
        ungroup()

    # Function to calculate Spearman correlation for a given year and gender
    calculate_spearman <- function(year, gender) {
        current_year_names <- top_names_by_year %>%
            filter(year == !!year, gender == !!gender) %>%
            select(name)

        correlations <- sapply(1:3, function(offset) {
            next_year_names <- top_names_by_year %>%
                filter(year == !!(year + offset), gender == !!gender) %>%
                select(name)

            common_names <- intersect(current_year_names$name, next_year_names$name)

            if (length(common_names) < 2) return(NA)

            current_ranks <- match(common_names, current_year_names$name)
            next_ranks <- match(common_names, next_year_names$name)

            cor(current_ranks, next_ranks, method = "spearman")
        })

        data.frame(
            year = year,
            gender = gender,
            cor_1_year = correlations[1],
            cor_2_year = correlations[2],
            cor_3_year = correlations[3]
        )
    }

    # Apply the function across years and genders
    correlations <- Baby_Names %>%
        filter(year >= 1910 & year <= 2011) %>%
        distinct(year, gender) %>%
        rowwise() %>%
        do(calculate_spearman(.$year, .$gender)) %>%
        ungroup()

    return(correlations)
}