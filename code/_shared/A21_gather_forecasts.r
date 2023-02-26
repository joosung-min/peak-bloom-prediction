# Save the final results.

final_result <- data.frame(
    year = 2023:2032
    , kyoto = c(90, 89, 88, 88, 87, 86, 86, 85, 85, 84)
    , liestal = c(86, 88, 87, 87, 86, 86, 86, 85, 85, 85)
    , washingtondc = c(87, 90, 90, 90, 90, 90, 89, 89, 89, 89)
    , vancouver = c(89, 88, 88, 87, 87, 87, 86, 86, 86, 85)
)

write.csv(final_result, "./final_result.csv", row.names = FALSE)
