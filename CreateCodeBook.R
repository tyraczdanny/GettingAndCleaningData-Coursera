CreateCodeBook = function(x) {
        result = data.frame(
                Variable_abbreviated_name = names(x),
                Variable_description = read.table("variable_descriptions.txt", sep = "\n", stringsAsFactors = F, header = F)[,1],
                Variable_class = sapply(x, class),
                Variable_range = sapply(x, function(y) 
                        if (class(y) == "integer" || class(y) == "numeric") {
                                paste(min(y), max(y), sep = "  -  ")
                        }
                        else if (class(y) == "factor") {
                                "Walking, WalkingUp, WalkingDown, Sitting, Standing, Laying"
                        }
                ),
                Variable_mean = sapply(x, function(y)
                        if (class(y) == "numeric") {
                                mean(y)
                        }
                        else {
                                "Not available"
                        }
                ),
                row.names = NULL
        )
        write.table(result, "codeBook.md", sep = " | ")
}