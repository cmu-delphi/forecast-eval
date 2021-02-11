#!/usr/bin/env Rscript

library(readr)

args = commandArgs(trailingOnly=TRUE)

file_name <- args[1]
df <- read_csv(file_name)
saveRDS(df, file = args[2])
