# Test remove_wav_files in LYS
# Update date : Nov. 13, 2026

devtools::load_all(".")

cat("=== Testing remove_wav_files ===\n")
lys <- readRDS(file.path("dev", "lys_G769.rds"))
n_init <- nrow(lys$metadata)

# 1. Remove by filename
target_fn <- lys$metadata$filename[1]
lys1 <- remove_wav_files(lys, files = target_fn, verbose = FALSE)
stopifnot(nrow(lys1$metadata) == n_init - 1)
stopifnot(!target_fn %in% lys1$metadata$filename)
stopifnot(!is.null(lys1$misc$removed_files))
stopifnot(nrow(lys1$misc$removed_files) == 1)

# 2. Remove by full path
target_path <- lys$metadata$file_path[2]
lys2 <- remove_wav_files(lys, files = target_path, verbose = FALSE)
stopifnot(nrow(lys2$metadata) == n_init - 1)
stopifnot(!target_path %in% lys2$metadata$file_path)

# 3. Remove by index
lys3 <- remove_wav_files(lys, files = c(3, 4), verbose = FALSE)
stopifnot(nrow(lys3$metadata) == n_init - 2)

# 4. Remove by min_size_bytes
lys_size <- remove_wav_files(lys, min_size_bytes = 200000, verbose = FALSE)
stopifnot(all(lys_size$metadata$file_size_bytes >= 200000))

# 5. Downstream vocalization pruning
target_fn_with_voc <- lys$metadata$filename[1] # file 1 has vocalizations
n_voc_init <- nrow(lys$vocalizations)
lys_voc <- remove_wav_files(lys, files = target_fn_with_voc, verbose = FALSE)
stopifnot(nrow(lys_voc$vocalizations) < n_voc_init)
stopifnot(!target_fn_with_voc %in% lys_voc$vocalizations$filename)

cat("All tests passed for remove_wav_files!\n")
