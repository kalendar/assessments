parseMarkdown <- function(dir) {
	results <- list()
	mdfiles <- list.files(path = dir, pattern = '*.md', recursive = FALSE)
	for(i in mdfiles) {
		tmp <- markdownToHTML(
			path.expand(paste0(dir, '/', i)), fragment.only = TRUE)
		results[[file_path_sans_ext(basename(i))]] <- gsub('\\\\', '', tmp)
	}
	dirs <- list.dirs(path = dir, recursive = FALSE)
	for(i in dirs) {
		tmp <- parseMarkdown(i)
		if(length(tmp) > 0) {
			results[[file_path_sans_ext(basename(i))]] <- tmp
		}
	}
	return(results)
}
