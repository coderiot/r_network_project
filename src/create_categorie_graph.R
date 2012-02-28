create_category_graph  <- function(ctgry) {
	pages  <- get_pages(ctgry)

	cats_page_list  <- list()

	for(i in 1:length(pages)) {
		cats_page_list[length(cats_page_list)+1]  <- list(get_page_categories(pages[[i]]))
	}

	graph_df <- data.frame(X1=numeric(0), X2=numeric(0))
	for(i in 1:length(cats_page_list)) {
		cats <- cats_page_list[[i]]
		if(length(cats) <= 1) {
			next
		}

		#print(length(cats))
		edge_combs <- combn(unlist(cats), 2)
		edge_df  <- data.frame(matrix(t(edge_combs), ncol=2))
		graph_df  <- merge(graph_df, edge_df, all=T)
	}

	return(graph_df)
	#return(cats_page_list)
}

get_pages <- function(ctgry) {
	cat_url <- paste("http://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmlimit=100&cmtype=page&format=json&cmtitle=Category:", ctgry, sep="")
	pages <- fromJSON(cat_url)$query$categorymembers

	page_list <- list()

	for(i in 1:length(pages)) {
		page_list[length(page_list)+1] <- pages[[i]]$pageid
	}


	return(page_list)
}

get_page_categories  <- function(pg_id) {
	page_cat_url  <- paste("http://en.wikipedia.org/w/api.php?action=query&prop=categories&clshow=!hidden&format=json&pageids=", pg_id, sep="")

	page_cats <- fromJSON(page_cat_url)$query$pages[[1]]$categories

	cat_list  <- list()

	for(i in 1:length(page_cats)) {
		cat_list[length(cat_list)+1] <- unlist(strsplit(page_cats[[i]]$title, ":"))[[2]]
	}

	return(cat_list)
}
