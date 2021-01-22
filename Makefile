talk.pdf: talk.Rmd
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"
