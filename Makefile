README.md: README.Rmd
	# Instead of just knitr::knit, use rmarkdown::render and remove the .html
	# file, because the latter uses pandoc to process the .md file, and it
	# supports adding a table of contents.
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"
	rm -f README.html
	optipng man/figures/README-*.png

clean:
	rm -f README.md
	rm -f man/figures/README-*.png
