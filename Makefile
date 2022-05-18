svg2pdf:
	inkscape images/*/*.svg --export-area-drawing --batch-process --export-type=pdf
build:
	xelatex main.tex
clean:
	rm *.aux *.lof *.log *.lot *.fls *.out *.toc *.fmt *.fot *.cb *.cb2 *.lb *.xml *.bcf *.synctex.gz *.bbl *.bcf *.blg */*.aux */*.lof */*.log */*.lot */*.fls */*.out */*.toc */*.fmt */*.fot */*.cb */*.cb2 */*.lb */*.xml */*.bcf */*.synctex.gz */*/*.aux */*/*.lof */*/*.log */*/*.lot */*/*.fls */*/*.out */*/*.toc */*/*.fmt */*/*.fot */*/*.cb */*/*.cb2 */*/*.lb */*/*.xml */*/*.bcf */*/*.synctex.gz

