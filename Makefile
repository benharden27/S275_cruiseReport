all: report

.PHONY: report

report:
	latexmk --pv S275_cruiseReport.tex

word:
	pandoc -s S275_cruiseReport.tex -o S275_cruiseReport.docx

clean:
	latexmk -c S275_cruiseReport.tex

veryclean:
	latexmk -c S275_cruiseReport.tex
	rm S275_cruiseReport.pdf
