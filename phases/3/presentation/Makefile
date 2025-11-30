INPUT = slides
ORG   = $(INPUT).org
PDF   = $(INPUT).pdf

all: $(PDF)

$(PDF): $(ORG)
	emacs -batch $(ORG) --funcall org-babel-tangle
	emacs -batch $(ORG) \
	  --eval "(setq org-latex-listings 'minted \
	                     org-latex-packages-alist '((\"\" \"minted\")) \
	                     org-latex-pdf-process '(\"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f\" \
	                                              \"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f\"))" \
	  --eval "(setq org-latex-minted-options '((\"frame\" \"lines\") (\"fontsize\" \"\\\\scriptsize\")))" \
	  --funcall org-beamer-export-to-pdf

clean:
	rm -f $(PDF) *.aux *.log *.nav *.out *.snm *.toc *.vrb *.tex
