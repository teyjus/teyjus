NAME= `date +teyjus-wiki-%Y-%m-%d.zip`

.PHONY: all
all: *.wiki wikify.py
	python wikify.py -e
	rm -f $(NAME)
	zip $(NAME) *.html
	rm -f *.html
	@echo $(NAME) has been created
