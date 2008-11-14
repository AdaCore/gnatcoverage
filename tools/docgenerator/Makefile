all: clean
	./obj/generate_doc.exe requirementFiles.txt targetFiles.txt > tor.html
	/Program\ Files/Mozilla\ Firefox/firefox.exe tor.html
	
clean:
	rm *.html