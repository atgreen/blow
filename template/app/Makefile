APPNAME=blow
# VERSION=`date +%Y%m%d`
VERSION=0.1

clean:
	find ./ -name \*.fasl | xargs rm -f
	find ./ -name \*.abcl | xargs rm -f
	find ./ -name \*class | xargs rm -f
	find ./ -name \*~ | xargs rm -f
	rm -rf dist/*

buildclass:
	(cd WEB-INF/classes; \
	 javac -target 1.5 -cp ../lib/abcl.jar:/usr/share/java/servletapi5.jar org/spindazzle/BlowServlet.java)

dist: clean buildclass
	echo $(abspath .); \
	(cd ..; cp -r $(basename $(PWD)) /tmp/$(APPNAME)-$(VERSION);); \
	mv /tmp/$(APPNAME)-$(VERSION) dist; \
        (cd dist; tar cvfj $(APPNAME)-$(VERSION).tar.bz2 --xattrs $(APPNAME)-$(VERSION); \
	zip -r $(APPNAME)-$(VERSION).zip $(APPNAME)-$(VERSION);)
