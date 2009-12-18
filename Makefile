VERSION=0.3

SOURCES = blow blow.spec ChangeLog lib Makefile README template

dist:
	rm -rf blow-$(VERSION)
	mkdir blow-$(VERSION)
	cp -r $(SOURCES) blow-$(VERSION)
	tar cvfz blow-$(VERSION).tar.gz blow-$(VERSION)
	rm -rf blow-$(VERSION)
