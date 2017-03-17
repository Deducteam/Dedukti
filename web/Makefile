
.SUFFIXES:

.PHONY: install clean

manual_v2_5.html: manual_v2_5.md
	cp manual_header.html $@
	markdown $< >> $@
	cat manual_footer.html >> $@

install: manual_v2_5.html
	scp -r . scm.gforge.inria.fr:/home/groups/dedukti/htdocs

clean:
	rm -f *~
