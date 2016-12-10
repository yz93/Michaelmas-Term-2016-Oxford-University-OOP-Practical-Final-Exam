all: .compiled

SOURCE := $(wildcard *.scala)

.compiled: $(SOURCE)
	@mkdir -p bin
	$(SCALAC) -d bin $(SOURCE)
	touch $@

clean: force
	rm -rf .compiled bin

force:

SCALAC = scalac
