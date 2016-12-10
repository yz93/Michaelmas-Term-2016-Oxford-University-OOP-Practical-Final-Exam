all: .compiled

SOURCE = Undoable.scala Text.scala PlaneText.scala Terminal.scala \
	Keymap.scala Display.scala EdBuffer.scala MiniBuffer.scala \
	Editor.scala Testbed.scala

.compiled: $(SOURCE)
	@mkdir -p bin
	$(SCALAC) -d bin $(SOURCE)
	touch $@

# Regression tests
REGRESS = $(wildcard test/test*)

regress: force
	for t in $(REGRESS); do ./runtest $$t; done

TEST = $(wildcard test/Q_*.scala)

test: force
	for t in $(TEST); do scala -cp bin:../lib/scalacheck.jar $$t; done

clean: force
	rm -rf .compiled bin

force:

SCALAC = fsc
