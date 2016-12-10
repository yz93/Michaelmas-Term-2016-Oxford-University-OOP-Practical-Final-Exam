SOURCE := $(wildcard *.scala)

all: .compiled

.compiled: $(SOURCE)
	@mkdir -p bin
	scalac -d bin $^
	cp mapinfo.txt erewhon.txt bin
	touch $@

profile: psrc .pcompiled

psrc:
	@mkdir -p psrc

.pcompiled: $(SOURCE:%=psrc/%) psrc/PMP.scala
	@mkdir -p pbin
	scalac -d pbin $^
	cp mapinfo.txt erewhon.txt pbin
	touch $@

psrc/PMP.scala: pmp/PMP.scala
	cp $< $@

psrc/%: %
	tclsh pmp/prep.tcl $< >$@

TEST = $(wildcard test/Q_*.scala)

test: .tcompiled force
	for t in $(TEST); do scala -cp bin:../lib/scalacheck.jar $$t; done

.tcompiled: .compiled test/T_PriQueue.scala
	scalac -cp bin:../lib/scalacheck.jar -d bin test/T_PriQueue.scala
	touch $@

clean: force
	rm -rf .compiled bin .pcompiled psrc pbin pmp.out .tcompiled

force:
