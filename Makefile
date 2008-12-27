DIRS = src tests

all: force
	-for d in $(DIRS); do (cd $$d; $(MAKE)); done

check: all
	-for d in $(DIRS); do (cd $$d; $(MAKE) check); done

force:
	@true
