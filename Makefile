DIRS = src tests

all: force
	-for d in $(DIRS); do (cd $$d; $(MAKE)); done

check: all
	-for d in $(DIRS); do (cd $$d; $(MAKE) check); done

clean:
	-for d in $(DIRS); do (cd $$d; $(MAKE) clean); done

force:
	@true
