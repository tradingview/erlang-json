# Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
# Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
#
# This file is part of EEP0018, which is released under the MIT
# license.

DIRS = src tests

all: force
	-for d in $(DIRS); do (cd $$d; $(MAKE)); done

check: all
	-for d in $(DIRS); do (cd $$d; $(MAKE) check); done

clean:
	-for d in $(DIRS); do (cd $$d; $(MAKE) clean); done

force:
	@true
