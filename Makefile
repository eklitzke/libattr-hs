LIBROOT = /usr/lib64

Xattr.hs: Xattr.hsc
	hsc2hs -I $(shell ghc --version | awk '{printf "$(LIBROOT)/ghc-%s/include", $$(NF)}') Xattr.hsc

clean:
	-rm -f *.o Xattr.hs

.PHONY: clean
