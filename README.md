Lisp Manipulation of C Structs
==============================

This was part of a presentation at the Lisp meetup in Seattle circa early
2007 as demonstration of using only standard ANSI Common Lisp functions
(thus no need for CFFI, UFFI or implementation-dependent libraries).

A tutorial version of this has been contributed to WikiBooks.  While its
location has
[changed](https://web.archive.org/web/20081002143447/http://en.wikibooks.org/wiki/Programming:Common_Lisp/Files_and_Directories/C_struct)
over the past ten years, it currently resides under
[Advanced Topics](https://en.wikibooks.org/wiki/Common_Lisp/Advanced_topics/Files_and_Directories/C_struct).

This code repo is merely to make the original files available.

Because this predates [Quicklisp](https://www.quicklisp.org/), it uses ASDF
directly and was only tested with [SBCL](http://sbcl.org/) v1.0.2 on
MacOSX 10.4 (Tiger) for PowerPC.

## Original Introduction, circa 2007

Within the scope of systems programming, there are often multiple languages
being used.  While there are verbose file formats for exchanging data among
foreign applications, sometimes you still must deal with packed structures
within "binary" files or network protocols.

This is about processing and generating packed data records such as those
native to C programs.

Specifically, we'll have Lisp reading and writing a C or C++ `struct`.

We'll also process Unix time values within ANSI Common Lisp, accounting for
the different epochs and depending upon hardware architecture, byte order.

## Bit twiddling in ANSI Common Lisp
  
Compare Lisp versus C manipulating 5 bits from a sequence with 7 or more bytes:

**Extract**

ANSI Common Lisp:

	(setf bits (ldb (byte 5 7) value))

C:

	bits = (word >> 7) & 0x1F;

**Modify**

ANSI Common Lisp:

	(setf new-value (dpb bits (byte 5 7) value))

C:

	new_value = (value & (~(0x1F << 7)))|((bits & 0x1F) << 7);

A better visual is in [intro.html](./intro.html)
or [Github CDN version](https://cdn.rawgit.com/dpezely/Lisp-manipulation-of-C-structs/master/intro.html)

## Caveats About Characters

Some Lisp code here uses `character` for easily displaying message buffers
from network sockets, which *accidentally worked* for the presentation but
is **wrong**.

The proper approach would be to instead use `(unsigned-byte 8)` as the type
and convert when printing via `sb-ext:octets-to-string`.  For portability,
consider `utf8-octets-to-string` function in `com.google.base` package
available via Quicklisp.

As was pointed-out during the presentation: Common Lisp characters contain
font information, consume a fairly large footprint in memory, and they are
also unlikely to hold necessary range of values that you may need.  This
limit on values might be unexpected to someone coming from a C programming
background.  The ANSI CL spec of 1994 requires little more than
alphanumerics and control characters be supported, yet contemporary (long
before 2017) implementations such as LispWorks and SBCL accommodate UTF-8.

## About the Lisp gathering

LispSea was restarted in 2007 as a way of giving back to the community for
those learning ANSI Common Lisp.

This was in the era shortly after publication of
[Practical Common Lisp](http://www.gigamonkeys.com/book/),
whereby ANSI Common Lisp was having a bit of a renaissance.

"It's the environment that I wanted while learning but didn't exist at the
time."

An earlier version of the group had previously merged with others to become
SeaFunc: functional programming.  At that time, they typically met for
informal discussions over beers.

With blessings from organizers of SeaFunc, from those on LispSea email list,
and from those who previously ran LispSea-- new meetings were planned.

These meetings were modelled after LispVan-- Vancouver, BC.  A projector was
used during a short presentation that served to seed open discussion and
held in a relatively quiet meeting place.  In our case, it was held at a
community room within a recently updated and expanded library branch.

https://web.archive.org/web/20080517034158/wiki.alu.org/lispsea
