#!/usr/bin/env python
# To create test cases for parsing, this is used as the scanner until a real
# scanner is written. This only parses input with whitespace between every
# token.
import sys, re
print "\n".join(["%-8s %-4s %s" % (m.group(), m.start(), m.end())
                for m in re.finditer(r"\S+", open(sys.argv[1]).read())])
