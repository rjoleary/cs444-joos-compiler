Joos Compiler: Final Report
===========================

Summary of Phases
=================

- Scanning
- Weeding
- Environment Building and Type Linking
- Type Checking
- Code Generation

Code Generation Design
======================

- Asm
- CodegenMain vs CodegenType
- Register saving
- Narrowing, widening

Generated Code Layout
=====================

- General overview (what's in .text, .data, etc.)
- Class layout (static fields)
- Object layout (vptr, fields)
- Methods (static, dynamic) and Constructors
- Arrays
- Strings

Performance
===========

Challenges
==========

- Functions that take functions that take functions that return functions
- Performance
