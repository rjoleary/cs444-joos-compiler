Assignment 2
============

This is a list of the rules we need to check and ideas for how to go
about checking them using our AST.

### Notes

* It might make sense to implement `Eq` where we need to do duplicate
  checking

* It might make sense to define utility functions like `isStatic`,
  `isAbstract` that check for the existance of a modifier. (Probably
  simplified by declaring a typeclass `Modifiable`).

### Environment Building

 The environment building stage creates environments (containing
 classes, interfaces, fields, methods, local variables, and formal
 parameters) for each scope. Given a name of one of these entities,
 the environment should be able to locate the correct declaration of
 the entity.

* No two fields declared in the same class may have the same name.

  Get class from compilation unit. Iterate through classFields,
  checking for duplicate name

* No two local variables with overlapping scope have the same name.

  Get all blocks from AST (each block has a scope).  Go through the
  variable names in each scope and check that rhere are no
  conflicts. We need to check for duplicates in current scope and
  parent scope

* No two classes or interfaces have the same canonical name.

  Iterate through CompilationUnits, checking for duplicate classname

### Type Linking

 The type linking stage connects each use of a named type (class or
 interface) to the declaration of the type. At this stage, only names
 that can be syntactically (according to JLS 6.5.1) determined to be
 names of types need to be linked. Some names are syntactically
 ambiguous, in the sense that type checking must be done before it can
 be determined whether they are names of type or of other entities
 (see JLS 6.5). These ambiguous names will be linked in a later
 assignment.

* No single-type-import declaration clashes with the class or
  interface declared in the same file.

  Get imports, classes from CompilationUnit and check for clashes.

* No two single-type-import declarations clash with each other.

  Get imports from CompilationUnit and check for duplicates

* All type names must resolve to some class or interface declared in
  some file listed on the Joos command line.

  Get children satisfying `isType` from CompilationUnit. If it's a
  primitive type (or void), it's ok. If it's a `NamedType`, check that
  the name occurs as a class declaration.

* When a fully qualified name resolves to a type, no strict prefix of
  the fully qualified name can resolve to a type in the same
  environment.

  Similar to the previous item, but instead of checking that the name
  occurs, check that no class declaration is a strict prefix of name
  name.

* All simple type names must resolve to a unique class or interface.

  TODO

* No package names or prefixes of package names of declared packages,
  single-type-import declarations or import-on-demand declarations
  that are used may resolve to types, except for types in the default
  package.

  TODO

* Every import-on-demand declaration must refer to a package declared
  in some file listed on the Joos command line. That is, the
  import-on-demand declaration must refer to a package whose name
  appears as the package declaration in some source file, or whose
  name is a prefix of the name appearing in some package declaration.

  TODO

### Hierarchy Checking

The third stage computes the inheritance relationships for classes,
interfaces, methods, and fields, and checks that they conform to the
various rules given in Chapters 8 and 9 of the Java Language
Specification. Specifically, this stage should check that:

* A class must not extend an interface. (JLS 8.1.3, dOvs simple
  constraint 1)

  Get class declarations from `CompilationUnit`s. Check that none of
  them has a `super` that is an interface

* A class must not implement a class. (JLS 8.1.4, dOvs simple
  constraint 2)

  Get class declarations from `CompilationUnit`s. Check that none of
  the members in `interface` is a (non-interface) class


* An interface must not be repeated in an implements clause, or in an
  extends clause of an interface. (JLS 8.1.4, dOvs simple constraint
  3)

  First part: Get class declarations. Check that `interface` contains
  no duplicates

  Second part: TODO

* A class must not extend a final class. (JLS 8.1.1.2, 8.1.3, dOvs
  simple constraint 4)

  Get class declarations. Check that all `super` classes don't have
  `Final` as a modifier. It might makes sense to define utility
  functions like `isFinal` for classes.

* An interface must not extend a class. (JLS 9.1.2)

  Get class declarations (which are the same as interface
  declarations). Filter using `isInterface`. Check that `super` is an
  interface for all filtered declarations.

* The hierarchy must be acyclic. (JLS 8.1.3, 9.1.2, dOvs
  well-formedness constraint 1)

  TODO

* A class or interface must not declare two methods with the same
  signature (name and parameter types). (JLS 8.4, 9.4, dOvs
  well-formedness constraint 2)

  Get class declarations. Go through their methods, checking for
  duplicates signature (name, `formalParameters`). It might make sense
  to implement the `Eq` typeclass for types for this check.

* A class must not declare two constructors with the same parameter
  types (dOvs 8.8.2, simple constraint 5)

  Get class declarations. Go through their constructors, checking for
  duplicate formalParameters.

* A class or interface must not contain (declare or inherit) two
  methods with the same signature but different return types (JLS
  8.1.1.1, 8.4, 8.4.2, 8.4.6.3, 8.4.6.4, 9.2, 9.4.1, dOvs
  well-formedness constraint 3)

  Get class declarations. Go through their methods, checking for
  same signature (name, `formalParameters`) and different type.

* A class that contains (declares or inherits) any abstract methods
  must be abstract. (JLS 8.1.1.1, well-formedness constraint 4)

  Get class declarations. Check for methods with the `abstract`
  modifier. If any exist, check that the class is abstract.

* A nonstatic method must not replace a static method (JLS 8.4.6.1,
  dOvs well-formedness constraint 5)

  TODO -- is this about `implements` or `extends`?

* A method must not replace a method with a different return
  type. (JLS 8.1.1.1, 8.4, 8.4.2, 8.4.6.3, 8.4.6.4, 9.2, 9.4.1, dOvs
  well-formedness constraint 6)

  TODO -- same as above

* A protected method must not replace a public method. (JLS 8.4.6.3,
  dOvs well-formedness constraint 7)


  TODO -- same as above


* A method must not replace a final method. (JLS 8.4.3.3, dOvs
  well-formedness constraint 9)

  TODO -- same as above
