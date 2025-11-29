# Ada 83 LRM Notes

## Chapter 10: Program Structure and Compilation Issues

### 10.1 Compilation Units - Library Units

compilation       ::= {compilation_unit}
compilation_unit  ::= context_clause library_unit | context_clause secondary_unit 
library_unit      ::= subprogram_declaration | package_declaration | generic_declaration
                      | generic_instantiation | subprogram_body 
secondary_unit    ::= library_unit_body | subunit 
library_unit_body ::= subprogram_body | package_body

### 10.2. Subunits of Compilation Units

body_stub ::= subprogram_specification is separate;
              | package body package_simple_name is separate;
              | task body task_simple_name is separate;
subunit ::= separate (parent_unit_name) proper_body

- A *program unit* is either a subprogram, package body, or task body.
- A *subunit* is a *compilation unit* consisting of the *proper body* of a *program unit* found in another *compilation unit*.
    - A corresponding *stub* ending in the keyword 'separate' is present in the other compilation unit.
- Stubs are only allowed immediately within the specification of a library package or a compilation unit's declarative part
  (i.e. no nested subprograms/packages can be 'separate')

### 10.3. Order of Compilation

- Compilation unit must be compiled after all library units named by its context clause.
- A secondary unit that is a subprogram or package body must be compiled after the corresponding library unit.

### 10.5. Elaboration of Library Units

- All library units/their bodies needed by the main program are elaborated before the main program runs
- Elaboration order must follow partial order found in 10.3.
- Library units mentioned in a subunit's context clause must be compiled before the body of the subunit's ancestor
library unit
- pragma Elaborate (library_unit_name) forces library unit body to be elaborated before any other unit can use the
library unit's specification