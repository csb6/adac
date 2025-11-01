------------------------------------------------------------------------------
-- Ada interface for the IEEE Standard for Binary Floating-Point Arithmetic --
------------------------------------------------------------------------------
--                          ANSI/IEEE Std 754-1985                          --
------------------------------------------------------------------------------
--                            Robert B. K. Dewar                            --
------------------------------------------------------------------------------
--                         Version 5  (05 Jan 1992)                         --
------------------------------------------------------------------------------

-- This specification is an initial proposal, suggested by Robert B. K. Dewar
-- and modified in response to suggestions received. Comments or suggestions
-- are welcome, and should be transmitted by EMAIL to dewar@nyu.edu, or by
-- mail to Robert Dewar, 73 5th Ave, NYC NY 10003. The proposal currently has
-- no official status, but the intention is that it eventually receive some
-- kind of official status if a sufficiently broad positive consensus is
-- achieved. Note: this document is formatted to print using 60 lines per
-- page and the maximum line length is 79 characters.


			    ----------------------
			    -- Revision History --
			    ----------------------

-- Changes made in Version 5 (05 Jan 1992)

   -- The facility for trapping errors and returning control has been made
   -- optional, since it is an optional feature of P754, and is not always
   -- implemented on IEEE conforming processors (due to difficulties in
   -- accomodating precise traps in pipelined machines).

   -- MACHINE_OVERFLOWS may now be FALSE. If it is FALSE, then exceptions
   -- are never generated on overflow (instead operation is always in the
   -- mode where infinities are generated and recognized).

   -- The PRECISION_CONTROL pragma is replaced by FLOAT_EVALUATE, with three
   -- possible settings, following the suggestions in the paper by Brian
   -- Wichman on floating-point evaluation schemes (ref.)

   -- A package IEEE_P754_PRECISION_CONTROL is provided for dynamic control
   -- over precision of results, as required in section 4.3 of P754. In
   -- previous versions, the (incorrect) claim was made that the static
   -- pragma controlling precision of intermediate results satisfies the
   -- requirements of section 4.3 of the standard.

   -- A number of the standard functions have been removed, on the grounds
   -- that they are provided in GEF. A full implementation of GEF is now
   -- required as part of the basic compatibility requirements.

-- Changes made in Version 4 (10 Aug 1989)

   -- The name INTERMEDIATE_ROUNDING has been changed to PRECISION_CONTROL,
   -- which more clearly reflects its function and also reflects the language
   -- of the standard. The effect of this pragma now terminates at scope exit,
   -- so that, for example, precision control pragmas within a procedure body
   -- cannot affect statements outside the procedure.

   -- Some errors in the comparison result table in section 5.7 have been
   -- corrected (the equal result column entries for LT,LE,LG were wrong).

   -- Documentation has been added to section 0.3 to make clear that the
   -- compiler is forced to store and retrieve variable values using the
   -- declared precision of the variable, and that optimizations which
   -- retain higher precision values in registers and violate this rule
   -- are not permitted.

   -- The text has been reedited to 78 characters/line format, for convenient
   -- use with versions of EMACS, this format should be maintained in future.

-- Changes made in Version 3 (28 May 1989)

   -- Section 0.4 has been modified to discuss the issue of values passed
   -- to and returned from functions. It is permissible for the computation
   -- and passing of argument values, and the return of function results to
   -- retain extended precision.

   -- Section 0.6 now requires accurate conversion of extended precision
   -- literals. A requirement is also added that literals be converted with
   -- a precision corresponding to the accuracy of the operation performed.
   -- There is also a requirement that compile time operations give identical
   -- results to the corresponding execution time operations.

   -- The definition of the type CLASS_TYPE is modified to be more consistent.

   -- RESULT_EXPONENT returns a float result, rather than INTEGER, to avoid
   -- the specific use of the implementation dependent INTEGER type.

-- Changes made in Version 2 (22 May 1989)

   -- A section 0 has been added specifying certain minimum requirements for
   -- an IEEE/P754 compatible Ada environment. This set of specifications
   -- stands alone as a useful partial implementation of the standard, and
   -- also means that the package itself can be simplified because it can
   -- make greater use of the underlying implementation environment. Problems
   -- with the use of predefined operations in generics are also handled by
   -- this approach, since basic operations like addition are now handled
   -- properly by the predefined operations, rather than operations defined
   -- in the package.

   -- The package providing the additional P754 specific operations is now a
   -- generic package, to be instantiated with the type or derived type for
   -- which the operations are required.

   -- The terminology has been clarified by substituting the term fault
   -- for exception, to avoid confusion with Ada exceptions.

   -- A number of these changes reflect Brian Wichmans comments on version 1.


                 ---------------------------------------------
		 -- 0. IEEE/P754 Compatibility Requirements --
		 ---------------------------------------------

-- The specifications in this file provide an interface from Ada programs to
-- all the required facilities of ANSI/IEEE standard 754-1985, hereinafter
-- referred to simply as P754. A complete implementation of these packages
-- results in an environment which fully conforms to the P754 requirements.

-- The packages assume that they are hosted in a P754 compatible Ada compiler.
-- P754 compatibility involves a set of requirements on the implementation of
-- standard Ada features that are compatible with, but go considerably beyond
-- the requirements of the Ada standard. The Ada standard deliberately allows
-- implementation freedoms in the area of floating point operations, in order
-- to accommodate a wide variety of hardware floating point models. The P754
-- compatibility rules specify exact semantics in areas where the Ada standard
-- leaves such freedoms. The requirements are not in any way incompatible with
-- the requirements of the standard, so that a P754 compatible Ada environment
-- is in every way a conforming Ada requirement.

-- 0.1  Provision of Standard Types

   -- STANDARD must define the base types FLOAT and LONG_FLOAT, which are
   -- respectively correct implementations of the basic single and double
   -- formats defined in section 3.2 of P754. The following attribute values
   -- apply to these types:

      --    Attribute              FLOAT              LONG_FLOAT
      --     DIGITS                 6                  15
      --     EMAX                   84                 204
      --     EPSILON                9.53674E-07        8.88178E-16
      --     FIRST                 -3.40282E+38       -1.79769E+308
      --     LARGE                  1.93428E+25        2.57110E+61
      --     LAST                   3.40282E+38        1.79769E+308
      --     MANTISSA               21                 51
      --     SAFE_EMAX              125                1021
      --     SAFE_SMALL             1.17549E-38        2.2250EE-308
      --     MACHINE_MANTISSA       24                 53
      --     MACHINE_RADIX          2                  2
      --     MACHINE_EMAX           128                1024
      --     MACHINE_EMIN          -125               -1021
      --     MACHINE_OVERFLOWS      (see below)        (see below)
      --     MACHINE_ROUNDS         TRUE               TRUE
      --     SIZE                   32                 64

   -- Note: MACHINE_OVERFLOWS is set to TRUE if floating-point overflows
   -- result in raising an exception. If MACHINE_OVERFLOWS is false, then
   -- operation is always in the generate-infinities mode, and no exceptions
   -- occur on overflow. This mode of operation is provided to allow for
   -- implementation on pipelined machines that do not accomodate traps.

   -- In addition, a type EXTENDED_FLOAT must be provided, representing the
   -- highest precision available. If the underlying P754 implementation
   -- provides double extended format, as described in section 3.3, then
   -- EXTENDED_FLOAT must correspond to this type. If the underlying P754
   -- implementation does NOT provide this format, then EXTENDED_FLOAT may
   -- be equivalent in accuracy and format to LONG_FLOAT. The attributes
   -- for EXTENDED_FLOAT indicate the actual accuracy and other
   -- characteristics of this type.

-- 0.2  Predefined Operations

   -- The predefined operations (+,-,*,/) applied to these types must conform
   -- to the definitions in P754, with the default behavior (i.e. the behavior
   -- in the absence of the use of the IEEE_P754_ROUNDING_CONTROL package)
   -- being "round to nearest" (see section 4.1). Overflow, divide by zero
   -- and invalid operation faults raise the exception CONSTRAINT_ERROR.
   -- Underflow and inexact faults are disabled (see section 7 of P754 for
   -- further details).

   -- Conversion operations between floating point types, and between integer
   -- types and floating point types must be performed in accordance with the
   -- P754 rules, including round to nearest semantics.

-- 0.3  Accuracy of Predefined Operations

   -- In the absence of the use of the pragmas described in section 0.4, it is
   -- permissible for the calculation of intermediate results to be done with
   -- greater precision than that implied by the result type (for example by
   -- using EXTENDED_FLOAT for intermediate results). However, an explicit
   -- assignment operation requires that the result be rounded as appropriate
   -- to the precision type of the assignment target. Subsequent references to
   -- the variable must yield a copy of the stored value. It is specifically
   -- NOT permissible for an optimizer to use a higher precision value held in
   -- a register. For example, suppose we have the sequence

      -- A := B * C * D;
      -- Q := A + C;

   -- where all variables are of type FLOAT in an environment where all
   -- intermediate results are computed in double extended. It is not
   -- permissble to compute B*C*D in double extended, store a rounded single
   -- value in A, and then use the double extended value still sitting in a
   -- register for the subsequent addition to C.

   -- In deciding to adopt this approach, it is understood that in hardware
   -- environments where intermediate results are naturally computed in
   -- extended precision, this requirement may cause "inefficient" code
   -- sequences. However, it seems fundamental to the approach that floating
   -- point arithmetic should behave in a consistent and predictable manner.
   -- Allowing the compiler to do optimizations which affect the results in
   -- a non-predictable manner is unacceptable as the default behavior.

   -- For function and procedure calls, the copy in of the argument value is
   -- not considered to be an explicit assignment. If the argument is a
   -- floating point expression which is computed using extended precision,
   -- then this extended precision may be preserved in passing values to
   -- the procedure. Similarly, functions may return results using a return
   -- statement whose expression is computed using extended precision and
   -- this extended precision value may be used at the point of call.

   -- A requirement for this default mode (which corresponds to the setting
   -- pragma FLOAT_EVALUATE (PREDICTABLE)), it is mandatory that complete
   -- documentation be provided indicating exactly when extended precision
   -- is used, so that the behavior is completely predictable from the source.

-- 0.4  Control over Intermediate Result Precision

   -- The following pragmas must be provided to control the precision of
   -- intermediate results, as required by the standard.
      -- pragma FLOAT_EVALUATE (PORTABLE);

	-- In the PORTABLE mode, all intermediate computations are required
	-- to yield results with the precision and range of their returned
	-- types. This means that if EXTENDED_FLOAT is not used, and overflow
	-- is avoided, then precisely identical results are obtained on all
	-- implementations. For example, in the statement:

	   -- A := (B + C) + D;

	-- where all variables are of type FLOAT, the intermediate result B + C
	-- must be rounded to FLOAT precision, even in an implementation which
	-- more efficiently uses extended precision for all operations. In the
	-- case of functions, arguments and returned results must also be
	-- converted to the appropriate type.

      -- pragma FLOAT_EVALUATE (PREDICTABLE);

	-- In PREDICTABLE mode, explicit assignments must round to the target
	-- type, but intermediate results in expressions can be computed using
	-- extended precision, providing that such usage is documented in com-
	-- plete detail, so that the behavior is completely predictable.  Note
	-- that in this mode, passing arguments to subprograms, or returning
	-- results from functions is not considered to be an assignment, so
	-- that extended precision can also be used in these contexts.

      -- pragma FLOAT_EVALUATE (OPTIMIZE);

	-- In OPTIMIZE mode, extended precision may be used anywhere, including
	-- the case of assignments to variables of type FLOAT or LONG_FLOAT.
	-- The general principle is that out of range results are permissible
	-- providing the "correct" results are generated.

	-- In this mode, it is also not required that the places that use is
	-- made of extended precision need not be exactly documented. For
	-- example, if it is the case that the use of extended precision
	-- depends on unpredictable details of the register allocation
	-- algorithm, then this behavior is acceptable in the OPTIMIZE
	-- mode, but not in any of the other modes.
	-- result types, as described in section 0.3.

   -- The default mode in the absence of either form of this pragma is
   -- FLOAT_EVALUATE(PREDICTABLE), so that intermediate results may be
   -- calculated using extra precision, but assignments must be to the
   -- proper type, and all use of extended precision must be documented.

   -- The appearence of the FLOAT_EVALUATE pragma affects all subsequent
   -- computations which textually follow the occurence of the pragma and
   -- remains in effect until another FLOAT_EVALUATE pragma appears or
   -- until the end of the current scope is reached. At the end of a scope,
   -- the setting in effect on entry to the scope is restored. This means
   -- for example that precision control pragmas within a procedure body
   -- can never affect statements outside the procedure. The pragma may
   -- appear in the context clause for a compilation unit, in which case
   -- it determines the default behaviour for the entire compilation unit.

   -- Note: an implementation must implement FLOAT_EVALUATE (PORTABLE) mode.
   -- The implementation of the other modes is optional, since it is permitted
   -- to ignore the additional freedoms of the additional modes, and treat
   -- all three modes in identical manner (i.e. identical to PORTABLE mode).
-- 0.5  Comparison Operations

   -- The predefined comparison operations correspond to the semantics of the
   -- first six predicates listed in Table 4 of P754. In the absence of the
   -- use of the IEEE_P754_FAULT_CONTROL package, infinite and NaN values
   -- will not occur, so the semantics of these operations are consistent with
   -- normal Ada semantics.

-- 0.6  Accuracy of Conversion of Literals

   -- Floating point literals for basic format values must be accurately
   -- converted using round to nearest throughout the entire range. This
   -- is a more stringent requirement than that specified by P754, see
   -- section 5.6.1 of this specification. The accuracy of conversion of
   -- extended format literals is implementation dependent.

   -- When operations are carried out using a precision greater than that
   -- implied by the Ada type, literals must be converted in accordance with
   -- the actual precision used. For example consider the statement:

      -- A := B * 3.145 / 4.189

   -- where A and B are both of type FLOAT. If the implementation carries out
   -- the multiplication and division in extended precision (possible only if
   -- PRECISION_CONTROL mode is OFF), then the two literals must be converted
   -- with the accuracy of EXTENDED_FLOAT, even though they are of type FLOAT
   -- by Ada rules. Named numbers are treated similarly.

   -- Note: any arithmetic carried out at compile time (e.g. folding of
   -- constant expressions) must give absolutely identical results to the
   -- corresponding operations performed at execution time.

-- 0.7  Input and Output of Floating Point Values

   -- The FLOAT_IO package must input basic format values (FLOAT, LONG_FLOAT)
   -- in a manner that conforms to the error bounds described in section 5.6
   -- of P754. Where possible, exact rounding should be provided. The
   -- accuracy of input conversion for EXTENDED_FLOAT values is
   -- implementation dependent.

-- 0.8  Implementation of GEF

   -- A conforming implementation must provide a completely implementation of
   -- the GENERIC_ELEMENTARY_FUNCTIONS package (defined in ISO standard .....)

-- 0.9  Use of a P754 Compatible Ada Compiler

   -- The provision of P754 compatibility as defined in this section is useful
   -- even if an Ada implementation does not fully support the packages
   -- defined in the remainder of this specification. For many purposes, this
   -- basic level of support will prove adequate for IEEE/P754 applications.

   -- Implementations for IEEE/P754 compatible targets are encouraged to
   -- conform to these compatibility requirements to the greatest extent
   -- possible, even if the additionally specified packages are not provided.


                                --------------
				-- 1. Scope --
				--------------

-- Note: The section numbers in the remainder of this file match those
-- used in the ANSI/IEEE Std 754-1985 document, except for a few cases of
-- additional sections (e.g. 5.6.1) which are included for clarification.

-- 1.1  Implementation Objectives

   -- ANSI/IEEE Std 754-1985 specifies a hardware and software standard for
   -- floating point arithmetic. The compatibility requirements described in
   -- the previous section provide the basic P754 compatible environment, but
   -- a full implementation of the standard requires additional facilities.
   -- These facilities are provided with a set of four packages:

      -- IEEE_P754_ROUNDING_CONTROL   Control over rounding mode
      -- IEEE_P754_OPERATIONS         Additional required operations
      -- IEEE_P754_IO                 Input-output and string conversions
      -- IEEE_P754_FAULT_CONTROL      Control over exceptions and traps

-- 1.2  Inclusions

   -- All aspects of the standard, as listed in this section, are included
   -- in these packages and specified in a standard manner.

-- 1.3  Exclusions

   -- P754 does not specify the formats of decimal strings and integers, or
   -- the interpretation of the sign and significand fields of NaN's, or
   -- binary to decimal conversions to and from extended formats.

   -- An implementation of this interface must include the specification of
   -- the following implementation dependent characteristics:

      -- The accuracy and representation of the type EXTENDED_FLOAT,

      -- The accuracy and representation of any additional FLOAT types,

      -- The accuracy of conversion of input values for all FLOAT types,

      -- The representation of all NaN values supported, including both the
      -- binary representation, and the representation as string values for
      -- input-output (using IEEE_P754_IO) and the IMAGE and VALUE functions,

      -- The manner in which NaN values are produced and recognized,

      -- The manner in which intermediate results are computed with extended
      -- precision if PRECISION_CONTROL mode is on,

      -- Whether or not the functions COPYSIGN, FINITE, ISNAN, and the
      -- predefined unary negation operation signal an invalid operation fault
      -- if a signalling NaN is used as an operand value,

      -- The order of bits used in the representation of the float formats.


                             --------------------
			     -- 2. Definitions --
			     --------------------

   -- The terminology in this specification generally matches that of section
   -- 2 of P754. The one point at which a deliberate departure from the
   -- defined terminology occurs is that the word "fault" is used instead of
   -- "exception" to avoid an otherwise inevitable confusion with the Ada use
   -- of the term exception.


			       ----------------
			       -- 3. Formats --
			       ----------------

-- 3.1  Sets of Values

   -- The sets of values provided shall correspond to those defined in section
   -- 3.1 in all respects. The generation of infinities and NaN's is possible
   -- if the facilities of IEEE_P754_FAULT_CONTROL are used to suppress
   -- raising of exceptions or traps on overflow or other faults.

-- 3.2  Basic Formats

   -- The required P754 basic computational formats are supplied in package
   -- STANDARD as FLOAT and LONG_FLOAT (see section 0.1).

-- 3.3  Extended Formats

   -- If double extended format is provided by the underlying implementation,
   -- then the type EXTENDED_FLOAT in STANDARD must correspond exactly to the
   -- implementation of this format, and P754 compatible operations are to be
   -- made available for this type. In some implementations, this extended
   -- precision format will automatically be used for all intermediate
   -- computations, but this behavior is not required (and can be suppressed,
   -- see section 0.4).

   -- Programs using EXTENDED_FLOAT are potentially implementation dependent.
   -- One portable case involves programs where the extra precision is either
   -- harmless or beneficial. Programs which depend on the precision of the
   -- extended float type must either be parametrized with respect to the
   -- values of the Ada attributes revealing the precision, or must be
   -- understood to be implementation dependent.

   -- Note: P754 allows for the implementation of an additional representation
   -- "single extended format", which presumably has an intermediate precision
   -- between FLOAT and DOUBLE_FLOAT. Very few implementations of P754 include
   -- this format. For an implementation which does include this format, the
   -- recommended name of the additional type is SINGLE_EXTENDED_FLOAT.

-- 3.4  Combinations of Formats

   -- P754 defines four formats (single, single extended, double, and double
   -- extended). A minimum implementation requires that single be supported,
   -- and recommends that the extended format for the widest basic format be
   -- supported.

   -- The Ada interface requires that both single and double formats be
   -- supported, but does not require the implementation of any extended
   -- formats, although it allows for the possibility of double extended
   -- format. This corresponds to typical implementations of floating point
   -- hardware which support either single and double only or single, double
   -- and double extended.

   -- A hardware implementation supporting only single format would meet the
   -- requirements, but not the recommendations, of P754. An implementation
   -- providing single and single extended formats only would meet both the
   -- requirements and recommendations of P754, but would NOT satisfy the
   -- requirements of the Ada interface, which is thus more stringent than
   -- P754. The reason for this added stringency is that the Ada interface
   -- is intended to promote portability of code including both basic formats.

   -- The following is a summary of possible combinations of formats:

      --   Formats       Meets P754      Meets P754     Meets Ada Interface
      --  Supported     Requirements   Recommendations     Requirements
      --   S                YES             NO                 NO
      --   S,SX,D           YES             NO                 NO
      --   S,SX             YES             YES                NO
      --   S,D              YES             NO                 YES
      --   S,D,DX           YES             YES                YES
      --   S,SX,D,DX        YES             YES                YES


			       -----------------
			       -- 4. Rounding --
			       -----------------

-- 4.1  Round to Nearest

   -- The default rounding mode is round to nearest. This is the mode which
   -- applies to all computations if the IEEE_P754_ROUNDING_CONTROL package
   -- is not used. In the case where a result is exactly half way between
   -- two representable values, the even value is always chosen.

-- 4.2  Directed Roundings

   -- The package IEEE_P754_ROUNDING_CONTROL can be used to specify the use of
   -- directed rounding modes for all computations. The rounding mode applies
   -- to all operations executed by a single task. The rounding mode may be
   -- specified dynamically and changed as execution proceeds. It applies on
   -- a task by task basis, so that different tasks may have different
   -- rounding modes, but all operations on all floating point types in a
   -- single task are controlled by a single setting of the rounding mode.

   package IEEE_P754_ROUNDING_CONTROL is

      type ROUNDING_MODE is (
	 ROUND_TO_NEAREST,         -- Round to nearest
	 ROUND_UP,                 -- Round up toward plus infinity
	 ROUND_DOWN,               -- Round down towards minus infinity
	 TRUNCATE);                -- Round towards zero

      -- Subprograms to obtain and set the current rounding mode. The mode is
      -- valid on a task by task basis, i.e. different tasks can have different
      -- rounding modes. The default initial rounding mode for the environment
      -- task is ROUND_TO_NEAREST. The default initial rounding mode for any
      -- other task is the rounding mode of its parent task. The rounding mode
      -- controls the behavior of all subsequently executed operations.

	 function CURRENT_ROUNDING_MODE return ROUNDING_MODE;
	 -- CURRENT_ROUNDING_MODE yields the current rounding mode for the
	 -- currently active task.

	 procedure SET_ROUNDING_MODE (MODE : ROUNDING_MODE);
	 -- SET_ROUNDING_MODE sets the current rounding mode for all
	 -- subsequent operations performed by the currently active task.

   end IEEE_P754_ROUNDING_CONTROL;

   -- Note: The Ada attribute MACHINE_ROUNDS is a static value, and reflects
   -- the default behavior. It's value is thus unaffected by calls to the
   -- SET procedure, and may for example be TRUE even after SET_ROUNDING_MODE
   -- has been used to deliberately set some other rounding mode.

-- 4.2.1  Scope of Rounding Effect

   -- The rounding mode affects all predefined operations, including the
   -- conversions between floating point types, and between integer types
   -- and floating point types. The operations introduced by the package
   -- IEEE_P754_OPERATIONS are also affected, as is the input rounding for
   -- the IEEE_P754_IO operations. The conversion of literals appearing in
   -- the source text is NOT affected (see section 5.6.1).

-- 4.3  Rounding Precision

   -- The package IEEE_P754_PRECISION_CONTROL can be used to override the
   -- normal rounding of results for all computations. The mode set applies
   -- to all operations executed by a single task. The precision mode may be
   -- specified dynamically and changed as execution proceeds. It applies on
   -- a task by task basis, so that different tasks may have different
   -- precision modes, but all operations on all floating point types in a
   -- single task are controlled by a single setting of the rounding mode.

   package IEEE_P754_PRECISION_CONTROL is

      type PRECISION_MODE is (
	 NORMAL,                   -- Precision is determined by result
	 ROUND_DOUBLE,             -- All results rounded to double
	 ROUND_SINGLE);            -- All results rounded to single

      -- Subprograms to obtain and set the current precision mode.  The mode
      -- is valid on a task by task basis, i.e. different tasks can have
      -- different precision modes. The default initial precision mode for the
      -- environment task is ROUND_TO_NEAREST. The default initial precision
      -- mode for any other task is the precision mode of its parent task.
      -- The precision mode controls the behavior of all subsequently executed
      -- operations.

	 function CURRENT_PRECISION_MODE return PRECISION_MODE;
	 -- CURRENT_PRECISION_MODE yields the current rounding mode for the
	 -- currently active task.

	 procedure SET_PRECISION_MODE (MODE : PRECISION_MODE);
	 -- SET_PRECISION_MODE sets the current precision mode for all
	 -- subsequent operations performed by the currently active task.

   end IEEE_P754_PRECISION_CONTROL;

-- 4.3.1  Scope of Precision Effect

   -- The precision mode affects all predefined operations, including the
   -- conversions between floating point types, and between integer types
   -- and floating point types. The operations introduced by the package
   -- IEEE_P754_OPERATIONS are also affected, as is the input precision for
   -- the IEEE_P754_IO operations. The conversion of literals appearing in
   -- the source text is NOT affected (see section 5.6.1).


			      -------------------
			      -- 5. Operations --
			      -------------------

   -- The basic arithmetic operations, and a subset of the required comparison
   -- operations are available as predefined operations (see sections 0.2 and
   -- 0.5) The generic package IEEE_P754_OPERATIONS provides the additional
   -- operations required by section 5 and recommended by the appendix of P754
   -- for the type designated in the generic instantiation.

   generic
      type FLOAT_TYPE is digits <>;
   package IEEE_P754_OPERATIONS is

-- 5.1  Arithmetic

   -- The add, subtract, multiply and divide operations correspond to the
   -- predefined operations +, -, * and / in STANDARD, which are defined on
   -- all FLOAT types. The remainder operation is provided by the appropriate
   -- GEF function.

   -- Note: P754 requires that operations be provided on operands of differing
   -- formats. The Ada interface however does NOT provide such "mixed mode"
   -- operations because such operations are not in the style of Ada, and in
   -- particular do not interact well with the provision of derived types. The
   -- effect of mixed mode operations can be achieved using appropriate
   -- conversion operations (and indeed most hardware implementations of P754
   -- require the use of such conversions to perform mixed mode computations).

-- 5.2  Square Root


   -- The square root function is provided by the appropriate function in GEF.

-- 5.3  Floating-Point Format Conversions

   -- Floating point format conversions are achieved using the predefined
   -- conversion operations as described in section 0.2.

-- 5.4  Conversion Between Floating-Point and Integer Formats

   -- Conversions between floating point and integer formats are achieved
   -- using the predefined conversion operations as described in section 0.2.

-- 5.5  Round Floating-Point Number to Integer Value

   -- The round operation rounds a floating point value to an integral valued
   -- floating point value.

      function RNDINT (X : FLOAT_TYPE) return FLOAT_TYPE;
      -- The RNDINT function returns its input rounded to an integral value
      -- using the current rounding mode. The result is in the same floating
      -- point format as the input (NOT in INTEGER format).

-- 5.6  Binary <--> Decimal Conversion

   -- Conversion from string values to and from decimal values may be achieved
   -- using an instantiation of the generic package IEEE_P754_IO whose Ada
   -- specification is identical to FLOAT_IO (see Ada RM section 14.3.8).
   -- The semantics of this package are identical to those of the FLOAT_IO
   -- package except for the following (which would not be permitted in a
   -- correct implementation of FLOAT_IO).

      -- On output negative zero is output as -0.0 (the sign is significant)
      -- On input the string "INF" with an optional preceding sign is allowed
      -- On input implementation dependent strings for NaN's may be recognized

   -- In addition, the following specific behavior (permitted but not required
   -- by the Ada RM for FLOAT_IO) is required:

      -- On input rounding must obey the current rounding mode (see 4.1,4.2)
      -- On output infinite values are output as " INF" or "-INF"
      -- On output NaN's are represented by implementation dependent strings

   -- Note: it seems unfortunate that the separate package should be required
   -- simply to satisfy the legalistic requirements that the special behaviour
   -- required in marginal cases is not compatible with the RM. This matter
   -- should be referred to the ARG to determine whether or not the deviations
   -- suggested here could be regarded as acceptable for the standard package.

   -- In addition, IEEE_P754_OPERATIONS itself contains the following two
   -- functions for performing binary to decimal conversions:

      function VALUE (X : STRING) return FLOAT_TYPE;
      -- The VALUE function converts the string in the same manner as the GET
      -- operation defined in IEEE_P754_IO, including the recognition of -0.0,
      -- INF and implementation defined strings representing NaN values.

      function IMAGE (X : FLOAT_TYPE) return STRING;
      -- The IMAGE function converts its argument and returns a string using
      -- a format identical to that generated by the PUT procedure in package
      -- IEEE_P754_IO using the following parameters:

	 -- FORE  =  2
	 -- AFT   =  FLOAT_TYPE'DIGITS - 1
	 -- EXP   =  3


-- 5.6.1  Conversion of Literals

   -- The IEEE standard recommends that literals be converted at compile time
   -- in a manner identical to the result of these conversions at run time.
   -- This is a potentially very expensive proposition, since the rounding
   -- mode is not known till run time, and this would prevent compile time
   -- conversion of real literals. The spirit of this recommendation is
   -- considered to be best met by always using round to nearest for
   -- conversion of literals.

   -- In cases where literal values are required which must be rounded
   -- according to the currently active rounding mode, or where infinite or
   -- NaN are required as literals, the VALUE function should be used as in
   -- the following examples:

      -- INFINITY   : constant FLOAT_TYPE := VALUE ("+INF");
      -- ROUNDED_PI : constant FLOAT_TYPE :=
      --                 VALUE("3.14159_26535_89793_23846");

-- 5.7 Comparison

   -- P754 specifies two methods for comparisons. The first yields a result
   -- indicating which of four possible relations holds:

      type RELATION_TYPE is (GREATER_THAN, LESS_THAN, EQUAL, UNORDERED);

      function COMPARE (X, Y : FLOAT_TYPE) return RELATION_TYPE;
      -- COMPARE indicates the ordering relation which holds between its two
      -- operands. The result is UNORDERED if either operand is a NaN. This
      -- operation never signals a fault.

   -- The other approach involves the provision of a set of predicate
   -- functions. Note that the first six functions are equivalent to the
   -- predefined comparison operations (= /= > >= < <=). The table at the
   -- right side (which is excerpted from Table 4 in P754) shows the result
   -- for each of the four relations which can hold between the operands
   -- (U = Unordered).

							--    Result if
							--   >   <   =   U
      function EQ  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   F   F   T   F
      function NE  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   T   T   F   T
      function GT  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   T   F   F   F
      function GE  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   T   F   T   F
      function LT  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   F   T   F   F
      function LE  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   F   T   T   F
      function LG  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   T   T   F   F
      function LEG (X, Y : FLOAT_TYPE) return BOOLEAN;  --   T   T   T   F
      function UG  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   T   F   F   T
      function UGE (X, Y : FLOAT_TYPE) return BOOLEAN;  --   T   F   T   T
      function UL  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   F   T   F   T
      function ULE (X, Y : FLOAT_TYPE) return BOOLEAN;  --   F   T   T   T
      function UE  (X, Y : FLOAT_TYPE) return BOOLEAN;  --   F   F   T   T

      -- Note: the "unordered" function (?) listed in Table 4 of P754 is
      -- provided as the UNORDERED function in the following section. There
      -- seems to be some confusion as to whether this function is required,
      -- as implied by its presence in Table 4, or optional, as implied by
      -- its presence in the Appendix. In any case the Ada interface requires
      -- that the functions in the Appendix be provided, so the confusion does
      -- not affect this specification.

-- 5.7.1  Recommended Functions and Predicates (from P754 Appendix)

   -- P754 lists a number of recommended functions in the Appendix which are
   -- not required. However, a full implementation of the Ada packages
   -- requires that these functions be provided.

      function COPYSIGN (X, Y : FLOAT_TYPE) return FLOAT_TYPE;
      -- COPYSIGN returns X with the sign of Y. This operation is not required
      -- to signal the invalid operation fault for signaling NaN's. An
      -- implementation must specify whether or not the fault is signaled.

      function SCALB (X : FLOAT_TYPE; Y : INTEGER) return FLOAT_TYPE;
      -- SCALB returns X * 2**Y without computing 2**Y.

      function LOGB (X : FLOAT_TYPE) return FLOAT_TYPE;
      -- LOGB returns the unbiased exponent of X, a signed integer in the
      -- same format as X. See P754 for handling of special cases.

      function NEXTAFTER (X, Y : FLOAT_TYPE) return FLOAT_TYPE;
      -- NEXTAFTER returns the next representable neighbor of X in the
      -- direction toward Y. See P754 for handling of special cases.

      function FINITE (X : FLOAT_TYPE) return BOOLEAN;
      -- FINITE returns TRUE if its argument is neither a NaN nor infinite.
      -- This operation is not required to signal the invalid operation fault
      -- for signaling NaN's.  An implementation must specify whether or not
      -- the fault is signaled.

      function ISNAN (X : FLOAT_TYPE) return BOOLEAN;
      -- ISNAN returns TRUE if its argument is a NaN. This operation is not
      -- required to signal the invalid operation fault for signaling NaN's.
      -- An implementation must specify whether or not the fault is signaled.

      function UNORDERED (X, Y : FLOAT_TYPE) return BOOLEAN;
      -- Returns TRUE if either X or Y is a NaN. No fault is ever signaled.

      type CLASS_TYPE is (
	 SIGNALLING_NAN,
	 QUIET_NAN,
	 NEGATIVE_ZERO,
	 NEGATIVE_DENORMALIZED,
	 NEGATIVE_NORMALIZED_NONZERO,
	 NEGATIVE_INFINITY,
	 POSITIVE_ZERO,
	 POSITIVE_DENORMALIZED,
	 POSITIVE_NORMALIZED_NONZERO,
	 POSITIVE_INFINITY);

      subtype POSITIVE is CLASS_TYPE range POSITIVE_ZERO .. POSITIVE_INFINITY;
      subtype NEGATIVE is CLASS_TYPE range POSITIVE_ZERO .. POSITIVE_INFINITY;

      function CLASS (X : FLOAT_TYPE) return CLASS_TYPE;
      -- CLASS returns the class of its argument. No fault is ever signaled.

   -- Note: the negation operation -X is provided in STANDARD. This operation
   -- is not required to signal the invalid operation fault if the operand is
   -- a signalling NaN. An implementation must specify whether the fault is
   -- signaled.

   -- Note: some but not all of these functions are provided by GPF, and/or
   -- by the proposed attributes in Ada 9X, so some further discussion is
   -- appropriate.

   end IEEE_P754_OPERATIONS;


		    ---------------------------------------
		    -- 6. Infinity, NaNs and Signed Zero --
		    ---------------------------------------

   -- 6.1  Infinity Arithmetic

     -- Signed infinities and signed zeroes shall be fully supported by any
     -- implementation of this package. Infinite values can only be generated
     -- if traps and exceptions for overflow faults are suppressed using the
     -- the facilities of the IEEE_P754_FAULT_CONTROL package.

   -- 6.2  Operations with NaNs

     -- NaN's shall be implemented as defined in section 6.2 of the standard,
     -- and any implementation of this package must describe the NaN formats
     -- supported, their representations, and the manner in which they are
     -- produced and recognized.

   -- 6.3 The Sign Bit

     -- The implementation must recognize and distinguish negative zeroes
     -- from positive zeroes in cases where such distinction is required.
     -- Notably, the IEEE_P754_IO input routines recognize minus zero, and
     -- preserve the sign on outputting minus zero. Both the predefined
     -- operations and the operations introduced in IEEE_P754_OPERATIONS
     -- treat signed zero values in a manner consistent with P754. For
     -- example the division of positive infinity by zero returns a value
     -- with the same sign as the zero value.


		   -----------------------------------------
		   -- 7. Fault Handling (P754 Exceptions) --
		   -----------------------------------------

   -- Note: as previously mentioned in section 2, this specification uses
   -- the term "fault" rather than "exception", to avoid confusion with the
   -- Ada use of the term exception. Thus any use of the word exception (other
   -- than in the previous sentence and the corresponding sentence in section
   -- 2) refers to Ada exceptions and NOT to P754 exceptions.

   -- The required level of control over fault handling is provided by the
   -- by the package IEEE_P754_FAULT_CONTROL. This package makes use of
   -- a user provided trap routine, IEEE_P754_USER_TRAP_ROUTINE. A program
   -- which incorporates the IEEE_P754_FAULT_CONTROL package must provide
   -- a body for this parameterless library procedure. If trap processing is
   -- not required (FAULT_ACTION never set to TRAP), then a dummy body can be
   -- used to satisfy this requirement.

   procedure IEEE_P754_USER_TRAP_ROUTINE;

   with IEEE_P754_USER_TRAP_ROUTINE;
   package IEEE_P754_FAULT_CONTROL is

   -- The following functions read, set and reset the values of the status
   -- flags for faults defined in section 7. These status flags are provided
   -- on a task by task basis, so that each task has its own separate set of
   -- of status flags. The flags are initially FALSE when a task is created.

      type FAULT_TYPE is (
	 INVALID_OPERATION, DIVISION_BY_ZERO, OVERFLOW, UNDERFLOW, INEXACT);

      function FAULT_STATUS_FLAG (FAULT : FAULT_TYPE) return BOOLEAN;
      -- FAULT_STATUS_FLAG returns the setting of the specified status flag
      -- for the currently active task.

      procedure SET_FAULT_STATUS_FLAG (FAULT : FAULT_TYPE);
      -- SET_FAULT_STATUS_FLAG sets the specified status flag to TRUE for the
      -- currently active task.

      procedure RESET_FAULT_STATUS_FLAG (FAULT : FAULT_TYPE);
      -- RESET_FAULT_STATUS_FLAG sets the specified status flag to FALSE for
      -- the currently active task.

      procedure RESET_FAULT_STATUS_FLAGS;
      -- RESET_FAULT_STATUS_FLAGS resets all status flags to FALSE for the
      -- currently active task.

   -- 7.1  Invalid Operation
   -- 7.2  Division by Zero
   -- 7.3  Overflow
   -- 7.4  Underflow
   -- 7.5  Inexact

      -- These faults are signaled in the situations described in the
      -- corresponding section of P754. The effect of a fault being signaled
      -- is described in the following section.


				--------------
				-- 8. Traps --
				--------------

   -- P754 requires that a user be able to specify whether or not a user trap
   -- routine is called following the occurrence of a fault. In addition this
   -- specification allows for a third possibility of raising an Ada exception
   -- which is more in keeping with the normal Ada view of how exceptional
   -- conditions should be handled. The definitions in this section provide
   -- means of controlling which of the three fault handling approaches is to
   -- be used for subsequent operations.

      type FAULT_ACTION is (

	 TRAP,
	    -- When the fault occurs, IEEE_P754_USER_TRAP_ROUTINE is called.
	    -- This procedure may use the subprograms defined in section 8.1
	    -- to determine the cause of the fault. It can take one of two
	    -- possible actions, it may raise an Ada exception, in which case
	    -- the Ada exception will be propagated from the location of the
	    -- operation signalling the P754 fault, or it may return after
	    -- calling SET_RESULT to provide a replacement result, in which
	    -- case execution continues from the point of the fault.

	    -- This mode may only be specified if MACHINE_OVERFLOWS is FALSE.


	 RAISE_FAULT,
	    -- The exception FAULT is raised. The exception handler may use
	    -- the subprograms in section 8.1 to determine the cause of
	    -- the fault.

	    -- This mode may only be specified if MACHINE_OVERFLOWS is FALSE.

	 RAISE_ERROR,
	    -- The exception CONSTRAINT_ERROR is raised. This is the action
	    -- which corresponds to the normal Ada handling on a numeric
	    -- error, where no attempt is made to determine the cause of the
	    -- fault, and is the default mode if MACHINE_OVERFLOWS is TRUE.

	    -- This mode may only be specified if MACHINE_OVERFLOWS is FALSE.

	 NO_ACTION);
	    -- No trap occurs and no exception is raised. The operation will
	    -- return the default value specified in P754.

	    -- This is the only mode which may be specified, and is the
	    -- default mode, if MACHINE_OVERFLOWS is set to FALSE. It must
	    -- still be fully implemented if MACHINE_OVERFLOWS is TRUE.

	 -- Regardless of the fault action, the corresponding status flag is
	 -- set when a fault occurs (and remains set until explicitly reset).

      -- The following subprograms may be used to set or determine the trap
      -- action associated with any of the individual fault types.

	 procedure SET_FAULT_ACTION (FAULT : FAULT_TYPE;
				     ACTION : FAULT_ACTION);
	 -- Set the action to be taken on a the specified fault. Any
	 -- subsequent operations in the current task causing this fault will
	 -- result in the specified action to be taken. It is possible for
	 -- different tasks to have different trap actions for the same
	 -- fault, and it is possible to have different actions for different
	 -- faults within a single task.

	 -- Note: on a machine where MACHINE_OVERFLOWS is FALSE the attempt
	 -- to set any ACTION other than NO_ACTION results in raising the
	 -- predefined exception PROGRAM_ERROR.

	 function GET_FAULT_ACTION (FAULT : FAULT_TYPE) return FAULT_ACTION;
	 -- Get the action currently being taken on the specified fault for
	 -- the current task.
      -- The initial default actions for the environment task are RAISE_ERROR
      -- for the overflow, divide by zero and invalid operation faults, and
      -- NO_ACTION for the underflow and inexact faults. The initial set of
      -- actions for any other task is inherited from the parent task.

   -- 8.1  Trap Handler

      -- Section 8.1 requires that a trap handler can obtain certain
      -- information about the nature of the fault causing the trap. This
      -- section of the specification provides a set of functions which can
      -- be used after a fault has occurred to determine the required
      -- information. These functions may only be used in a user trap
      -- routine, or in an exception handler for the exception FAULT.

      -- A check is made when any of these functions are used that a fault
      -- has occurred for which the specified fault action is either
      -- RAISE_FAULT or TRAP. If this check fails, then PROGRAM_ERROR is
      -- raised, indicating a flaw in the logical structure of the program.

      -- 8.1.1  Determining which exception has occurred

	 -- The following functions can be used to determine which fault or
	 -- faults caused the trap:

	    function FAULT_OCCURRED (FAULT : FAULT_TYPE) return BOOLEAN;
	    -- FAULT_OCCURRED returns TRUE if the specified FAULT_TYPE was one
	    -- of the faults which caused the current trap. Note that the
	    -- result differs from that obtained from FAULT_STATUS_FLAG in
	    -- that the status flags are sticky and remain set until
	    -- explicitly reset. FAULT_OCCURRED signals TRUE only for the
	    -- specific faults which have caused the most recent trap or
	    -- exception in the current task.

      -- 8.1.2  Determining the kind of operation that was being performed

	 -- The following definitions allow the determination of the kind of
	 -- the operation which caused the trap.

	    type OPERATION is
	       (OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_REM, OP_CONVERSION,
		EQ, NE, GT, GE, LT, LE, LG, LEG, UG, UGE, UL, ULE, UE,
		COMPARE, VALUE, IMAGE, SQRT, SCALB, LOGB, NEXTAFTER);

	    function FAULT_OPERATION return OPERATION;
	    -- FAULT_OPERATION returns one of the possible values of OPERATION
	    -- to indicate the type of the operation causing the fault.

	    type FORMAT is (FLOAT_FORMAT,
			    LONG_FLOAT_FORMAT,
			    EXTENDED_FLOAT_FORMAT,
			    INTEGER_FORMAT);
	    -- The type FORMAT is used to indicate precisions of operands and
	    -- results. Implementations which provide additional FLOAT formats
	    -- (such as SINGLE_EXTENDED_FLOAT), or additional integer types
	    -- (such as LONG_INTEGER), should extend the definition of FORMAT
	    -- appropriately.

	    function OPERAND_FORMAT return FORMAT;
	    -- The function OPERAND_FORMAT may be used to determine the format
	    -- of the operand or operands of the operation.
      -- 8.1.3  Determining the destination's format

	  -- The following function may be used to determine the format of
	  -- the destination (result) of the operation causing the trap:

	     function DESTINATION_FORMAT return FORMAT;

       -- 8.1.4  Determining the correctly rounded result

	 -- In the case where the fault is caused by overflow, underflow or
	 -- inexact results, the following functions may be used to determine
	 -- the correctly rounded result.

	    function RESULT_SIGNIFICAND return FLOAT;
	    function RESULT_SIGNIFICAND return LONG_FLOAT;
	    function RESULT_SIGNIFICAND return EXTENDED_FLOAT;
	    -- RESULT_SIGNIFICAND returns the significand of the correctly
	    -- rounded result. The absolute value of the significand is in
	    -- the range 1.0 <= significand < 2.0, except in the case of a
	    -- true zero result, in which case an appropriately signed zero
	    -- value is returned.

	    function RESULT_EXPONENT return FLOAT;
	    function RESULT_EXPONENT return LONG_FLOAT;
	    function RESULT_EXPONENT return EXTENDED_FLOAT;
	    -- RESULT_EXPONENT returns the exponent of the correctly rounded
	    -- result as a signed unbiased integral value. This exponent value
	    -- may be outside the normally permitted exponent range.

	 -- Note: if the fault causing the trap or FAULT exception was not
	 -- overflow, underflow or inexact, then the use of either of these
	 -- functions represents a logical flaw in the program structure, and
	 -- PROGRAM_ERROR is raised.

      -- 8.1.5  Determining the operand values

	 -- In the case where the fault is caused by invalid operation or
	 -- divide by zero conditions, the following functions may be used to
	 -- determine the operand values.

	    function OPERAND_X return FLOAT;
	    function OPERAND_X return LONG_FLOAT;
	    function OPERAND_X return EXTENDED_FLOAT;
	    function OPERAND_Y return FLOAT;
	    function OPERAND_Y return LONG_FLOAT;
	    function OPERAND_Y return EXTENDED_FLOAT;
	    -- The corresponding operand value is returned (X and Y refer to
	    -- the names of the formal parameter).

	 -- Note: the use of these functions when the fault condition is other
	 -- than divide by zero or invalid operation, or the use of OPERAND_Y
	 -- when the operation causing the fault has only one operand, or when
	 -- the format does not correspond to the operand format, represents a
	 -- logical flaw in the program structure, and results in raising the
	 -- exception PROGRAM_ERROR.

	 -- Note: even if the original operation involved types derived
	 -- from the standard types, the operands may be fetched using the
	 -- basic types. It is assumed that the trap routine knows only about
	 -- representations and not about types at the Ada level.
      -- 8.1.6  Setting an Alternate Result

	 -- The following functions may be used in the user trap routine to
	 -- set an alternate result for the operation:

	    procedure SET_RESULT (X : FLOAT);
	    procedure SET_RESULT (X : LONG_FLOAT);
	    procedure SET_RESULT (X : EXTENDED_FLOAT);

	 -- Note: the use of these procedures if the FAULT_ACTION is other
	 -- than TRAP, or an attempt to set a result of inappropriate format,
	 -- represents a logical flaw in the program structure, and results
	 -- in PROGRAM_ERROR being raised.

   -- 8.2  Precedence

      -- In accordance with section 8.2, if overflow or underflow traps are
      -- enabled (i.e. the fault action is other than NO_ACTION), then the
      -- overflow or underflow trap or exception takes precedence over a
      -- separate inexact trap or exception.

   end IEEE_P754_FAULT_CONTROL;


				--------------
				-- Appendix --
				--------------

   -- P754 lists a number of recommended functions in the Appendix which are
   -- not required. However, a full implementation of the Ada packages requires
   -- that these functions be provided. See section 5.7.1 for details.
