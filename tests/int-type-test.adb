package IntTypeTest is
    -- comment
    type SmallInteger is range 16#FFF# .. 12+1;

    type BigInteger is range 1 .. 12_000_000_000;
    small_int: SmallInteger := 1 + 2 * +4 * abs 1 / 3**12-1;
    big_int: constant BigInteger;
    c: constant := 1_000;

    type CharType is ('a', 'b', 'c');
    type EnumType is (A, B, C);
    type MixedEnumType is (A, 'b', C, 'd', 'e');
    type Boolean is (False, True);
    char: CharType := 'c';
    e: EnumType := A;
    bool: Boolean := True and then False;
    bool2: boOlean := False or else True and True not in False;

    subtype NewBoolean is Boolean;
    nb: NewBoolean := False;
    type DistinctBoolean is new Boolean;
    db: DistinctBoolean := True;

    -- subtype Bad is Nonexistent;
    -- type BigInteger is range 0 .. 1;
    -- type biginteger is range 0 .. 1;
    -- BigInteger: Boolean;

    procedure zeroary_procedure;
    procedure unary_procedure(a : CharType);
    procedure binary_procedure(a : CharType; b : Boolean := False);

    function zeroary_function return Boolean;
    function unary_function(a : CharType) return Boolean;
    function binary_function(a : CharType; b : Boolean := False) return Boolean;
    function modes(a : in CharType; b : in out CharType; c : out Boolean) return Boolean;

    function "+"(a : BigInteger) return BigInteger;
    function "+"(a : BigInteger; b : BigInteger) return BigInteger;
    -- function "+"(a : BigInteger; b : BigInteger; c : BigInteger) return BigInteger;
    function "="(a : BigInteger; b : BigInteger) return Boolean;
    -- function "="(a : BigInteger) return Boolean;
    -- function "="(a : BigInteger; b : BigInteger; c : BigInteger) return Boolean;
    function "abs"(a : BigInteger) return BigInteger;
    -- function "abs"(a : BigInteger; b : BigInteger) return BigInteger;
    -- function "mod"(a : BigInteger; a : BigInteger) return Boolean;
    -- procedure "+"(a : BigInteger; b : BigInteger);

    procedure foo is
        a : Boolean := False;
        b : CharType := 'b';
        procedure inner is
        begin
            null;
        end;
    begin
        foo;
        zeroary_procedure;
        unary_procedure('u');
        binary_procedure('a', c);
        if True then
            null;
        end if;

        if True then
            null;
        elsif False then
            null;
        end if;

        if True then
            null;
        elsif False then
            null;
        else
            a := True;
        end if;
        return;
    end;

    procedure bar is
        a : DistinctBoolean := True;
        b : CharType := 'c';
        -- a : Boolean := True;
    begin
        null;
        declare
            a : Boolean := True;
        begin
            null;
        end;
        begin
            b := False;
            case b is
                when False | True => b := True;
                when 0 .. 1 => null;
                when others  =>
                  b := True;
                  b := False;
            end case;
        end;
        return 1 + 1;
    end bar;

    procedure some_loops is
    begin
        loop
            null;
        end loop;
        while True loop
            null;
            some_loops;
        end loop;
        for i in 0 .. 10 loop
            null;
            exit when True;
        end loop;
        for i in reverse 0 .. 10 loop
            null;
            exit;
        end loop;
    end;

    procedure some_gotos is
    begin
        goto A;
        <<A>> null;

        <<B>> null;
        goto B;

        declare
        begin
           goto B;
        end;

        goto C; -- TODO: this will be a semantic error (C is never defined)
        declare
        begin
            <<C>> null; -- Not visible to enclosing scope
        end;
    end;
end inttypetest;
