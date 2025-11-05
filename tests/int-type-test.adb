package IntTypeTest is
    -- comment
    type SmallInteger is range 16#FFF# .. 12;

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
    bool: Boolean := True;
    bool2: boOlean := False;

    subtype NewBoolean is Boolean;
    nb: NewBoolean := False;
    type DistinctBoolean is new Boolean;
    db: DistinctBoolean := True;

    -- subtype Bad is Nonexistent;
    -- type BigInteger is range 0 .. 1;
    -- type biginteger is range 0 .. 1;
    -- BigInteger: Boolean;
end;
