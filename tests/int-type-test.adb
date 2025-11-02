package IntTypeTest is
    -- comment
    type SmallInteger is range 16#FFF# .. 12;

    type BigInteger is range 1 .. 12_000_000_000;
    small_int: SmallInteger := 1;
    big_int: constant BigInteger;
    c: constant := 1_000;

    type CharType is ('a', 'b', 'c');
    type EnumType is (A, B, C);
    type MixedEnumType is (A, 'b', C, 'd', 'e');
    type Boolean is (False, True);
    char: CharType := 'c';
    e: EnumType := A;
    bool: Boolean := True;
end;
