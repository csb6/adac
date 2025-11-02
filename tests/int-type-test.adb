package IntTypeTest is
    -- comment
    type SmallInteger is range 16#FFF# .. 12;

    type BigInteger is range 1 .. 12_000_000_000;
    foo: SmallInteger := 1;
    bar: constant BigInteger;
    foobar: constant := 1_000;
end;
