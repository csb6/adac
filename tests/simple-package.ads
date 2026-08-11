package SimplePackage is
    type Boolean is (False, True);
    type Foo is (A, B, C);

    package Inner is
        type B is range 0 .. 1;
    end Inner;

    procedure Bar;
end SimplePackage;
