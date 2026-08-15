package SimplePackage is
    type Boolean is (False, True);
    type Foo is (A, B, C);

    package Inner is
        type B is range 0 .. 1;
        type InnerFoo is new Foo;
    end Inner;

    procedure Bar;
    use Inner;
    function Return_Foo return InnerFoo;
end SimplePackage;
