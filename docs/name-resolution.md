# Name Resolution

## Cases
- Object usage
    - Variants: "foo"
    - Parentheses: No
- Function call
    - Variants: "foo", "foo(bar)", "foo(bar, baz)", etc.
    - Parentheses: Sometimes (if > 0 arguments)
- Indexed component
    - Variants: "foo(1)", "foo(1, 2)", etc.
    - Parentheses: Yes
- Array access on array returned by function
    - Variants: "foo(1)", "foo(bar)(baz)", etc.
    - Parentheses: Yes
- Slice
    - Variants: "foo(1 .. 2)"
    - Parentheses: Yes
    - Note: slices cannot have more than one dimension

## Algorithm

- TODO: handle default/keyword parameters (ugh)
- TODO: handle packages/qualified names/fields
- TODO: handle operators (maybe intially treat all operator expressions as binary/unary calls with the operator string as the name?)
- CandidateExpr is a placeholder expression holding the original expression and all of its possible definitions
- Instead of replacing CandidateExpr after the fact, just do it eagerly as each expression is parsed (will only need to
do it for expressions that involve names)

function resolve_pass_1(visible_decls: map String to set of Decl; expr: Expr) return Expr is
begin
    case expr is
        case FunctionCallLike =>
            if expr.arity = 0 then
                return CandidateExpr(
                    expr: expr,
                    candidates: { d | d in visible_decls(expr.name) and (d is ObjectDecl or (d is FunctionDecl and d.arity = 0)}
                );
            else if expr.index.kind = RangeExpr then
                return CandidateExpr(
                    expr: expr,
                    candidates: {d | d in visible_decls(expr.name) and d.type is ArrayType}
                )
            else
                return CandidateExpr(
                    expr: expr,
                    candidates: { d |
                        d in visible_decls(name) and (
                            (d is FunctionDecl and d.arity = expr.arity) or
                            (d is ObjectDecl and d.type is ArrayType and d.dimension_count = expr.arity)
                        )
                    };
                )
            end if;
        others =>
            return expr with each subexpression replaced by resolve_pass_1(subexpression);
    end case;
end get_candidates;
