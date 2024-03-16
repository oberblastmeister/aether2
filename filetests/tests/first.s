        .intel_syntax   noprefix     
        .text
        .type   another,@function
        .globl  another
another:
        sub     rsp, 8
.Lstart0:
        movabs  rax, 1
        movabs  rdi, 1
        mov     rsi, rdi
        mov     rdi, rax
        call    assert_eq_u64
        add     rsp, 8
        ret
        .type   entry,@function
        .globl  entry
entry:
        sub     rsp, 8
.Lstart0:
        call    another
        add     rsp, 8
        ret
