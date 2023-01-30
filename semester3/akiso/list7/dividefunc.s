.globl divide

.section .text

divide:
    # %rcx - bitmask
    # %rax - result
    # %rdx - current number
    # %rdi - opperand A
    # %rsi - opperand B
    movq    $63,     %rcx
    movq    $0,     %rdx
    movq    $0,     %rax

loop:
    shlq    %rax
    shlq    %rdx
    movq    $1, %rbx
    shlq    %rcx,    %rbx
    test    %rbx,   %rdi
    jz      nonincr
    incq    %rdx
nonincr:
    cmpq    %rsi,   %rdx
    jl      continue
    subq    %rsi,   %rdx
    incq    %rax

continue:
    decq    %rcx
    cmpq    $0,     %rcx
    jge     loop

end:
    ret
