.globl main

.section .rodata
   fmt:    .ascii  "%lld\n"

.section .text
main:
    movq    $-12,   %rdi
    movq    $16,    %rsi

    call    multiply

    # print our number
    movq    stdout, %rdi
    movq    %rax,   %rdx
    movq    $fmt,   %rsi
    movq    $0,     %rax
    call    fprintf

    movq    $24,   %rdi
    movq    $7,    %rsi

    call    divide

    # backup %rdx, where we have remainder
    movq    %rdx,   %r12

    # print our number
    movq    stdout, %rdi
    movq    %rax,   %rdx
    movq    $fmt,   %rsi
    movq    $0,     %rax
    call    fprintf

    movq    stdout, %rdi
    movq    %r12,   %rdx
    movq    $fmt,   %rsi
    movq    $0,     %rax
    call    fprintf

    movq    $0, %rax
    ret
