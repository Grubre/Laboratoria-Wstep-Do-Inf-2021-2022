.globl main

.equ    MAXNUM, 100000

.section .bss
    .lcomm  mask,   2*MAXNUM + 2

.section .rodata
   fmt:    .ascii  "%lld\n"

.section .text
main:
    # set mask array to 0
    movq    $MAXNUM, %rcx
setmaskloop:
    movw    $0,     mask(,%rcx,2)
    loop    setmaskloop

    # sieve of eratosthenes
    movq    $2,     %r12
primesloop:
    # first we check if the number is marked
    movw    mask(,%r12,2),   %dx
    test    %dx,    %dx
    jnz     continue

    # if not then print it
    movq    stdout, %rdi
    movq    %r12,   %rdx
    movq    $fmt,   %rsi
    movq    $0,     %rax
    call    fprintf

    # and mark all of its multiples
    movq    %r12,   %r13
    addq    %r12,   %r13
    jmp     innerloopcond
innerloop:
    movw    $1,     mask(,%r13,2)
    addq    %r12,   %r13
innerloopcond:
    cmp     $MAXNUM,   %r13
    jle     innerloop

continue:
    incq    %r12
    cmp     $MAXNUM,   %r12
    jle     primesloop


    # exit the program
    movq $0, %rax
    ret
