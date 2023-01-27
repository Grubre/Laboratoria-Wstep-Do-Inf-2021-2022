.globl _start


.section .bss
    .lcomm  hexnum, 16


.section .data
    lookup: .ascii "0123456789ABCDEF"
    input:  .quad 4294967295


.section .text
_start:
    # r9  - length of our string
    # rax - number to be converted to hex
    # rsi - pointer to output string
    movq    $0,     %r9
    movq    $hexnum,  %rsi
    movq    input,    %rax

    # push "0x" to our string
    movb    $48,    (%rsi, %r9)
    incq    %r9
    movb    $120,    (%rsi, %r9)
    incq    %r9

    # push guard to stack
    pushq   $17

    # push chars to stack to put them in reverse order
    movq    $16,   %rbx
stackloop:
    movq    $0,    %rdx
    div     %rbx
    movb    lookup(, %rdx), %cl
    pushq   %rcx
    test    %rax,   %rax
    jnz     stackloop

    # move chars from stack to output string
constructloop:
    popq    %rax
    movb    %al, (%rsi, %r9)
    incq    %r9
    cmp     $17,    %rax
    jne     constructloop

    # push newline and \0 to our string
    movb    $10,   (%rsi, %r9)
    incq    %r9
    movb    $0,    (%rsi, %r9)
    incq    %r9

    # call write
    movq    $1,     %rax
    movq    $1,     %rdi
    movq    %r9,     %rdx
    syscall

    # exit the program
    movq    $0,     %rdi
    movq    $60,    %rax
    syscall
