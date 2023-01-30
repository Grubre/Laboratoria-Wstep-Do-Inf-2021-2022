.globl main


.section .data
    lookup: .ascii "0123456789"
    input:  .quad   0x0453


.section .bss
    .lcomm  output, 6


.section .text
main:
    # r9  - length of our string
    # rax - binary represantion of BCD number
    # rsi - pointer to output string
    movq    $0,     %r9
    movq    input,  %rax
    movq    $output,    %rsi

    movq    $12,    %rcx
loop:
    movq    %rax,   %rdx
    shrq    %rcx,    %rdx
    andq    $0xF,   %rdx
    movb    lookup(, %rdx), %bl
    movb    %bl,    (%rsi,  %r9)
    incq    %r9
    subq    $4,     %rcx
    cmpq    $0,     %rcx
    jge     loop

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

    movq    $0, %rax
    ret
