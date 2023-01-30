.globl multiply

.section .text

multiply:
    # %rcx - counter
    # %rax - result
    # %rdx - bitmask
    # %rdi - opperand A
    # %rsi - opperand B
    movq    $0,     %rcx
    movq    $0,     %rax
    movq    $0x1,   %rdx

loop:
    test    %rdx,   %rdi
    jz      continue
    movq    %rsi,   %rbx
    shl     %rcx,   %rbx
    addq    %rbx,   %rax

continue:
    shl     %rdx
    incq    %rcx
    cmp     $64,    %rcx
    jle     loop

    ret
