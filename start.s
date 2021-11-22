.intel_syntax noprefix

.extern kernel_main
.global start
.set MB_MAGIC, 0x1BADB002
.set MB_FLAGS, 3
.set MB_CHECKSUM, (0 - (MB_MAGIC + MB_FLAGS))

.section .multiboot
.align 4
.long MB_MAGIC
.long MB_FLAGS
.long MB_CHECKSUM

.section .bss
.align 16
stack_bottom:
.skip 4096
stack_top:

.section .text
start:
mov stack_top, esp
call enter_protected
call kernel_main

hang:
cli
hlt
jmp hang

enter_protected:
call load_gdt
mov eax, cr0
or al, 1
mov cr0, eax
ret

load_gdt:
cli
xor   eax, eax
mov   ax, ds
shl   eax, 4
add   eax, gdt
mov   [gdtr + 2], eax
mov   eax, gdt_end
sub   eax, gdt
mov   [gdtr], ax
lgdt  [gdtr]
ret

// this probably shouldn't be in text...
gdtr:
.word 0
.double 0

// this probably shouldn't be in text...
gdt:
.quad 0
.quad 0x00C09A0000000FFF // full memory, R/W permissions
gdt_end:
