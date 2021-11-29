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
cli
call load_gdt
call load_idt
mov eax, cr0
or al, 1
mov cr0, eax
sti
ret

load_gdt:
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

load_idt:
xor   eax, eax
mov   ax, ds
shl   eax, 4
add   eax, idt
mov   [idtr + 2], eax
mov   eax, idt_end
sub   eax, idt
mov   [idtr], ax
lidt  [idtr]
ret

.section .data
gdtr:
.word 0
.double 0

gdt:
.quad 0
.quad 0x00C09A0000000FFF // full memory, R/W permissions
gdt_end:

idtr:
.word 0
.double 0

idt:
.quad 0 // 0
.quad 0 // 1
.quad 0 // 2
.quad 0 // 3
.quad 0 // 4
.quad 0 // 5
.quad 0 // 6
.quad 0 // 7
.quad 0 // 8
// 9:
idt_offset_a: .word 0 // function goes here
.double 0x8E000008
idt_offset_b: .word 0
idt_end:
