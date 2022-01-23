global _start

MB_MAGIC: equ 0x1BADB002
MB_FLAGS: equ 3
MB_CKSUM: equ -(MB_MAGIC + MB_FLAGS)
VGA_COLS: equ 80
VGA_ROWS: equ 25
VGA_STRT: equ 0xB8000
BKSLASHN: equ 0xA

section .multiboot
align 4
dd MB_MAGIC
dd MB_FLAGS
dd MB_CKSUM

section .bss
align 16
stack_bottom:
resb 4096
stack_top:

section .data
hello_world:
db "Welcome to Chuck OS! (now in nasm)"
db 0

section .text
_start:
mov esp, stack_top
call kernel_main
hang:
cli
hlt
jmp hang

kernel_main:
call clear_screen
mov ecx, 0
mov edx, 0
mov eax, hello_world
call print_str
ret

print_str:
; eax: string start pos
; ecx, edx: current position
; clobbers eax, bx
; updates ecx, edx
mov bl, [eax]
cmp bl, 0
je ret_lbl
inc eax
cmp bl, BKSLASHN
je print_str.newline
push eax
call write_char
pop eax
call vga_inc
jmp print_str
.newline:
call vga_newline
jmp print_str

clear_screen:
mov bl, ' '
mov ecx, 0
mov edx, 0
.loop:
call write_char
call vga_inc
cmp ecx, VGA_ROWS
je ret_lbl
jmp clear_screen.loop

vga_inc:
; ecx: row
; edx: col
inc edx
cmp edx, VGA_COLS
je vga_newline
ret

vga_newline:
mov edx, 0
inc ecx
ret

write_char:
; bl:  character to write
; ecx: row to write
; edx: col to write
; clobbers eax and bh
mov bh, 0x0F
mov eax, VGA_COLS
push edx
mul ecx
pop edx
add eax, edx
push edx
mov edx, 2
mul edx
pop edx
add eax, VGA_STRT
mov [eax], bx
ret

ret_lbl:
ret
