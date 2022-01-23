global start

; code-wide standard:
; when dealing with VGA, ecx is row and edx is col

MB_MAGIC equ 0x1BADB002
MB_FLAGS equ 3
MB_CKSUM equ -(MB_MAGIC + MB_FLAGS)
VGA_COLS equ 80
VGA_ROWS equ 25
VGA_STRT equ 0xB8000
BKSLASHN equ 0xA

section .multiboot
align 4
dd MB_MAGIC
dd MB_FLAGS
dd MB_CKSUM

section .bss
align 16
stackBottom:
resb 4096
stackTop:

section .data
welcome db "Welcome to Chuck OS! (now in nasm)", BKSLASHN, 0
vga_row dd 0
vga_col dd 0

section .text
start:
mov esp, stackTop
call kernelMain
hang:
cli
hlt
jmp hang

kernelMain:
call clearScreenStd
mov eax, welcome
call printStrStd
mov eax, welcome
call printStrStd
ret

printStrStd:
; eax: string start pos
; uses global vars vga_row and vga_col
mov ecx, [vga_row]
mov edx, [vga_col]
call printStr
mov [vga_row], ecx
mov [vga_col], edx
ret

clearScreenStd:
; uses global vars vga_row and vga_col
call clearScreen
mov ecx, 0
mov [vga_row], ecx
mov [vga_col], ecx
ret

printStr:
; eax: string start pos
; ecx, edx: current position
; clobbers eax, bx
; updates ecx, edx
mov bl, [eax]
cmp bl, 0
je retLbl
inc eax
cmp bl, BKSLASHN
je printStr.newline
push eax
call writeChar
pop eax
call vgaInc
jmp printStr
.newline:
call vgaNewline
jmp printStr

clearScreen:
; clobbers bx, ecx, and edx
mov bl, ' '
mov ecx, 0
mov edx, 0
.loop:
call writeChar
call vgaInc
cmp ecx, VGA_ROWS
je retLbl
jmp clearScreen.loop

vgaInc:
; ecx, edx: coord
; updates ecx, edx
inc edx
cmp edx, VGA_COLS
je vgaNewline
ret

vgaNewline:
; ecx, edx: coord
; updates ecx, edx
mov edx, 0
inc ecx
ret

writeChar:
; bl: character to write
; ecx, edx: coord
; clobbers eax and bh
mov bh, 0x0F
mov eax, VGA_COLS*2
push edx
mul ecx
pop edx
add eax, edx
add eax, edx
add eax, VGA_STRT
mov [eax], bx
ret

retLbl:
ret
