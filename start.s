global start

; code-wide standard:
; when dealing with VGA, ecx is row and edx is col

EAXMAGIC equ 0x2BADB002
MB_MAGIC equ 0x1BADB002
MB_FLAGS equ 3
MB_CKSUM equ -(MB_MAGIC + MB_FLAGS)
VGA_COLS equ 80
VGA_ROWS equ 25
VGA_STRT equ 0xB8000
BKSLASHN equ 0xA
CODE_SEG equ 8
DATA_SEG equ 16

section .multiboot
align 4
dd MB_MAGIC
dd MB_FLAGS
dd MB_CKSUM

section .bss
align 16
stack:
times 4096 db 0
.top:

section .data
welcome db "Welcome to Chuck OS! (now in nasm)", BKSLASHN, 0

vgaRow dd 0
vgaCol dd 0

gdt:
db 0,    0,    0, 0, 0, 0,    0,    0
db 0xFF, 0xFF, 0, 0, 0, 0x9A, 0xCF, 0
db 0xFF, 0xFF, 0, 0, 0, 0x92, 0xCF, 0
.end:

gdtr:
dw gdt.end-gdt-1
dd gdt

section .text
start:
mov esp, stack.top
call checkEax
call loadGdt
call kernelMain

hang:
cli
hlt
jmp hang

checkEax:
cmp eax, EAXMAGIC
jne hang
ret

loadGdt:
lgdt [gdtr]
jmp CODE_SEG:.protectedStart
.protectedStart:
mov ax, DATA_SEG
mov ds, ax
mov es, ax
mov fs, ax
mov gs, ax
mov ss, ax
ret

kernelMain:
call clearScreenStd
mov eax, welcome
call printStrStd
ret

printStrStd:
; eax: string start pos
; uses global vars vgaRow and vgaCol
mov ecx, [vgaRow]
mov edx, [vgaCol]
call printStr
mov [vgaRow], ecx
mov [vgaCol], edx
ret

clearScreenStd:
; uses global vars vgaRow and vgaCol
call clearScreen
mov ecx, 0
mov [vgaRow], ecx
mov [vgaCol], ecx
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
je .newline
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
jmp .loop

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
