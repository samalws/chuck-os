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
times 4096 db 0
stackTop:

section .data
gdtMsg  db "Loading GDT...", BKSLASHN, 0
welcome db "Welcome to Chuck OS! (now in nasm)", BKSLASHN, 0

vgaRow dd 0
vgaCol dd 0

gdtSpec:
; ENTRY 0
dd 0 ; base
dd 0 ; limit
db 0 ; type
; ENTRY 1
dd 0 ; base
db 0xCF, 0xFF, 0xFF ; limit
db 0x9A ; type
; ENTRY 2
dd 0 ; base
db 0xCF, 0xFF, 0xFF ; limit
db 0x92 ; type

gdt:
times 8*3 db 0
gdtEnd:

gdtr:
dw 0
dd 0

section .text
start:
mov esp, stackTop
; -----
call clearScreenStd
mov eax, gdtMsg
call printStrStd
; -----
call makeGdt
call setGdt
; -----
call kernelMain

hang:
cli
hlt
jmp hang

makeGdt:
mov eax, gdtSpec
mov ebx, gdt
.loop:
call makeGdtN
add eax, 8
add ebx, 8
cmp ebx, gdtEnd
je retLbl
jmp makeGdt.loop

makeGdtN:
; eax: gdt spec entry
; ebx: gdt entry
; clobbers cl
; ------
; first encode base
mov cl, eax[3]
mov ebx[2], cl
mov cl, eax[2]
mov ebx[3], cl
mov cl, eax[1]
mov ebx[4], cl
mov cl, eax[0]
mov ebx[7], cl
; then encode limit
mov cl, eax[6]
mov ebx[0], cl
mov cl, eax[5]
mov ebx[1], cl
mov cl, eax[4]
mov ebx[6], cl
; finally encode type
mov cl, eax[7]
mov ebx[5], cl
; done
ret

setGdt:
; copypasted from osdev wiki, I don't really understand rn
mov eax, 0
mov ax, ds
shl eax, 4
add eax, gdt
mov [gdtr+2], eax
mov eax, gdtEnd
sub eax, gdt
mov [gdtr], ax
lgdt [gdtr]
ret

kernelMain:
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
