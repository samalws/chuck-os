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

gdt:
times 8 db 0
db 0xFF, 0xFF, 0, 0, 0, 0x9A, 0xCF
db 0xFF, 0xFF, 0, 0, 0, 0x92, 0xCF
gdtEnd:

gdtr:
dw 0
dd 0

section .text
start:
cli
mov esp, stackTop
; enable A20 line (whatever that is)
call enableA20
; print out "loading GDT"
call clearScreenStd
mov eax, gdtMsg
call printStrStd
; load GDT
call setGdt
; enter protected mode
mov eax, cr0
or al, 1
mov cr0, eax
; jmp 08h:protectedStart ; TODO CAUSES PROBLEM
protectedStart:
call kernelMain

hang:
cli
hlt
jmp hang

enableA20:
; copypasted from osdev.org
in al, 0x92
or al, 2
out 0x92, al
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
