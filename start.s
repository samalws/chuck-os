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
CODESEG0 equ 8*1
DATASEG0 equ 8*2
CODESEG3 equ 8*3 + 3
DATASEG3 equ 8*4 + 3

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
isrNonExceptMsg db "Sir an interrupt has triggered! (non except)", BKSLASHN, 0
isrExceptMsg    db "Sir an interrupt has triggered! (except)",     BKSLASHN, 0

vgaRow dd 0
vgaCol dd 0

gdt:
db 0,    0,    0, 0, 0, 0,    0,    0
db 0xFF, 0xFF, 0, 0, 0, 0x9A, 0xCF, 0
db 0xFF, 0xFF, 0, 0, 0, 0x92, 0xCF, 0
db 0xFF, 0xFF, 0, 0, 0, 0xFA, 0xCF, 0
db 0xFF, 0xFF, 0, 0, 0, 0xF2, 0xCF, 0
.end:

gdtr:
dw gdt.end-gdt-1
dd gdt

idt:
times 8*256 db 0
.end:

idtr:
dw idt.end-idt-1
dd idt

section .text
start:
mov esp, stack.top
call checkEax
call loadGdt
call loadIdt
call enterRing3
call kernelMain
.loop:
jmp .loop

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
jmp CODESEG0:.protectedStart
.protectedStart:
mov ax, DATASEG0
mov ds, ax
mov es, ax
mov fs, ax
mov gs, ax
mov ss, ax
ret

loadIdt:
call genIdt
lidt [idtr]
ret

genIdt:
mov ecx, idt
.loop:
cmp ecx, idt+(32*8)
jl .trap
; non-trap:
mov bl, 0
mov edx, isrNonExcept
jmp .afterTrap
.trap:
mov bl, 1
mov edx, isrExcept
.afterTrap:
call genIdtEntry
add ecx, 8
cmp ecx, idt.end
je retLbl
jmp .loop

genIdtEntry:
; ecx: entry address
; bl:  0 if non-exception, 1 if exception
; edx: isr address
; clobbers eax only
; -----
; first load offset
mov eax, edx
mov ecx[0], ax
shr eax, 8*2
mov ecx[6], ax
; then load seg selector
mov ax, CODESEG0
mov ecx[2], ax
; finally load type etc
mov al, 0b11101110
or al, bl
mov ecx[5], al
; ecx[4] is reserved, don't touch it
ret

isrNonExcept:
mov eax, isrNonExceptMsg
call printStrStd
iret

isrExcept:
mov eax, isrExceptMsg
call printStrStd
iret

enterRing3:
mov ax, DATASEG3
mov ds, ax
mov es, ax
mov fs, ax
mov gs, ax
mov eax, esp
push DATASEG3
push eax
pushf
push CODESEG3
push retLbl
iret
jmp hang

kernelMain:
call clearScreenStd
mov eax, welcome
call printStrStd
; int 80
; int 10
; int 0
; mov eax, 0
; div eax
; int 80
ret

printStrStd:
; eax: string start pos
; uses global vars vgaRow and vgaCol
call stdVgaRead
call printStr
call stdVgaWrite
ret

clearScreenStd:
; uses global vars vgaRow and vgaCol
call clearScreen
mov ecx, 0
mov edx, 0
call stdVgaWrite
ret

stdVgaRead:
mov ecx, [vgaRow]
mov edx, [vgaCol]
ret

stdVgaWrite:
mov [vgaRow], ecx
mov [vgaCol], edx
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
