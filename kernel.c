#include <stddef.h>
#include <stdint.h>

volatile uint16_t* vga_buffer = (uint16_t*) 0xB8000;
const int VGA_COLS = 80;
const int VGA_ROWS = 25;
uint8_t term_color = 0x0F;

void write_term(int col, int row, char letter) {
  vga_buffer[VGA_COLS*row + col] = ((uint16_t) term_color << 8) | letter;
}

void clear_term() {
  for (int col = 0; col < VGA_COLS; col++)
    for (int row = 0; row < VGA_ROWS; row++)
      write_term(col, row, ' ');
}

void print_term(int* col, int* row, const char* str) {
  while (*str) {
    write_term(*col, *row, *str);
    ++ *col;
    if (*col >= VGA_COLS) {
      ++ *row;
      *col = 0;
    }
    ++ str;
  }
}

typedef struct {
  uint16_t offset_low;
  uint16_t segment;
  uint8_t  reserved;
  uint16_t attrs;
  uint16_t offset_high;
} __attribute__((packed)) idt_entry;

typedef struct {
  uint16_t size;
  uint32_t base;
} __attribute__((packed)) idtr;

idt_entry kernel_idt[256];
idtr kernel_idtr;

__attribute__((noreturn))
void handle_interrupt() { // TODO maybe move into asm file
  write_term(0, 1, 'X');
  __asm__ volatile ("cli; hlt");
  for (;;) {}
}

void make_idt_entry(uint8_t entry_num, void* handler, uint8_t attrs) {
  idt_entry *entry = &kernel_idt[entry_num];
  entry->offset_low  = ((uint32_t) handler) & 0xFFFF;
  entry->offset_high = ((uint32_t) handler) >> 16;
  entry->segment     = 8;
  entry->attrs       = attrs;
  entry->reserved    = 0;
}

void make_idt() {
  kernel_idtr.base = (uint32_t) kernel_idt;
  kernel_idtr.size = 256 * ((uint16_t) sizeof(idt_entry)) - 1;

  for (int i = 0; i < 256; ++i)
    make_idt_entry(i, &handle_interrupt, 0x8E);

  __asm__ volatile ("lidt %0" : : "m"(kernel_idtr)); // what's with the : : ?
  __asm__ volatile ("sti");
}

void kernel_main() {
  clear_term();
  int col = 0;
  int row = 0;
  print_term(&col, &row, "Hello!");
  col = 0;
  ++row;
  print_term(&col, &row, "Welcome to Chuck OS!");

  make_idt();

  for (char c = 0;; c++)
    write_term(0, 0, c);
}
