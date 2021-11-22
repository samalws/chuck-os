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

/*
void key_handler_c(char c) {
  write_term(0, 0, c);
}
*/

void kernel_main() {
  clear_term();
  int col = 0;
  int row = 0;
  print_term(&col, &row, "Hello!");
  col = 0;
  ++row;
  print_term(&col, &row, "Welcome to Chuck OS!");
}
